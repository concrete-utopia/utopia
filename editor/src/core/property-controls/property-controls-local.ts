import type {
  ComponentToRegister,
  ComponentInsertOption,
  PropertyControls,
  PreferredChildComponent,
} from 'utopia-api/core'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { packageJsonFileFromProjectContents } from '../../components/assets'
import type {
  ComponentDescriptorWithName,
  ComponentDescriptorsForFile,
  ComponentInfo,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import { parseControlDescription } from './property-controls-parser'
import type { ParseResult } from '../../utils/value-parser-utils'
import {
  objectKeyParser,
  optionalObjectKeyParser,
  parseArray,
  parseBoolean,
  parseObject,
  parseString,
} from '../../utils/value-parser-utils'
import type { ParseOrPrintResult, UtopiaTsWorkers } from '../workers/common/worker-types'
import { getCachedParseResultForUserStrings } from './property-controls-local-parser-bridge'
import type { Either } from '../shared/either'
import { applicative3Either, applicative4Either, mapEither, sequenceEither } from '../shared/either'
import { setOptionalProp } from '../shared/object-utils'
import type { EditorDispatch } from '../../components/editor/action-types'
import { isParseSuccess } from '../shared/project-file-types'
import { resolveParamsAndRunJsCode } from '../shared/javascript-cache'
import * as EditorActions from '../../components/editor/actions/action-creators'
import { mapArrayToDictionary } from '../shared/array-utils'
import { DefaultThirdPartyControlDefinitions } from '../../core/third-party/third-party-controls'

async function parseInsertOption(
  insertOption: ComponentInsertOption,
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentInfo>> {
  const allRequiredImports = `import { ${componentName} } from '${moduleName}'; ${
    insertOption.additionalImports ?? ''
  }`

  const parsedParams = await getCachedParseResultForUserStrings(
    workers,
    allRequiredImports,
    insertOption.code,
  )

  return mapEither(({ importsToAdd, elementToInsert }) => {
    return {
      insertMenuLabel: insertOption.label ?? componentName,
      elementToInsert: () => elementToInsert,
      importsToAdd: importsToAdd,
    }
  }, parsedParams)
}

export const ComponentDescriptorFile = '/utopia/components.utopia.js'

export function maybeUpdateComponentDescriptor(
  parseResult: Array<ParseOrPrintResult>,
  workers: UtopiaTsWorkers,
  dispatch: EditorDispatch,
) {
  // TODO: check file extension here
  // e.g. `.utopia.registration.js`
  const componentsFile = parseResult.find((parse) => parse.filename === ComponentDescriptorFile)
  if (componentsFile == null) {
    return
  }
  if (!isParseSuccess(componentsFile.parseResult)) {
    return
  }
  const combinedTopLevelArbitraryBlock = componentsFile.parseResult.combinedTopLevelArbitraryBlock

  if (combinedTopLevelArbitraryBlock == null) {
    return
  }

  try {
    // TODO: this isn't the right function to call, as `resolveParamsAndRunJsCode` doesn't actually give us access to the exports/default exports
    const evaluatedFile = resolveParamsAndRunJsCode(
      componentsFile.filename,
      combinedTopLevelArbitraryBlock,
      {},
      {},
    )

    // TODO: unhardcode these values
    const parsedComponents = parseComponents(evaluatedFile['Components']['/src/playground'])

    if (parsedComponents.type === 'LEFT') {
      return
    }

    const componentDescriptorPromises = Object.entries(parsedComponents.value).map(
      ([componentName, componentToRegister]) => {
        return componentDescriptorForComponentToRegister(
          componentToRegister,
          componentName,
          '/src/playground', // TODO: unhardcode this value
          workers,
        )
      },
    )

    const componentDescriptorsUnsequenced = Promise.all(componentDescriptorPromises)
    const componentDescriptors = componentDescriptorsUnsequenced.then((unsequenced) =>
      sequenceEither(unsequenced),
    )

    void componentDescriptors.then((result) => {
      if (result.type === 'LEFT') {
        // TODO: error handling
        return
      }

      const updatedPropertyControlsInfo = updateWithComponentDescriptors(
        componentsFile.filename,
        result.value,
      )
      dispatch([EditorActions.updatePropertyControlsInfo(updatedPropertyControlsInfo)])
      return
    })
  } catch (e) {
    console.warn('Error evaluating component descriptor file')
  }
}

const COMPONENTS_FILE_CACHE: { current: { [path: string]: ComponentDescriptorWithName[] } } = {
  current: {},
}

export function deleteComponentRegistrationFromFile(fileName: string): PropertyControlsInfo {
  return updateWithComponentDescriptors(fileName, [])
}

function updateWithComponentDescriptors(
  componentFileName: string,
  info: ComponentDescriptorWithName[],
): PropertyControlsInfo {
  COMPONENTS_FILE_CACHE.current[componentFileName] = info

  // TODO: this might not be ideal for perf, but it's a generic problem at this point
  const allComponentDescriptors = Object.values(COMPONENTS_FILE_CACHE.current).flatMap(
    (descriptors) => descriptors,
  )
  const newDescriptorsForFile: ComponentDescriptorsForFile = mapArrayToDictionary(
    allComponentDescriptors,
    (descriptorWithName) => descriptorWithName.componentName,
    (descriptorWithName) => {
      return {
        properties: descriptorWithName.properties,
        supportsChildren: descriptorWithName.supportsChildren,
        variants: descriptorWithName.variants,
        preferredChildComponents: descriptorWithName.preferredChildComponents ?? [],
      }
    },
  )

  return {
    'src/playground': newDescriptorsForFile,
    ...DefaultThirdPartyControlDefinitions,
  }
}

function variantsForComponentToRegister(
  componentToRegister: ComponentToRegister,
  componentName: string,
): Array<ComponentInsertOption> {
  if (componentToRegister.variants.length > 0) {
    return componentToRegister.variants
  } else {
    // If none provided, fall back to a default insert option
    return [
      {
        label: componentName,
        code: `<${componentName} />`,
      },
    ]
  }
}

export async function componentDescriptorForComponentToRegister(
  componentToRegister: ComponentToRegister,
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentDescriptorWithName>> {
  const insertOptionsToParse = variantsForComponentToRegister(componentToRegister, componentName)

  const parsedInsertOptionPromises = insertOptionsToParse.map((insertOption) =>
    parseInsertOption(insertOption, componentName, moduleName, workers),
  )

  const parsedVariantsUnsequenced = await Promise.all(parsedInsertOptionPromises)
  const parsedVariants = sequenceEither(parsedVariantsUnsequenced)

  return mapEither((variants) => {
    return {
      componentName: componentName,
      supportsChildren: componentToRegister.supportsChildren,
      preferredChildComponents: componentToRegister.preferredChildComponents ?? [],
      properties: componentToRegister.properties,
      variants: variants,
    }
  }, parsedVariants)
}

function fullyParsePropertyControls(value: unknown): ParseResult<PropertyControls> {
  return parseObject(parseControlDescription)(value)
}

function parseComponentInsertOption(value: unknown): ParseResult<ComponentInsertOption> {
  return applicative3Either(
    (code, additionalImports, label) => {
      let insertOption: ComponentInsertOption = {
        code: code,
      }

      setOptionalProp(insertOption, 'additionalImports', additionalImports)
      setOptionalProp(insertOption, 'label', label)

      return insertOption
    },
    objectKeyParser(parseString, 'code')(value),
    optionalObjectKeyParser(parseString, 'additionalImports')(value),
    optionalObjectKeyParser(parseString, 'label')(value),
  )
}

export function parsePreferredChild(value: unknown): ParseResult<PreferredChildComponent> {
  return applicative3Either(
    (name, additionalImports, variants) => ({ name, additionalImports, variants }),
    objectKeyParser(parseString, 'name')(value),
    optionalObjectKeyParser(parseString, 'additionalImports')(value),
    optionalObjectKeyParser(parseArray(parseComponentInsertOption), 'variants')(value),
  )
}

function parseComponentToRegister(value: unknown): ParseResult<ComponentToRegister> {
  return applicative4Either(
    (properties, supportsChildren, variants, preferredChildComponents) => {
      return {
        properties: properties,
        supportsChildren: supportsChildren,
        variants: variants,
        preferredChildComponents: preferredChildComponents,
      }
    },
    objectKeyParser(fullyParsePropertyControls, 'properties')(value),
    objectKeyParser(parseBoolean, 'supportsChildren')(value),
    objectKeyParser(parseArray(parseComponentInsertOption), 'variants')(value),
    optionalObjectKeyParser(parseArray(parsePreferredChild), 'preferredChildComponents')(value),
  )
}

export const parseComponents: (
  value: unknown,
) => ParseResult<{ [componentName: string]: ComponentToRegister }> =
  parseObject(parseComponentToRegister)

export function getThirdPartyControlsIntrinsic(
  elementName: string,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): PropertyControls | null {
  const packageJsonFile = packageJsonFileFromProjectContents(projectContents)
  const dependencies = dependenciesFromPackageJson(packageJsonFile, 'combined')
  const foundPackageWithElement = Object.keys(propertyControlsInfo).find((key) => {
    return (
      propertyControlsInfo[key][elementName] != null &&
      dependencies.some((dependency) => dependency.name === key)
    )
  })
  if (foundPackageWithElement != null) {
    return propertyControlsInfo[foundPackageWithElement]?.[elementName]?.properties
  }
  return null
}
