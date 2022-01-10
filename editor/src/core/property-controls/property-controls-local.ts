import {
  registerModule as registerModuleAPI,
  ComponentToRegister,
  ComponentInsertOption,
  PropertyControls,
} from 'utopia-api/core'
import deepEqual from 'fast-deep-equal'
import { ProjectContentTreeRoot } from '../../components/assets'
import {
  ComponentDescriptorsForFile,
  ComponentDescriptorWithName,
  ComponentInfo,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import type { EditorDispatch } from '../../components/editor/action-types'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import {
  EditorState,
  packageJsonFileFromProjectContents,
} from '../../components/editor/store/editor-state'
import { updatePropertyControlsInfo } from '../../components/editor/actions/action-creators'
import {
  parseControlDescription,
  ParsedPropertyControls,
  parsePropertyControls,
} from './property-controls-parser'
import {
  getParseErrorDetails,
  objectKeyParser,
  optionalObjectKeyParser,
  parseArray,
  parseObject,
  ParseResult,
  parseString,
} from '../../utils/value-parser-utils'
import { UtopiaTsWorkers } from '../workers/common/worker-types'
import { getCachedParseResultForUserStrings } from './property-controls-local-parser-bridge'
import {
  applicative2Either,
  applicative3Either,
  Either,
  forEachRight,
  isLeft,
  isRight,
  mapEither,
  sequenceEither,
} from '../shared/either'
import { mapArrayToDictionary } from '../shared/array-utils'
import { setOptionalProp } from '../shared/object-utils'
import { addRegisteredControls } from '../../components/canvas/canvas-globals'
import { getGlobalEvaluatedFileName } from '../shared/code-exec-utils'

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
      elementToInsert: elementToInsert,
      importsToAdd: importsToAdd,
    }
  }, parsedParams)
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

async function componentDescriptorForComponentToRegister(
  componentToRegister: ComponentToRegister,
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentDescriptorWithName>> {
  const parsedPropertyControls = parsePropertyControls(componentToRegister.properties)
  const unparsedVariants = variantsForComponentToRegister(componentToRegister, componentName)

  const parsedInsertOptionPromises = unparsedVariants.map((insertOption) =>
    parseInsertOption(insertOption, componentName, moduleName, workers),
  )

  const parsedVariantsUnsequenced = await Promise.all(parsedInsertOptionPromises)
  const parsedVariants = sequenceEither(parsedVariantsUnsequenced)

  return mapEither((variants) => {
    return {
      componentName: componentName,
      properties: parsedPropertyControls,
      variants: variants,
    }
  }, parsedVariants)
}

function registerModuleInternal(
  workers: UtopiaTsWorkers,
  moduleNameOrPath: string,
  components: { [componentName: string]: ComponentToRegister },
): void {
  const componentNames = Object.keys(components)
  const componentDescriptorPromises = componentNames.map((componentName) => {
    const componentToRegister = components[componentName]
    return componentDescriptorForComponentToRegister(
      componentToRegister,
      componentName,
      moduleNameOrPath,
      workers,
    )
  })

  const componentDescriptorsUnsequenced = Promise.all(componentDescriptorPromises)
  const componentDescriptors = componentDescriptorsUnsequenced.then((unsequenced) =>
    sequenceEither(unsequenced),
  )
  addRegisteredControls(getGlobalEvaluatedFileName(), moduleNameOrPath, componentDescriptors)
}

export function fullyParsePropertyControls(value: unknown): ParseResult<PropertyControls> {
  return parseObject(parseControlDescription)(value)
}

export function parseComponentInsertOption(value: unknown): ParseResult<ComponentInsertOption> {
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

export function parseComponentToRegister(value: unknown): ParseResult<ComponentToRegister> {
  return applicative2Either(
    (properties, variants) => {
      return {
        properties: properties,
        variants: variants,
      }
    },
    objectKeyParser(fullyParsePropertyControls, 'properties')(value),
    objectKeyParser(parseArray(parseComponentInsertOption), 'variants')(value),
  )
}

export const parseComponents: (
  value: unknown,
) => ParseResult<{ [componentName: string]: ComponentToRegister }> = parseObject(
  parseComponentToRegister,
)

export function createRegisterModuleFunction(
  workers: UtopiaTsWorkers | null,
): typeof registerModuleAPI {
  // create a function with a signature that matches utopia-api/registerModule
  return function registerModule(
    unparsedModuleName: string,
    unparsedComponents: { [componentName: string]: ComponentToRegister },
  ): void {
    const parsedModuleName = parseString(unparsedModuleName)
    const parsedComponents = parseComponents(unparsedComponents)

    const parsedParams = applicative2Either(
      (moduleName, components) => {
        return { moduleName: moduleName, components: components }
      },
      parsedModuleName,
      parsedComponents,
    )

    if (isLeft(parsedModuleName)) {
      const errorDetails = getParseErrorDetails(parsedModuleName.value)
      throw new Error(`registerModule first param (moduleName): ${errorDetails.description}`)
    }

    if (isLeft(parsedComponents)) {
      const errorDetails = getParseErrorDetails(parsedComponents.value)
      throw new Error(
        `registerModule second param (components): ${errorDetails.description} [${errorDetails.path}]`,
      )
    }

    forEachRight(parsedParams, ({ moduleName, components }) => {
      if (workers != null) {
        // Fires off asynchronously.
        registerModuleInternal(workers, moduleName, components)
      }
    })
  }
}

export function getThirdPartyControlsIntrinsic(
  elementName: string,
  propertyControlsInfo: PropertyControlsInfo,
  projectContents: ProjectContentTreeRoot,
): ParseResult<ParsedPropertyControls> | null {
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
