import type React from 'react'
import type {
  registerModule as registerModuleAPI,
  registerComponent as registerComponentAPI,
  ComponentToRegister,
  ComponentInsertOption,
  PropertyControls,
} from 'utopia-api/core'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { packageJsonFileFromProjectContents } from '../../components/assets'
import type {
  ComponentDescriptorWithName,
  ComponentInfo,
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import { parseControlDescription } from './property-controls-parser'
import type { ParseResult } from '../../utils/value-parser-utils'
import {
  getParseErrorDetails,
  objectKeyParser,
  optionalObjectKeyParser,
  parseArray,
  parseBoolean,
  parseObject,
  parseString,
} from '../../utils/value-parser-utils'
import type { UtopiaTsWorkers } from '../workers/common/worker-types'
import { getCachedParseResultForUserStrings } from './property-controls-local-parser-bridge'
import type { Either } from '../shared/either'
import {
  applicative2Either,
  applicative3Either,
  bimapEither,
  foldEither,
  mapEither,
  sequenceEither,
} from '../shared/either'
import { setOptionalProp } from '../shared/object-utils'
import type { ControlsToCheck } from '../../components/canvas/canvas-globals'
import { addRegisteredControls } from '../../components/canvas/canvas-globals'
import { getGlobalEvaluatedFileName } from '../shared/code-exec-utils'
import { memoize } from '../shared/memoize'
import fastDeepEqual from 'fast-deep-equal'
import { TimedCacheMap } from '../shared/timed-cache-map'
import { dropFileExtension } from '../shared/file-utils'
import { isComponentRendererComponent } from '../../components/canvas/ui-jsx-canvas-renderer/component-renderer-component'

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
      properties: componentToRegister.properties,
      variants: variants,
    }
  }, parsedVariants)
}

interface PreparedComponentDescriptorsForRegistering {
  sourceFile: string
  moduleNameOrPath: string
  componentDescriptors: ControlsToCheck
}

function prepareComponentDescriptorsForRegistering(
  workers: UtopiaTsWorkers,
  moduleNameOrPath: string,
  components: { [componentName: string]: ComponentToRegister },
): PreparedComponentDescriptorsForRegistering {
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

  return {
    sourceFile: getGlobalEvaluatedFileName(),
    moduleNameOrPath: moduleNameOrPath,
    componentDescriptors: componentDescriptors,
  }
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

function parseComponentToRegister(value: unknown): ParseResult<ComponentToRegister> {
  return applicative3Either(
    (properties, supportsChildren, variants) => {
      return {
        properties: properties,
        supportsChildren: supportsChildren,
        variants: variants,
      }
    },
    objectKeyParser(fullyParsePropertyControls, 'properties')(value),
    objectKeyParser(parseBoolean, 'supportsChildren')(value),
    objectKeyParser(parseArray(parseComponentInsertOption), 'variants')(value),
  )
}

const parseComponents: (
  value: unknown,
) => ParseResult<{ [componentName: string]: ComponentToRegister }> =
  parseObject(parseComponentToRegister)

function parseAndPrepareComponents(
  workers: UtopiaTsWorkers,
  moduleNameOrPath: string,
  unparsedComponents: unknown,
): Either<string, PreparedComponentDescriptorsForRegistering> {
  const parsedComponents = parseComponents(unparsedComponents)

  return bimapEither(
    (parseError) => {
      const errorDetails = getParseErrorDetails(parseError)
      return `registerModule second param (components): ${errorDetails.description} [${errorDetails.path}]`
    },
    (components: { [componentName: string]: ComponentToRegister }) => {
      return prepareComponentDescriptorsForRegistering(workers, moduleNameOrPath, components)
    },
    parsedComponents,
  )
}

type PartiallyAppliedParseAndPrepareComponents = (
  unparsedComponents: unknown,
) => Either<string, PreparedComponentDescriptorsForRegistering>

const partiallyParseAndPrepareComponents = (
  workers: UtopiaTsWorkers,
  moduleNameOrPath: string,
): PartiallyAppliedParseAndPrepareComponents => {
  return (unparsedComponents: unknown) =>
    parseAndPrepareComponents(workers, moduleNameOrPath, unparsedComponents)
}

export function createRegisterModuleAndComponentFunction(workers: UtopiaTsWorkers | null): {
  registerModule: typeof registerModuleAPI
  registerComponent: typeof registerComponentAPI
} {
  let cachedParseAndPrepareComponentsMap = new TimedCacheMap<
    string,
    PartiallyAppliedParseAndPrepareComponents
  >()

  function registerModule(
    unparsedModuleName: string,
    unparsedComponents: { [componentName: string]: ComponentToRegister },
  ): void {
    const parsedModuleName = parseString(unparsedModuleName)

    foldEither(
      (parseFailure) => {
        const errorDetails = getParseErrorDetails(parseFailure)
        throw new Error(`registerModule first param (moduleName): ${errorDetails.description}`)
      },
      (moduleName) => {
        if (workers != null) {
          let parseAndPrepareComponentsFn = cachedParseAndPrepareComponentsMap.get(moduleName)
          if (parseAndPrepareComponentsFn == null) {
            // Create a memoized function for the handling of component descriptors for the specified module name
            parseAndPrepareComponentsFn = memoize(
              partiallyParseAndPrepareComponents(workers, moduleName),
              {
                matchesArg: fastDeepEqual,
                maxSize: 5,
              },
            )
            cachedParseAndPrepareComponentsMap.set(moduleName, parseAndPrepareComponentsFn)
          }

          const parsedPreparedDescriptors = parseAndPrepareComponentsFn(unparsedComponents)
          foldEither(
            (parseFailureErrorMessage) => {
              throw new Error(parseFailureErrorMessage)
            },
            (preparedDescriptors) => {
              // Fires off asynchronously.
              addRegisteredControls(
                preparedDescriptors.sourceFile,
                preparedDescriptors.moduleNameOrPath,
                preparedDescriptors.componentDescriptors,
              )
            },
            parsedPreparedDescriptors,
          )
        }
      },
      parsedModuleName,
    )
  }

  function registerComponent(
    component: React.FunctionComponent,
    properties: ComponentToRegister,
    moduleName?: string,
  ): void {
    // when the recieved component is an internal component, it is wrapped into a ComponentRendererComponent
    if (isComponentRendererComponent(component)) {
      const name = component.originalName
      const module = dropFileExtension(component.filePath)

      if (name == null) {
        console.warn(
          `registerComponent failed: ComponentRendererComponent of internal component doesn't have originalName`,
        )
        return
      }

      registerModule(module, { [name]: properties })
    }

    // when the received component is not a ComponentRendererComponent, it is an external component, and a moduleName is required
    if (moduleName == null) {
      console.warn(
        'registerComponent failed: external components require a moduleName ',
        component.displayName,
      )
      return
    }
    const name = component.displayName ?? component.name
    registerModule(moduleName, { [name]: properties })
  }

  return {
    registerModule,
    registerComponent,
  }
}

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
