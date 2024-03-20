import type React from 'react'
import type {
  registerModule as registerModuleAPI,
  registerInternalComponent as registerInternalComponentAPI,
  registerExternalComponent as registerExternalComponentAPI,
  RegularControlDescription as RegularControlDescriptionFromUtopia,
  JSXControlDescription as JSXControlDescriptionFromUtopia,
  PropertyControls as PropertyControlsFromUtopiaAPI,
  ComponentToRegister,
  ComponentInsertOption,
  PreferredChildComponent,
} from 'utopia-api/core'
import { isBaseControlDescription } from 'utopia-api/core'
import type {
  ObjectControlDescription,
  PropertyControls,
  RegularControlDescription,
  JSXControlDescription,
  PreferredChildComponentDescriptor,
} from '../../components/custom-code/internal-property-controls'
import type {
  ComponentDescriptorWithName,
  ComponentInfo,
} from '../../components/custom-code/code-file'
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
  applicative3Either,
  applicative4Either,
  bimapEither,
  foldEither,
  isLeft,
  mapEither,
  right,
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
import { assertNever } from '../shared/utils'

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

async function makePreferredChildDescriptor(
  preferredChild: PreferredChildComponent,
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, PreferredChildComponentDescriptor>> {
  const allRequiredImports = `import { ${componentName} } from '${moduleName}'; ${
    preferredChild.additionalImports ?? ''
  }`

  const parsedParams = await getCachedParseResultForUserStrings(workers, allRequiredImports, '</>')
  if (isLeft(parsedParams)) {
    return parsedParams
  }

  const variantsPromise: Array<Promise<Either<string, ComponentInfo>>> =
    preferredChild.variants == null
      ? []
      : preferredChild.variants.map((insertOption) =>
          parseInsertOption(insertOption, componentName, moduleName, workers),
        )

  const variants = sequenceEither(await Promise.all(variantsPromise))
  if (isLeft(variants)) {
    return variants
  }

  return right({
    name: preferredChild.name,
    imports: parsedParams.value.importsToAdd,
    variants: variants.value,
  })
}

type PropertyDescriptorResult<T> = Either<string, T>

async function parseJSXControlDescription(
  descriptor: JSXControlDescriptionFromUtopia,
  context: { moduleName: string; workers: UtopiaTsWorkers },
): Promise<PropertyDescriptorResult<JSXControlDescription>> {
  if (descriptor.preferredChildComponents == null) {
    return right({
      ...descriptor,
      preferredChildComponents: undefined,
    })
  }
  const parsedPreferredChildComponents = sequenceEither(
    await Promise.all(
      descriptor.preferredChildComponents.map((preferredChildDescriptor) =>
        makePreferredChildDescriptor(
          preferredChildDescriptor,
          preferredChildDescriptor.name,
          context.moduleName,
          context.workers,
        ),
      ),
    ),
  )

  return mapEither(
    (preferredChildComponents) => ({
      ...descriptor,
      preferredChildComponents,
    }),
    parsedPreferredChildComponents,
  )
}

async function makeRegularControlDescription(
  descriptor: RegularControlDescriptionFromUtopia,
  context: { moduleName: string; workers: UtopiaTsWorkers },
): Promise<PropertyDescriptorResult<RegularControlDescription>> {
  if (isBaseControlDescription(descriptor)) {
    if (descriptor.control === 'jsx') {
      return parseJSXControlDescription(descriptor, context)
    }
    return right(descriptor)
  }
  switch (descriptor.control) {
    case 'array':
      const parsedArrayPropertyControl = await makeRegularControlDescription(
        descriptor.propertyControl,
        context,
      )
      return mapEither(
        (propertyControl) => ({ ...descriptor, propertyControl }),
        parsedArrayPropertyControl,
      )
    case 'object':
      const parsedObject: ObjectControlDescription['object'] = {}
      for await (const [key, value] of Object.entries(descriptor.object)) {
        const parsedValue = await makeRegularControlDescription(value, context)
        if (isLeft(parsedValue)) {
          return parsedValue
        }
        parsedObject[key] = parsedValue.value
      }
      return right({ ...descriptor, object: parsedObject })
    case 'tuple':
      const parsedTuplePropertyControls = sequenceEither(
        await Promise.all(
          descriptor.propertyControls.map((tuplePropertyControl) =>
            makeRegularControlDescription(tuplePropertyControl, context),
          ),
        ),
      )

      return mapEither(
        (propertyControls) => ({ ...descriptor, propertyControls }),
        parsedTuplePropertyControls,
      )

    case 'union':
      const parsedUnionPropertyControls = sequenceEither(
        await Promise.all(
          descriptor.controls.map((tuplePropertyControl) =>
            makeRegularControlDescription(tuplePropertyControl, context),
          ),
        ),
      )

      return mapEither((controls) => ({ ...descriptor, controls }), parsedUnionPropertyControls)
    default:
      assertNever(descriptor)
  }
}

async function makePropertyDescriptors(
  properties: PropertyControlsFromUtopiaAPI,
  context: { moduleName: string; workers: UtopiaTsWorkers },
): Promise<PropertyDescriptorResult<PropertyControls>> {
  let result: PropertyControls = {}

  for await (const [propertyName, descriptor] of Object.entries(properties)) {
    if (descriptor.control === 'folder') {
      const parsedControlsInFolder = await makePropertyDescriptors(descriptor.controls, context)
      if (isLeft(parsedControlsInFolder)) {
        return parsedControlsInFolder
      }
      result['propertyName'] = { ...descriptor, controls: parsedControlsInFolder.value }
    } else {
      const parsedRegularControl = await makeRegularControlDescription(descriptor, context)
      if (isLeft(parsedRegularControl)) {
        return parsedRegularControl
      }
      result[propertyName] = parsedRegularControl.value
    }
  }

  return right(result)
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
  const parsePreferredChildrenPromises =
    componentToRegister.preferredChildComponents == null
      ? []
      : componentToRegister.preferredChildComponents.map((c) =>
          makePreferredChildDescriptor(c, c.name, moduleName, workers),
        )

  const parsedPreferredChildren = sequenceEither(await Promise.all(parsePreferredChildrenPromises))
  if (isLeft(parsedPreferredChildren)) {
    return parsedPreferredChildren
  }

  const properties = await makePropertyDescriptors(componentToRegister.properties, {
    moduleName,
    workers,
  })

  if (isLeft(properties)) {
    return properties
  }

  return mapEither((variants) => {
    return {
      componentName: componentName,
      supportsChildren: componentToRegister.supportsChildren,
      preferredChildComponents: parsedPreferredChildren.value,
      properties: properties.value,
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

function fullyParsePropertyControls(value: unknown): ParseResult<PropertyControlsFromUtopiaAPI> {
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
  registerInternalComponent: typeof registerInternalComponentAPI
  registerExternalComponent: typeof registerExternalComponentAPI
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

  function registerInternalComponent(
    component: React.FunctionComponent,
    properties: ComponentToRegister,
  ): void {
    // when the recieved component is an internal component, it is wrapped into a ComponentRendererComponent
    if (!isComponentRendererComponent(component)) {
      console.warn(
        'registerIntenalComponent failed: component is not internal but imported from external source ',
        component.displayName,
      )
      return
    }
    const name = component.originalName
    const module = dropFileExtension(component.filePath)

    if (name == null) {
      console.warn(
        `registerIntenalComponent failed: ComponentRendererComponent of internal component doesn't have originalName`,
      )
      return
    }

    registerModule(module, { [name]: properties })
  }

  function registerExternalComponent(
    componentName: string,
    packageName: string,
    properties: ComponentToRegister,
  ): void {
    registerModule(packageName, { [componentName]: properties })
  }

  return {
    registerModule,
    registerInternalComponent,
    registerExternalComponent,
  }
}
