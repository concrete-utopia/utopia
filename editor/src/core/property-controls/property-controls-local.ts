import type {
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
  PropertyControlsInfo,
} from '../../components/custom-code/code-file'
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
import {
  applicative3Either,
  applicative4Either,
  isLeft,
  mapEither,
  right,
  sequenceEither,
} from '../shared/either'
import { setOptionalProp } from '../shared/object-utils'
import { assertNever } from '../shared/utils'
import { isExportDefault, isParseSuccess } from '../shared/project-file-types'
import { resolveParamsAndRunJsCode } from '../shared/javascript-cache'
import type { EditorDispatch } from '../../components/editor/action-types'
import { updatePropertyControlsInfo } from '../../components/editor/actions/action-creators'
import { DefaultThirdPartyControlDefinitions } from '../third-party/third-party-controls'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { packageJsonFileFromProjectContents } from '../../components/assets'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'

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

// TODO: find a better way to detect component descriptor files, e.g. package.json
export const isComponentDescriptorFile = (filename: string) =>
  filename.startsWith('/utopia/') && filename.endsWith('.utopia.js')

async function getComponentDescriptorPromisesFromParseResult(
  parseResult: ParseOrPrintResult,
  workers: UtopiaTsWorkers,
): Promise<ComponentDescriptorWithName[]> {
  if (!isParseSuccess(parseResult.parseResult)) {
    return []
  }
  const exportDefaultIdentifier = parseResult.parseResult.exportsDetail.find(isExportDefault)
  if (exportDefaultIdentifier?.name == null) {
    // TODO: error handling
    console.warn('No export default in descriptor file')
    return []
  }

  const combinedTopLevelArbitraryBlock = parseResult.parseResult.combinedTopLevelArbitraryBlock

  if (combinedTopLevelArbitraryBlock == null) {
    // TODO: error handling
    return []
  }

  try {
    // TODO: provide execution scope, so imports can be resolved
    const evaluatedFile = resolveParamsAndRunJsCode(
      parseResult.filename,
      combinedTopLevelArbitraryBlock,
      {},
      {},
    )

    const descriptors = evaluatedFile[exportDefaultIdentifier.name]

    if (descriptors == null) {
      // TODO: error handling
      console.warn('Could not find component descriptors in the descriptor file')
      return []
    }

    let result: ComponentDescriptorWithName[] = []

    for await (const [moduleName, descriptor] of Object.entries(descriptors)) {
      const parsedComponents = parseComponents(descriptor)

      if (parsedComponents.type === 'LEFT') {
        continue
      }

      for await (const [componentName, componentToRegister] of Object.entries(
        parsedComponents.value,
      )) {
        const componentDescriptor = await componentDescriptorForComponentToRegister(
          componentToRegister,
          componentName,
          moduleName,
          workers,
        )

        if (componentDescriptor.type === 'RIGHT') {
          result = result.concat(componentDescriptor.value)
        }
      }
    }
    return result
  } catch {
    // TODO: error handling
    console.warn('Error evaluating component descriptor file')
    return []
  }
}

export async function maybeUpdatePropertyControls(
  parseResults: Array<ParseOrPrintResult>,
  workers: UtopiaTsWorkers,
  dispatch: EditorDispatch,
) {
  let componentDescriptorUpdates: ComponentDescriptorFileLookup = {}

  const componentDescriptorParseResults = parseResults.filter((p) =>
    isComponentDescriptorFile(p.filename),
  )
  if (componentDescriptorParseResults.length === 0) {
    // there is nothing to update, return early so no empty dispatch is made
    return
  }

  for await (const file of componentDescriptorParseResults) {
    const descriptors = await getComponentDescriptorPromisesFromParseResult(file, workers)
    if (descriptors.length > 0) {
      componentDescriptorUpdates[file.filename] = descriptors
    }
  }

  const updatedPropertyControlsInfo = updatePropertyControlsOnDescriptorFileUpdate(
    componentDescriptorUpdates,
  )
  dispatch([updatePropertyControlsInfo(updatedPropertyControlsInfo)])
}

interface ComponentDescriptorFileLookup {
  [path: string]: ComponentDescriptorWithName[]
}

const COMPONENTS_DESCRIPTOR_FILE_CACHE: { current: ComponentDescriptorFileLookup } = {
  current: {},
}

export function updatePropertyControlsOnDescriptorFileDelete(
  componentDescriptorFile: string,
): PropertyControlsInfo {
  COMPONENTS_DESCRIPTOR_FILE_CACHE.current[componentDescriptorFile] = []
  return getPropertyControlsFromComponentDescriptors(COMPONENTS_DESCRIPTOR_FILE_CACHE.current)
}

export function updatePropertyControlsOnDescriptorFileUpdate(
  componentDescriptorUpdates: ComponentDescriptorFileLookup,
): PropertyControlsInfo {
  Object.entries(componentDescriptorUpdates).forEach(([filename, componentDescriptors]) => {
    COMPONENTS_DESCRIPTOR_FILE_CACHE.current[filename] = componentDescriptors
  })
  return getPropertyControlsFromComponentDescriptors(COMPONENTS_DESCRIPTOR_FILE_CACHE.current)
}

function getPropertyControlsFromComponentDescriptors(
  componentDescriptorFiles: ComponentDescriptorFileLookup,
): PropertyControlsInfo {
  // TODO: this might not be ideal for perf, but it's a generic problem at this point
  const allComponentDescriptors = Object.values(componentDescriptorFiles).flatMap(
    (descriptors) => descriptors,
  )

  // Adding the default third party controls to the property controls here, but maybe we should do this in the UpdatePropertyControlsInfo action
  let propertyControls: PropertyControlsInfo = {
    ...DefaultThirdPartyControlDefinitions,
  }

  for (const propertyControlsForComponent of allComponentDescriptors) {
    if (propertyControls[propertyControlsForComponent.moduleName] == null) {
      propertyControls[propertyControlsForComponent.moduleName] = {}
    }

    propertyControls[propertyControlsForComponent.moduleName] = {
      ...propertyControls[propertyControlsForComponent.moduleName],
      [propertyControlsForComponent.componentName]: {
        properties: propertyControlsForComponent.properties,
        supportsChildren: propertyControlsForComponent.supportsChildren,
        variants: propertyControlsForComponent.variants,
        preferredChildComponents: propertyControlsForComponent.preferredChildComponents ?? [],
      },
    }
  }

  return propertyControls
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
  const allRequiredImports = `import { ${componentName} } from '${preferredChild.additionalImports}';`

  const parsedParams = await getCachedParseResultForUserStrings(
    workers,
    allRequiredImports,
    '<placeholder />',
  )
  if (isLeft(parsedParams)) {
    return parsedParams
  }

  const variantsPromise: Array<Promise<Either<string, ComponentInfo>>> =
    preferredChild.variants == null
      ? []
      : preferredChild.variants.map((insertOption) =>
          parseInsertOption(
            insertOption,
            componentName,
            preferredChild.additionalImports ?? moduleName,
            workers,
          ),
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
      const parsedJSXControlDescription = parseJSXControlDescription(descriptor, context)
      return parsedJSXControlDescription
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
      moduleName: moduleName,
    }
  }, parsedVariants)
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
