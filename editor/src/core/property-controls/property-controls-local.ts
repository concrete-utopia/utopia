import * as BabelParser from '@babel/parser'
import traverse from '@babel/traverse'

import type {
  RegularControlDescription as RegularControlDescriptionFromUtopia,
  JSXControlDescription as JSXControlDescriptionFromUtopia,
  PropertyControls as PropertyControlsFromUtopiaAPI,
  RadioControlDescription as RadioControlDescriptionFromUtopia,
  AllowedEnumType as AllowedEnumTypeFromUtopia,
  BasicControlOptionWithIcon as BasicControlOptionWithIconFromUtopia,
  ComponentToRegister,
  ComponentInsertOption,
  ComponentExample,
  ChildrenSpec,
  Children,
  PreferredContents,
  SectionSpec,
  Display,
  InspectorSpec,
} from 'utopia-api/core'
import {
  DisplayOptions,
  EmphasisOptions,
  FocusOptions,
  IconOptions,
  StylingOptions,
  isBaseControlDescription,
} from 'utopia-api/core'
import type {
  ObjectControlDescription,
  PropertyControls,
  RegularControlDescription,
  JSXControlDescription,
  PreferredChildComponentDescriptor,
  RadioControlDescription,
  RadioControlOption,
} from '../../components/custom-code/internal-property-controls'
import { packageJsonFileFromProjectContents, walkContentsTree } from '../../components/assets'
import {
  ComponentDescriptorDefaults,
  componentDescriptorFromDescriptorFile,
  isDefaultComponentDescriptor,
} from '../../components/custom-code/code-file'
import type {
  ComponentDescriptorSource,
  ComponentDescriptorWithName,
  ComponentInfo,
  PropertyControlsInfo,
  TypedInpsectorSpec,
} from '../../components/custom-code/code-file'
import { dependenciesFromPackageJson } from '../../components/editor/npm-dependency/npm-dependency'
import { parseControlDescription } from './property-controls-parser'
import type { ParseError, ParseResult } from '../../utils/value-parser-utils'
import {
  getParseErrorDetails,
  objectKeyParser,
  objectParser,
  optionalObjectKeyParser,
  optionalProp,
  parseAlternative,
  parseAny,
  parseArray,
  parseConstant,
  parseEnum,
  parseObject,
  parseString,
} from '../../utils/value-parser-utils'
import type { UtopiaTsWorkers } from '../workers/common/worker-types'
import { getCachedParseResultForUserStrings } from './property-controls-local-parser-bridge'
import type { Either } from '../shared/either'
import {
  defaultEither,
  foldEither,
  forEachRight,
  isLeft,
  isRight,
  left,
  mapEither,
  right,
  sequenceEither,
} from '../shared/either'
import { assertNever } from '../shared/utils'
import type {
  Imports,
  ParsedTextFile,
  TextFile,
  TextFileContents,
} from '../shared/project-file-types'
import {
  importAlias,
  importDetails,
  isExportDefault,
  isTextFile,
} from '../shared/project-file-types'
import type { UiJsxCanvasContextData } from '../../components/canvas/ui-jsx-canvas'
import type { EditorState } from '../../components/editor/store/editor-state'
import type { MutableUtopiaCtxRefData } from '../../components/canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import {
  isComponentRendererComponent,
  type ComponentRendererComponent,
} from '../../components/canvas/ui-jsx-canvas-renderer/component-renderer-component'
import type { MapLike } from 'typescript'
import { attemptToResolveParsedComponents } from '../../components/canvas/ui-jsx-canvas'
import { NO_OP } from '../shared/utils'
import { createExecutionScope } from '../../components/canvas/ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import type { EditorDispatch } from '../../components/editor/action-types'
import {
  setCodeEditorComponentDescriptorErrors,
  updatePropertyControlsInfo,
} from '../../components/editor/actions/action-creators'
import type { ProjectContentTreeRoot } from '../../components/assets'
import type { JSXElementChildWithoutUID } from '../shared/element-template'
import {
  getJSXElementNameLastPart,
  jsxAttributesFromMap,
  jsxElement,
  jsxElementNameFromString,
  jsxTextBlock,
} from '../shared/element-template'
import type { ErrorMessage, ErrorMessageSeverity } from '../shared/error-messages'
import { errorMessage } from '../shared/error-messages'
import { dropFileExtension } from '../shared/file-utils'
import type { FancyError } from '../shared/code-exec-utils'
import type { ScriptLine } from '../../third-party/react-error-overlay/utils/stack-frame'
import { intrinsicHTMLElementNamesAsStrings } from '../shared/dom-utils'
import { valueOrArrayToArray } from '../shared/array-utils'
import { optionalMap } from '../shared/optional-utils'
import type { BoundsInFile } from 'utopia-vscode-common'
import { boundsInFile } from 'utopia-vscode-common'

const exportedNameSymbol = Symbol('__utopia__exportedName')
const moduleNameSymbol = Symbol('__utopia__moduleName')

export interface RequireInfo {
  name: string
  moduleName: string
}

export function getRequireInfoFromComponent(component: any): RequireInfo {
  return {
    name: component[exportedNameSymbol],
    moduleName: component[moduleNameSymbol],
  }
}

export function setRequireInfoOnComponent(exported: any, name: string, moduleName: string): void {
  exported[exportedNameSymbol] = name
  exported[moduleNameSymbol] = moduleName
}

function extendExportsWithInfo(exports: any, toImport: string): any {
  Object.entries(exports).forEach(([name, exp]) => {
    if (typeof exp === 'object' || typeof exp === 'function') {
      try {
        ;(exp as any)[exportedNameSymbol] = name
        ;(exp as any)[moduleNameSymbol] = toImport
        // eslint-disable-next-line no-empty
      } catch (e) {}
    }
  })
  return exports
}

export type ModuleEvaluator = (moduleName: string) => any
export function createModuleEvaluator(editor: EditorState): ModuleEvaluator {
  return (moduleName: string) => {
    let mutableContextRef: { current: MutableUtopiaCtxRefData } = { current: {} }
    let topLevelComponentRendererComponents: {
      current: MapLike<MapLike<ComponentRendererComponent>>
    } = { current: {} }
    const emptyMetadataContext: UiJsxCanvasContextData = {
      current: { spyValues: { allElementProps: {}, metadata: {}, variablesInScope: {} } },
    }

    let resolvedFiles: MapLike<MapLike<any>> = {}
    let resolvedFileNames: Array<string> = [moduleName]

    const requireFn = editor.codeResultCache.curriedRequireFn(editor.projectContents)
    const resolve = editor.codeResultCache.curriedResolveFn(editor.projectContents)

    const customRequire = (importOrigin: string, toImport: string) => {
      if (resolvedFiles[importOrigin] == null) {
        resolvedFiles[importOrigin] = []
      }
      let resolvedFromThisOrigin = resolvedFiles[importOrigin]

      const alreadyResolved = resolvedFromThisOrigin[toImport] !== undefined
      const filePathResolveResult = alreadyResolved
        ? left<string, string>('Already resolved')
        : resolve(importOrigin, toImport)

      forEachRight(filePathResolveResult, (filepath) => resolvedFileNames.push(filepath))

      const resolvedParseSuccess: Either<string, MapLike<any>> = attemptToResolveParsedComponents(
        resolvedFromThisOrigin,
        toImport,
        editor.projectContents,
        customRequire,
        mutableContextRef,
        topLevelComponentRendererComponents,
        moduleName,
        editor.canvas.base64Blobs,
        editor.hiddenInstances,
        editor.displayNoneInstances,
        emptyMetadataContext,
        NO_OP,
        false,
        filePathResolveResult,
        null,
      )
      const result = foldEither(
        () => {
          // We did not find a ParseSuccess, fallback to standard require Fn
          return requireFn(importOrigin, toImport, false)
        },
        (scope) => {
          // Return an artificial exports object that contains our ComponentRendererComponents
          return scope
        },
        resolvedParseSuccess,
      )
      const absoluteFilenameOrPackage = defaultEither(toImport, filePathResolveResult)
      return extendExportsWithInfo(result, absoluteFilenameOrPackage)
    }
    return createExecutionScope(
      moduleName,
      customRequire,
      mutableContextRef,
      topLevelComponentRendererComponents,
      editor.projectContents,
      moduleName,
      editor.canvas.base64Blobs,
      editor.hiddenInstances,
      editor.displayNoneInstances,
      emptyMetadataContext,
      NO_OP,
      false,
      null,
    ).scope
  }
}

// TODO: find a better way to detect component descriptor files, e.g. package.json
export const isComponentDescriptorFile = (filename: string) =>
  filename.startsWith('/utopia/') && filename.endsWith('.utopia.js')

type ComponentRegistrationValidationError =
  | { type: 'component-undefined'; registrationKey: string }
  | { type: 'component-name-does-not-match'; componentName: string; registrationKey: string }
  | { type: 'module-name-does-not-match'; moduleName: string; moduleKey: string }

function messageForComponentRegistrationValidationError(
  error: ComponentRegistrationValidationError,
): string {
  switch (error.type) {
    case 'component-name-does-not-match':
      return `Component name (${error.componentName}) does not match the registration key (${error.registrationKey})`
    case 'module-name-does-not-match':
      return `Module name (${error.moduleName}) does not match the module key (${error.moduleKey})`
    case 'component-undefined':
      return `Component registered for key '${error.registrationKey}' is undefined`
    default:
      assertNever(error)
  }
}

type ComponentRegistrationValidationResult =
  | { type: 'valid' }
  | ComponentRegistrationValidationError

type ComponentDescriptorRegistrationError =
  | { type: 'file-unparsed' }
  | { type: 'file-parse-failure'; parseErrorMessages: ErrorMessage[] }
  | { type: 'no-export-default' }
  | { type: 'no-exported-component-descriptors' }
  | { type: 'evaluation-error'; evaluationError: unknown }
  | { type: 'invalid-schema'; invalidSchemaError: ParseError }
  | { type: 'cannot-extract-component'; componentExtractionError: string }
  | {
      type: 'registration-validation-failed'
      validationError: ComponentRegistrationValidationError
      severity: ErrorMessageSeverity
    }

interface ComponentDescriptorRegistrationResult {
  descriptors: ComponentDescriptorWithName[]
  errors: ComponentDescriptorRegistrationError[]
}

function isComponentRegistrationValid(
  registrationKey: string,
  moduleKey: string,
  registration: ComponentToRegister,
): ComponentRegistrationValidationResult {
  const { component } = registration

  if (typeof component === 'undefined') {
    return { type: 'component-undefined', registrationKey: registrationKey }
  }

  // check validity of internal component
  if (isComponentRendererComponent(component)) {
    // TODO: we only validate the last part of the name
    const nameLastPart = optionalMap(
      (name) => getJSXElementNameLastPart(jsxElementNameFromString(name)),
      component.originalName,
    )
    const registrationKeyLastPart = getJSXElementNameLastPart(
      jsxElementNameFromString(registrationKey),
    )
    if (nameLastPart !== registrationKeyLastPart) {
      return {
        type: 'component-name-does-not-match',
        registrationKey: registrationKey,
        componentName: component.originalName ?? 'null',
      }
    }
    const moduleName = dropFileExtension(component.filePath)
    if (moduleName !== moduleKey) {
      return {
        type: 'module-name-does-not-match',
        moduleKey: moduleKey,
        moduleName: moduleName,
      }
    }
    return { type: 'valid' }
  }

  // check validity of external component
  const { name, moduleName } = getRequireInfoFromComponent(component)
  if (name != null) {
    // TODO: this doesn't work yet for components which are not directly imported, e.g. Typography.Text (where Typography is the imported object)
    // The code is here to check the last part of the name, but since we don't require the component itself in these cases, the name and the moduleName will not be available.
    const nameLastPart = getJSXElementNameLastPart(jsxElementNameFromString(name))
    const registrationKeyLastPart = getJSXElementNameLastPart(
      jsxElementNameFromString(registrationKey),
    )
    if (nameLastPart !== registrationKeyLastPart) {
      return {
        type: 'component-name-does-not-match',
        registrationKey: registrationKey,
        componentName: name,
      }
    }
  }
  if (moduleName != null && moduleName !== moduleKey) {
    return {
      type: 'module-name-does-not-match',
      moduleKey: moduleKey,
      moduleName: moduleName,
    }
  }

  return { type: 'valid' }
}

async function getComponentDescriptorPromisesFromParseResult(
  descriptorFile: TextFileContentsWithPath,
  workers: UtopiaTsWorkers,
  evaluator: ModuleEvaluator,
): Promise<ComponentDescriptorRegistrationResult> {
  if (descriptorFile.file.parsed.type === 'UNPARSED') {
    return { descriptors: [], errors: [{ type: 'file-unparsed' }] }
  }

  if (descriptorFile.file.parsed.type === 'PARSE_FAILURE') {
    return {
      descriptors: [],
      errors: [
        {
          type: 'file-parse-failure',
          parseErrorMessages: descriptorFile.file.parsed.errorMessages,
        },
      ],
    }
  }

  const exportDefaultIdentifier = descriptorFile.file.parsed.exportsDetail.find(isExportDefault)
  if (exportDefaultIdentifier?.name == null) {
    return { descriptors: [], errors: [{ type: 'no-export-default' }] }
  }
  const componentBounds = generateComponentBounds(descriptorFile)

  try {
    const evaluatedFile = evaluator(descriptorFile.path)

    const descriptors = evaluatedFile[exportDefaultIdentifier.name]

    if (descriptors == null) {
      return { descriptors: [], errors: [{ type: 'no-exported-component-descriptors' }] }
    }

    let result: ComponentDescriptorWithName[] = []
    let errors: ComponentDescriptorRegistrationError[] = []

    for await (const [moduleName, descriptor] of Object.entries(descriptors)) {
      const parsedComponents = parseComponents(descriptor)

      if (parsedComponents.type === 'LEFT') {
        errors.push({ type: 'invalid-schema', invalidSchemaError: parsedComponents.value })
        continue
      }

      for await (const [componentName, componentToRegister] of Object.entries(
        parsedComponents.value,
      )) {
        const validationResult = isComponentRegistrationValid(
          componentName,
          moduleName,
          componentToRegister,
        )
        if (validationResult.type !== 'valid') {
          const severity = validationResult.type === 'component-undefined' ? 'fatal' : 'warning'
          errors.push({
            type: 'registration-validation-failed',
            validationError: validationResult,
            severity: severity,
          })
          if (severity === 'fatal') {
            continue
          }
        }
        const componentDescriptor = await componentDescriptorForComponentToRegister(
          componentToRegister,
          componentName,
          moduleName,
          workers,
          componentDescriptorFromDescriptorFile(
            descriptorFile.path,
            componentBounds[moduleName][componentName]?.startLine ?? null,
          ),
        )

        switch (componentDescriptor.type) {
          case 'LEFT':
            errors.push({
              type: 'cannot-extract-component',
              componentExtractionError: componentDescriptor.value,
            })
            break
          case 'RIGHT':
            result = result.concat(componentDescriptor.value)
            break
          default:
            assertNever(componentDescriptor)
        }
      }
    }
    return { descriptors: result, errors: errors }
  } catch (e) {
    console.error(e)
    return { descriptors: [], errors: [{ type: 'evaluation-error', evaluationError: e }] }
  }
}

export type ComponentBoundsByModule = {
  [moduleName: string]: { [componentName: string]: BoundsInFile }
}

function generateComponentBounds(
  descriptorFile: TextFileContentsWithPath,
): ComponentBoundsByModule {
  const ast = BabelParser.parse(descriptorFile.file.code, {
    sourceType: 'module',
    plugins: ['jsx'],
  })

  const bounds: ComponentBoundsByModule = {}

  // Store variable declarations
  const variableDeclarations: Record<string, any> = {}
  // Traverse the AST to collect variable declarations
  traverse(ast, {
    VariableDeclarator(path) {
      const { id, init } = path.node
      if (id.type === 'Identifier' && init != null) {
        variableDeclarations[id.name] = init
      }
    },
  })

  // Function to resolve a variable to its object value
  function resolveVariable(node: any): any {
    if (node.type === 'Identifier' && variableDeclarations[node.name] != null) {
      return variableDeclarations[node.name]
    }
    return node
  }

  // Traverse the AST to find the default export and process the Components object
  traverse(ast, {
    ExportDefaultDeclaration(path) {
      const declaration = path.node.declaration
      if (declaration.type === 'Identifier') {
        const variableName = declaration.name
        const componentsNode = variableDeclarations[variableName]
        if (componentsNode?.type === 'ObjectExpression') {
          componentsNode.properties.forEach((prop: any) => {
            if (prop.type === 'ObjectProperty' && prop.key.type === 'StringLiteral') {
              const moduleName = prop.key.value
              if (bounds[moduleName] == null) {
                bounds[moduleName] = {}
              }
              const moduleValue = resolveVariable(prop.value)
              if (moduleValue.type === 'ObjectExpression') {
                moduleValue.properties.forEach((innerProp: any) => {
                  if (innerProp.type === 'ObjectProperty' && innerProp.key.type === 'Identifier') {
                    const componentName = innerProp.key.name
                    const { loc } = innerProp
                    if (loc != null) {
                      bounds[moduleName][componentName] = boundsInFile(
                        descriptorFile.path,
                        loc.start.line,
                        loc.start.column,
                        loc.end.line,
                        loc.end.column,
                      )
                    }
                  }
                })
              }
            }
          })
        }
      }
    },
  })
  return bounds
}

function simpleErrorMessage(
  fileName: string,
  error: string,
  severity: ErrorMessageSeverity = 'fatal',
): ErrorMessage {
  return errorMessage(
    fileName,
    null,
    null,
    null,
    null,
    '',
    severity,
    '',
    error,
    '',
    'component-descriptor',
    null,
  )
}

function errorsFromComponentRegistration(
  fileName: string,
  errors: ComponentDescriptorRegistrationError[],
): ErrorMessage[] {
  return errors.flatMap((error) => {
    switch (error.type) {
      case 'file-unparsed':
        // we control whether a file is parsed or not, so this failure mode is not surfaced to users
        return []
      case 'file-parse-failure':
        return error.parseErrorMessages
      case 'evaluation-error':
        if ((error.evaluationError as FancyError).hasOwnProperty('stackFrames')) {
          const fancyError = error.evaluationError as FancyError
          const errorMsgFromFancyError = fancyErrorToErrorMessage(fancyError)
          if (errorMsgFromFancyError != null) {
            return errorMsgFromFancyError
          }
        }
        if (error.evaluationError instanceof Error) {
          return [
            simpleErrorMessage(
              fileName,
              `${error.evaluationError.name}: ${error.evaluationError.message}`,
            ),
          ]
        }
        // Note: this is very ugly and should not happen at all, but I keep it here so we see if it happens
        return [simpleErrorMessage(fileName, JSON.stringify(error))]
      case 'no-export-default':
        return [simpleErrorMessage(fileName, `Components file has no default export`)]
      case 'no-exported-component-descriptors':
        return [simpleErrorMessage(fileName, `Cannot extract default export from file`)]
      case 'invalid-schema':
        const errorDetails = getParseErrorDetails(error.invalidSchemaError)
        return [
          simpleErrorMessage(
            fileName,
            `Malformed component registration: ${errorDetails.path}: ${errorDetails.description}`,
          ),
        ]
      case 'cannot-extract-component':
        return [
          simpleErrorMessage(
            fileName,
            `Malformed component registration: ${error.componentExtractionError}`,
          ),
        ]
      case 'registration-validation-failed':
        return [
          simpleErrorMessage(
            fileName,
            `Validation failed: ${messageForComponentRegistrationValidationError(
              error.validationError,
            )}`,
            error.severity,
          ),
        ]
      default:
        assertNever(error)
    }
  })
}

export interface TextFileContentsWithPath {
  file: TextFileContents
  path: string
}

export async function maybeUpdatePropertyControls(
  previousPropertyControlsInfo: PropertyControlsInfo,
  filesToUpdate: TextFileContentsWithPath[],
  workers: UtopiaTsWorkers,
  dispatch: EditorDispatch,
  evaluator: ModuleEvaluator,
) {
  let componentDescriptorUpdates: ComponentDescriptorFileLookup = {}
  let errors: { [filename: string]: ErrorMessage[] } = {}
  await Promise.all(
    filesToUpdate.map(async (file) => {
      const result = await getComponentDescriptorPromisesFromParseResult(file, workers, evaluator)
      errors[file.path] = errorsFromComponentRegistration(file.path, result.errors)
      if (result.descriptors.length > 0) {
        componentDescriptorUpdates[file.path] = result.descriptors
      }
    }),
  )

  const updatedPropertyControlsInfo = updatePropertyControlsOnDescriptorFileUpdate(
    previousPropertyControlsInfo,
    componentDescriptorUpdates,
  )
  dispatch([
    updatePropertyControlsInfo(updatedPropertyControlsInfo),
    setCodeEditorComponentDescriptorErrors(errors),
  ])
}

interface ComponentDescriptorFileLookup {
  [path: string]: ComponentDescriptorWithName[]
}

export function updatePropertyControlsOnDescriptorFileDelete(
  previousPropertyControlsInfo: PropertyControlsInfo,
  componentDescriptorFile: string,
): PropertyControlsInfo {
  return updatePropertyControlsOnDescriptorFileUpdate(previousPropertyControlsInfo, {
    [componentDescriptorFile]: [],
  })
}

export function updatePropertyControlsOnDescriptorFileUpdate(
  previousPropertyControlsInfo: PropertyControlsInfo,
  componentDescriptorUpdates: ComponentDescriptorFileLookup,
): PropertyControlsInfo {
  let updatedPropertyControls: PropertyControlsInfo = {}

  // go through the entries and only keep the ones that are not updated
  Object.entries(previousPropertyControlsInfo).forEach(([moduleName, moduleDescriptor]) => {
    const stillValidPropertyControls = Object.fromEntries(
      Object.entries(moduleDescriptor).filter(
        ([_, componentDescriptor]) =>
          isDefaultComponentDescriptor(componentDescriptor.source) ||
          componentDescriptorUpdates[componentDescriptor.source.sourceDescriptorFile] == null,
      ),
    )
    if (Object.keys(stillValidPropertyControls).length > 0) {
      updatedPropertyControls[moduleName] = stillValidPropertyControls
    }
  })

  // go through the updates and add them to the property controls
  Object.values(componentDescriptorUpdates).forEach((descriptors) => {
    descriptors.forEach((descriptor) => {
      if (updatedPropertyControls[descriptor.moduleName] == null) {
        updatedPropertyControls[descriptor.moduleName] = {}
      }

      updatedPropertyControls[descriptor.moduleName][descriptor.componentName] = {
        properties: descriptor.properties,
        supportsChildren: descriptor.supportsChildren,
        preferredChildComponents: descriptor.preferredChildComponents,
        variants: descriptor.variants,
        source: descriptor.source,
        focus: descriptor.focus,
        inspector: descriptor.inspector,
        emphasis: descriptor.emphasis,
        icon: descriptor.icon,
        label: descriptor.label,
      }
    })
  })

  return updatedPropertyControls
}

type TypedComponentExample =
  | { type: 'name-as-string'; name: string }
  | { type: 'component-reference'; reference: any }
  | { type: 'component-insert-option'; example: ComponentInsertOption }

function componentExampleToTyped(example: ComponentExample): TypedComponentExample {
  if ('name' in example) {
    return { type: 'name-as-string', name: example.name }
  }

  if ('component' in example) {
    return { type: 'component-reference', reference: example.component }
  }

  return { type: 'component-insert-option', example: example }
}

async function parseCodeFromInsertOption(
  componentInsertOption: ComponentInsertOption,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentInfo>> {
  const info = await parseComponentFromInsertOption(componentInsertOption, workers)

  if (isLeft(info)) {
    return info
  }

  return right({
    insertMenuLabel: componentInsertOption.label,
    elementToInsert: () => info.value.elementToInsert,
    importsToAdd: info.value.imports,
  })
}

function componentInsertOptionFromExample(
  example: ComponentExample,
  moduleName: string,
): ComponentInsertOption {
  const typed = componentExampleToTyped(example)
  switch (typed.type) {
    case 'name-as-string':
      if (intrinsicHTMLElementNamesAsStrings.includes(typed.name)) {
        return {
          label: typed.name,
          code: `<${typed.name} />`,
        }
      }
      const jsxName = jsxElementNameFromString(typed.name)
      return {
        label: typed.name,
        imports: `import {${jsxName.baseVariable}} from '${moduleName}'`,
        code: `<${typed.name} />`,
      }
    case 'component-reference':
      const requireInfo = getRequireInfoFromComponent(typed.reference)
      if (requireInfo.moduleName == null || requireInfo.name == null) {
        // TODO: we need a way better error message here
        throw new Error('Cannot find component info')
      }
      return {
        label: requireInfo.name,
        imports: `import {${requireInfo.name}} from '${requireInfo.moduleName}'`,
        code: `<${requireInfo.name} />`,
      }
    case 'component-insert-option':
      return typed.example
    default:
      assertNever(typed)
  }
}

function variantsFromJSComponentExample(
  variants: ComponentExample | ComponentExample[],
  moduleName: string,
): Array<ComponentInsertOption> {
  if (Array.isArray(variants)) {
    return variants.map((v) => componentInsertOptionFromExample(v, moduleName))
  }

  return [componentInsertOptionFromExample(variants, moduleName)]
}

interface ParsedComponentVariant {
  imports: Imports
  elementToInsert: JSXElementChildWithoutUID
}

async function parseComponentFromInsertOption(
  insertOption: ComponentInsertOption,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ParsedComponentVariant>> {
  const imports =
    insertOption.imports == null ? '' : valueOrArrayToArray(insertOption.imports).join('\n')

  const parsedParams = await getCachedParseResultForUserStrings(workers, imports, insertOption.code)

  if (isLeft(parsedParams)) {
    return parsedParams
  }

  return right({
    imports: parsedParams.value.importsToAdd,
    elementToInsert: parsedParams.value.elementToInsert,
  })
}

type PropertyDescriptorResult<T> = Either<string, T>

async function parseJSXControlDescription(
  descriptor: JSXControlDescriptionFromUtopia,
  context: { moduleName: string; workers: UtopiaTsWorkers },
): Promise<PropertyDescriptorResult<JSXControlDescription>> {
  if (descriptor.preferredContents == null) {
    return right({
      ...descriptor,
      preferredChildComponents: [],
    })
  }

  const preferredChildComponents = await parsePreferredChildren(
    valueOrArrayToArray(descriptor.preferredContents),
    'placeholder',
    context.moduleName,
    context.workers,
  )

  if (isLeft(preferredChildComponents)) {
    return preferredChildComponents
  }

  return right({
    ...descriptor,
    preferredChildComponents: preferredChildComponents.value,
  })
}

function parseAllowedEnumType(option: AllowedEnumTypeFromUtopia): RadioControlOption<unknown> {
  return { type: 'allowed-enum-type', allowedEnumType: option }
}

function parseBasicControlOptionWithIcon(
  option: BasicControlOptionWithIconFromUtopia<unknown>,
): RadioControlOption<unknown> {
  return {
    type: 'control-option-with-icon',
    option: {
      label: option.label,
      value: option.value,
      icon: option.icon ?? null,
    },
  }
}

function isAllowedEnumType(
  option: AllowedEnumTypeFromUtopia | BasicControlOptionWithIconFromUtopia<unknown>,
): option is AllowedEnumTypeFromUtopia {
  return (
    typeof option === 'string' ||
    typeof option === 'boolean' ||
    typeof option === 'number' ||
    typeof option === 'undefined' ||
    (typeof option === 'object' && option === null)
  )
}

function parseRadioControlOptions(
  options: AllowedEnumTypeFromUtopia[] | BasicControlOptionWithIconFromUtopia<unknown>[],
): Either<string, RadioControlOption<unknown>[]> {
  const allowedEnumTypes = sequenceEither(
    options.map(
      (o): Either<string, RadioControlOption<unknown>> =>
        isAllowedEnumType(o) ? right(parseAllowedEnumType(o)) : left('Not an allowed enum type'),
    ),
  )

  if (isRight(allowedEnumTypes)) {
    return allowedEnumTypes
  }

  const basicControlOptions = sequenceEither(
    options.map(
      (o): Either<string, RadioControlOption<unknown>> =>
        isAllowedEnumType(o)
          ? left('Not a BasicControlOptionWithIcon<unknown>>')
          : right(parseBasicControlOptionWithIcon(o)),
    ),
  )

  if (isRight(basicControlOptions)) {
    return basicControlOptions
  }

  return left('Cannot mix AllowedEnumType and BasicControlOptionWithIcon')
}

async function parseRadioControlDescription(
  descriptor: RadioControlDescriptionFromUtopia,
): Promise<PropertyDescriptorResult<RadioControlDescription>> {
  const options = parseRadioControlOptions(descriptor.options)
  if (isLeft(options)) {
    return options
  }

  return right({
    control: descriptor.control,
    label: descriptor.label,
    visibleByDefault: descriptor.visibleByDefault,
    options: options.value,
    required: descriptor.required,
    defaultValue: isAllowedEnumType(descriptor.defaultValue)
      ? parseAllowedEnumType(descriptor.defaultValue)
      : parseBasicControlOptionWithIcon(descriptor.defaultValue),
    folder: descriptor.folder,
  })
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
    if (descriptor.control === 'radio') {
      return parseRadioControlDescription(descriptor)
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
    const parsedRegularControl = await makeRegularControlDescription(descriptor, context)
    if (isLeft(parsedRegularControl)) {
      return parsedRegularControl
    }
    result[propertyName] = parsedRegularControl.value
  }

  return right(result)
}

async function parsePreferredChildren(
  preferredContents: PreferredContents[],
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, Array<PreferredChildComponentDescriptor>>> {
  let descriptors: PreferredChildComponentDescriptor[] = []

  for await (const contents of preferredContents) {
    if (contents === 'text') {
      descriptors.push({
        name: componentName,
        moduleName: null,
        variants: [
          {
            insertMenuLabel: 'text',
            elementToInsert: () =>
              jsxElement('span', '', jsxAttributesFromMap({}), [jsxTextBlock('Sample text')]),
            importsToAdd: {},
          },
        ],
      })
    } else {
      const variants = valueOrArrayToArray(contents.variants)
      const parsedVariants = sequenceEither(
        await Promise.all(
          variants.map((c) =>
            parseCodeFromInsertOption(componentInsertOptionFromExample(c, moduleName), workers),
          ),
        ),
      )

      if (isLeft(parsedVariants)) {
        return parsedVariants
      }

      descriptors.push({
        name: contents.component,
        moduleName: contents.moduleName ?? null,
        variants: parsedVariants.value,
      })
    }
  }

  return right(descriptors)
}

export function defaultImportsForComponentModule(
  componentName: string,
  moduleName: string | null,
): Imports {
  const jsxName = jsxElementNameFromString(componentName)
  return moduleName == null
    ? {}
    : {
        [moduleName]: importDetails(null, [importAlias(jsxName.baseVariable)], null),
      }
}

export async function parsePreferredChildrenExamples(
  componentName: string,
  moduleName: string,
  componentToRegister: ComponentToRegister,
  workers: UtopiaTsWorkers,
): Promise<Either<string, Array<PreferredChildComponentDescriptor>>> {
  if (
    componentToRegister.children == null ||
    typeof componentToRegister.children === 'string' ||
    componentToRegister.children.preferredContents == null
  ) {
    return right([])
  }

  const descriptors = await parsePreferredChildren(
    valueOrArrayToArray(componentToRegister.children.preferredContents),
    componentName,
    moduleName,
    workers,
  )

  return descriptors
}

async function parseComponentVariants(
  componentToRegister: ComponentToRegister,
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
): Promise<Either<string, ComponentInfo[]>> {
  if (
    componentToRegister.variants == null ||
    (Array.isArray(componentToRegister.variants) && componentToRegister.variants.length === 0)
  ) {
    const jsxName = jsxElementNameFromString(componentName)
    const parsed = await parseCodeFromInsertOption(
      {
        label: componentName,
        imports: `import { ${jsxName.baseVariable} } from '${moduleName}'`,
        code: `<${componentName} />`,
      },
      workers,
    )

    return mapEither((p) => [p], parsed)
  }

  const insertOptionsToParse =
    componentToRegister.variants == null
      ? []
      : variantsFromJSComponentExample(componentToRegister.variants, moduleName)

  const parsedVariants = sequenceEither(
    await Promise.all(
      insertOptionsToParse.map((insertOption) => parseCodeFromInsertOption(insertOption, workers)),
    ),
  )

  return parsedVariants
}

function parseInspectorSpec(inspector: InspectorSpec | undefined): TypedInpsectorSpec {
  if (inspector == null) {
    return ComponentDescriptorDefaults.inspector
  }
  if (inspector === 'hidden') {
    return { type: 'hidden' }
  }

  return {
    type: 'shown',
    display: inspector.display ?? 'expanded',
    sections: inspector.sections ?? [...StylingOptions],
  }
}

export async function componentDescriptorForComponentToRegister(
  componentToRegister: ComponentToRegister,
  componentName: string,
  moduleName: string,
  workers: UtopiaTsWorkers,
  source: ComponentDescriptorSource,
): Promise<Either<string, ComponentDescriptorWithName>> {
  const parsedVariants = await parseComponentVariants(
    componentToRegister,
    componentName,
    moduleName,
    workers,
  )

  if (isLeft(parsedVariants)) {
    return parsedVariants
  }
  const childrenPropSpec = await parsePreferredChildrenExamples(
    componentName,
    moduleName,
    componentToRegister,
    workers,
  )
  if (isLeft(childrenPropSpec)) {
    return childrenPropSpec
  }

  const properties = await makePropertyDescriptors(componentToRegister.properties, {
    moduleName,
    workers,
  })

  if (isLeft(properties)) {
    return properties
  }

  const supportsChildren = componentToRegister.children !== 'not-supported'

  return right({
    componentName: componentName,
    properties: properties.value,
    variants: parsedVariants.value,
    moduleName: moduleName,
    source: source,
    supportsChildren: supportsChildren,
    preferredChildComponents: childrenPropSpec.value,
    focus: componentToRegister.focus ?? ComponentDescriptorDefaults.focus,
    inspector: parseInspectorSpec(componentToRegister.inspector),
    emphasis: componentToRegister.emphasis ?? ComponentDescriptorDefaults.emphasis,
    icon: componentToRegister.icon ?? ComponentDescriptorDefaults.icon,
    label: componentToRegister.label ?? null,
  })
}

function fullyParsePropertyControls(value: unknown): ParseResult<PropertyControlsFromUtopiaAPI> {
  return parseObject(parseControlDescription)(value)
}

export const parseComponentInsertOption = objectParser<ComponentInsertOption>({
  code: parseString,
  label: parseString,
  imports: optionalProp(
    parseAlternative<string | string[]>(
      [parseString, parseArray(parseString)],
      'Invalid imports prop',
    ),
  ),
})

const parseComponentName = (value: unknown) =>
  mapEither((name) => ({ name }), objectKeyParser(parseString, 'name')(value))
const parseComponentFunction = (value: unknown) =>
  mapEither((component) => ({ component }), objectKeyParser(parseAny, 'component')(value))

export const parseComponentExample = parseAlternative<ComponentExample>(
  [parseComponentName, parseComponentFunction, parseComponentInsertOption],
  'Invalid component example',
)

export function parsePreferredContents(value: unknown): ParseResult<PreferredContents> {
  const parsePreferredComponentObject = objectParser<{
    component: string
    moduleName?: string
    variants: ComponentExample | ComponentExample[]
  }>({
    component: parseString,
    moduleName: optionalProp(parseString),
    variants: parseAlternative<ComponentExample | ComponentExample[]>(
      [parseComponentExample, parseArray(parseComponentExample)],
      'Invalid preferred content',
    ),
  })

  return parseAlternative<PreferredContents>(
    [parseConstant('text'), parsePreferredComponentObject],
    'Invalid preferred contents value',
  )(value)
}

export const parseChildrenSpec = (value: unknown): ParseResult<ChildrenSpec> => {
  return mapEither(
    (preferredContents) => ({ preferredContents }),
    optionalObjectKeyParser(
      parseAlternative<PreferredContents | PreferredContents[]>(
        [parsePreferredContents, parseArray(parsePreferredContents)],
        'Invalid preferredContents value',
      ),
      'preferredContents',
    )(value),
  )
}

const parseSectionSpec = objectParser<SectionSpec>({
  display: optionalProp(parseEnum<Display>(DisplayOptions)),
  sections: optionalProp(parseArray(parseEnum(StylingOptions))),
})

const parseComponentToRegister = objectParser<ComponentToRegister>({
  component: parseAny,
  label: optionalProp(parseString),
  properties: fullyParsePropertyControls,
  variants: optionalProp(
    parseAlternative<ComponentExample | Array<ComponentExample>>(
      [parseComponentInsertOption, parseArray(parseComponentInsertOption)],
      'Invalid variants prop',
    ),
  ),
  children: optionalProp(
    parseAlternative<Children>(
      [parseConstant('supported'), parseConstant('not-supported'), parseChildrenSpec],
      'Invalid children prop',
    ),
  ),
  focus: optionalProp(parseEnum(FocusOptions)),
  inspector: optionalProp(
    parseAlternative<'hidden' | SectionSpec>(
      [parseConstant('hidden'), parseSectionSpec],
      'inspector value invalid',
    ),
  ),
  emphasis: optionalProp(parseEnum(EmphasisOptions)),
  icon: optionalProp(parseEnum(IconOptions)),
})

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

function fancyErrorToErrorMessage(error: FancyError): ErrorMessage | null {
  const frames = error.stackFrames
  if (frames != null && frames.length > 0) {
    const code = printScriptLines(frames[0]._originalScriptCode ?? [], frames[0].columnNumber)
    return errorMessage(
      frames[0]._originalFileName ?? '',
      frames[0]._originalLineNumber,
      frames[0]._originalColumnNumber,
      null,
      null,
      code,
      'fatal',
      '',
      `${error.name}: ${error.message}`,
      '',
      'component-descriptor',
      null,
    )
  }
  return null
}

function printScriptLines(scriptLines: Array<ScriptLine>, columnNumber: number | null): string {
  const maxLineNumberLength = Math.max(...scriptLines.map((c) => c.lineNumber.toString().length))
  const printedCode =
    scriptLines
      .map((c) => {
        const prefix = c.highlight ? `> ` : '  '

        // we need to have the same length for all line numbers
        const lineNumberStr =
          c.lineNumber.toString().length < maxLineNumberLength
            ? ` ${c.lineNumber.toString()}`
            : c.lineNumber.toString()

        // an extra line is added to after the highlighted line when we have a column number,
        // so we can highlight the actual column with a ^
        let extraLine = ''
        if (c.highlight && columnNumber != null) {
          extraLine = `\n${Array(columnNumber + maxLineNumberLength + 2)
            .fill(' ')
            .join('')}^`
        }

        return `${prefix}${lineNumberStr} | ${c.content}${extraLine}`
      })
      .join('\n') ?? ''

  return printedCode
}

export function getAllComponentDescriptorFilePaths(
  projectContents: ProjectContentTreeRoot,
): Array<string> {
  let componentDescriptorFiles: Array<string> = []

  walkContentsTree(projectContents, (fullPath, file) => {
    if (isTextFile(file) && isComponentDescriptorFile(fullPath)) {
      componentDescriptorFiles.push(fullPath)
    }
  })
  return componentDescriptorFiles
}
