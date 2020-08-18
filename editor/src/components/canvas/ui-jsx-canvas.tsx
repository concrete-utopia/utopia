import * as React from 'react'
import { MapLike } from 'typescript'
import { NormalisedFrame, View, Storyboard, PropertyControls } from 'utopia-api'
import { colorTheme, FlexRow, UtopiaStyles, UtopiaTheme } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
// Inject the babel helpers into the global scope
import '../../bundled-dependencies/babelHelpers'
import * as PP from '../../core/shared/property-path'
import * as TP from '../../core/shared/template-path'
import { UTOPIA_ORIGINAL_ID_KEY } from '../../core/model/element-metadata-utils'
import {
  ArbitraryJSBlock,
  ElementInstanceMetadata,
  ElementsWithin,
  emptySpecialSizeMeasurements,
  getJSXElementNameAsString,
  isArbitraryJSBlock,
  isJSXArbitraryBlock,
  isJSXElement,
  isUtopiaJSXComponent,
  JSXArbitraryBlock,
  jsxAttributeValue,
  JSXElement,
  JSXElementChild,
  MetadataWithoutChildren,
  TopLevelElement,
  UtopiaJSXComponent,
  ComponentMetadataWithoutRootElements,
  jsxElement,
  Param,
  isRegularParam,
  BoundParam,
  isDestructuredObject,
  isOmittedParam,
  JSXAttributeOtherJavaScript,
  emptyComputedStyle,
  isIntrinsicElement,
  isIntrinsicHTMLElement,
  isJSXFragment,
} from '../../core/shared/element-template'
import { getUtopiaID, getValidTemplatePaths } from '../../core/model/element-template-utils'
import {
  jsxAttributesToProps,
  setJSXValueAtPath,
  jsxAttributeToValue,
  AnyMap,
} from '../../core/shared/jsx-attributes'
import { getOrDefaultScenes } from '../../core/model/project-file-utils'
import {
  Imports,
  InstancePath,
  SceneContainer,
  SceneMetadata,
  ScenePath,
  TemplatePath,
  isParseSuccess,
  StaticInstancePath,
  NodeModules,
} from '../../core/shared/project-file-types'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../core/workers/parser-printer/parser-printer-utils'
import { applyUIDMonkeyPatch, makeCanvasElementPropsSafe } from '../../utils/canvas-react-utils'
import { flatMapEither, forEachRight, right, left, isRight } from '../../core/shared/either'
import Utils from '../../utils/utils'
import { CanvasVector } from '../../core/shared/math-utils'
import { UtopiaRequireFn } from '../custom-code/code-file'
import { importResultFromImports } from '../editor/npm-dependency/npm-dependency'
import {
  DerivedState,
  EditorState,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  UIFileBase64Blobs,
  ConsoleLog,
  getIndexHtmlFileFromEditorState,
} from '../editor/store/editor-state'
import { proxyConsole } from './console-proxy'
import { useDomWalker } from './dom-walker'
import { resolveParamsAndRunJsCode } from '../../core/shared/javascript-cache'
import { isLiveMode } from '../editor/editor-modes'
import { optionalMap } from '../../core/shared/optional-utils'
import { defaultPropertiesForComponent } from '../../core/property-controls/property-controls-utils'
import {
  isSceneElement,
  BakedInStoryboardVariableName,
  EmptyScenePathForStoryboard,
  PathForResizeContent,
} from '../../core/model/scene-utils'
import { WarningIcon } from '../../uuiui/warning-icon'
import { getMemoizedRequireFn } from '../../core/es-modules/package-manager/package-manager'
import { EditorDispatch } from '../editor/action-types'
import { resolveModule } from '../../core/es-modules/package-manager/module-resolution'
import { useKeepReferenceEqualityIfPossible } from '../inspector/common/property-path-hooks'
import { usePrevious } from '../editor/hook-utils'
import { arrayEquals, fastForEach } from '../../core/shared/utils'
import { unimportCSSFile } from '../../core/shared/css-style-loader'
import { removeAll, flatMapArray } from '../../core/shared/array-utils'
import { normalizeName } from '../custom-code/custom-code-utils'
import { omitWithPredicate, objectMap } from '../../core/shared/object-utils'
import { getGeneratedExternalLinkText } from '../../printer-parsers/html/external-resources-parser'
import { Helmet } from 'react-helmet'
import parse from 'html-react-parser'
import { cssValueOnlyContainsComments } from '../../printer-parsers/css/css-parser-utils'

const emptyFileBlobs: UIFileBase64Blobs = {}

export type SpyValues = {
  metadata: { [templatePath: string]: MetadataWithoutChildren }
  scenes: { [templatePath: string]: ComponentMetadataWithoutRootElements }
}

export interface UiJsxCanvasContextData {
  current: {
    spyValues: SpyValues
  }
}

export function emptyUiJsxCanvasContextData(): UiJsxCanvasContextData {
  return {
    current: {
      spyValues: {
        metadata: {},
        scenes: {},
      },
    },
  }
}

export const UiJsxCanvasContext = React.createContext<UiJsxCanvasContextData>(
  emptyUiJsxCanvasContextData(),
)
UiJsxCanvasContext.displayName = 'UiJsxCanvasContext'

export interface UiJsxCanvasProps {
  offset: CanvasVector
  scale: number
  uiFilePath: string | null
  requireFn: UtopiaRequireFn | null
  hiddenInstances: TemplatePath[]
  editedTextElement: InstancePath | null
  fileBlobs: UIFileBase64Blobs
  mountCount: number
  onDomReport: (elementMetadata: Array<ElementInstanceMetadata>) => void
  walkDOM: boolean
  imports: Imports
  topLevelElementsIncludingScenes: Array<TopLevelElement>
  dependencyOrdering: Array<string>
  jsxFactoryFunction: string | null
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean // FOR ui-jsx-canvas.spec TESTS ONLY!!!! this prevents us from having to update the legacy test snapshots
  clearConsoleLogs: () => void
  addToConsoleLogs: (log: ConsoleLog) => void
  linkTags: string
}

export interface CanvasReactReportErrorCallback {
  reportError: (editedFile: string, error: Error, errorInfo?: React.ErrorInfo) => void
}

export interface CanvasReactErrorCallback extends CanvasReactReportErrorCallback {
  clearErrors: () => void
}

export type UiJsxCanvasPropsWithErrorCallback = UiJsxCanvasProps & CanvasReactErrorCallback

const emptyImports: Imports = {}
const emptyTopLevelElements: Array<TopLevelElement> = []
const emptyDependencyOrdering: Array<string> = []

export function pickUiJsxCanvasProps(
  editor: EditorState,
  derived: DerivedState,
  walkDOM: boolean,
  onDomReport: (elementMetadata: Array<ElementInstanceMetadata>) => void,
  clearConsoleLogs: () => void,
  addToConsoleLogs: (log: ConsoleLog) => void,
  dispatch: EditorDispatch,
): UiJsxCanvasProps {
  const defaultedFileBlobs = Utils.defaultIfNull(
    emptyFileBlobs,
    Utils.optionalFlatMap((key) => editor.canvas.base64Blobs[key], getOpenUIJSFileKey(editor)),
  )

  let imports: Imports = emptyImports
  let topLevelElementsIncludingScenes: Array<TopLevelElement> = emptyTopLevelElements
  let dependencyOrdering: Array<string> = emptyDependencyOrdering
  let jsxFactoryFunction: string | null = null
  const uiFile = getOpenUIJSFile(editor)

  if (uiFile != null && isParseSuccess(uiFile.fileContents)) {
    const success = uiFile.fileContents.value
    const transientCanvasState = derived.canvas.transientState
    dependencyOrdering = success.dependencyOrdering
    imports = uiFile.fileContents.value.imports
    topLevelElementsIncludingScenes = success.topLevelElements
    jsxFactoryFunction = success.jsxFactoryFunction
    const transientFileState = transientCanvasState.fileState
    if (transientFileState != null) {
      imports = transientFileState.imports
      topLevelElementsIncludingScenes = transientFileState.topLevelElementsIncludingScenes
    }
  }
  const requireFn = editor.codeResultCache.requireFn

  let linkTags = ''
  const indexHtml = getIndexHtmlFileFromEditorState(editor)
  if (isRight(indexHtml)) {
    const parsedLinkTags = getGeneratedExternalLinkText(indexHtml.value.fileContents)
    if (isRight(parsedLinkTags)) {
      linkTags = parsedLinkTags.value
    }
  }

  return {
    offset: editor.canvas.roundedCanvasOffset,
    scale: editor.canvas.scale,
    uiFilePath: getOpenUIJSFileKey(editor),
    requireFn: requireFn,
    hiddenInstances: editor.hiddenInstances,
    editedTextElement: Utils.optionalMap((textEd) => textEd.templatePath, editor.canvas.textEditor),
    fileBlobs: defaultedFileBlobs,
    mountCount: editor.canvas.mountCount,
    onDomReport: onDomReport,
    walkDOM: walkDOM,
    imports: imports,
    topLevelElementsIncludingScenes: topLevelElementsIncludingScenes,
    dependencyOrdering: dependencyOrdering,
    jsxFactoryFunction: jsxFactoryFunction,
    clearConsoleLogs: clearConsoleLogs,
    addToConsoleLogs: addToConsoleLogs,
    canvasIsLive: isLiveMode(editor.mode),
    shouldIncludeCanvasRootInTheSpy: true,
    linkTags: linkTags,
  }
}

function runBlockUpdatingScope(
  requireResult: MapLike<any>,
  block: ArbitraryJSBlock,
  currentScope: MapLike<any>,
  errorHandler: (error: Error) => void,
): void {
  try {
    const result = resolveParamsAndRunJsCode(block, requireResult, currentScope)
    Utils.fastForEach(block.definedWithin, (within) => {
      currentScope[within] = result[within]
    })
  } catch (e) {
    errorHandler(e)
  }
}

function runJSXArbitraryBlock(
  requireResult: MapLike<any>,
  block: JSXArbitraryBlock,
  currentScope: MapLike<any>,
  errorHandler: (error: Error) => void,
): any {
  try {
    return resolveParamsAndRunJsCode(block, requireResult, currentScope)
  } catch (e) {
    errorHandler(e)
  }
}

function fixStyleObjectRemoveCommentOnlyValues(props: Readonly<unknown>): any {
  if (typeof props === 'object' && 'style' in props) {
    const propsAsAny = props as any
    const style = propsAsAny.style
    const fixedStyle: any = objectMap((styleProp) => {
      if (typeof styleProp === 'string' && cssValueOnlyContainsComments(styleProp)) {
        /**
         * see https://github.com/facebook/react/issues/19477
         * our problem: we store the disabled style values as commented out,
         * and we allow a style prop to only contain commented out values.
         *
         * This is fine if you render something from scratch, so this will never be an issue for our users.
         *
         * But in the case of the canvas, when you change the value from _something_ to _only comments_,
         * we expect the DOM API to clear out the previous value. The real behavior however is to ignore the comments-only new value,
         * and keep the old value alive.
         *
         * Solution: we explicitly mange the style prop such that if a property only contains comments, we replace it with a `null`,
         * which the DOM API will treat as "remove existing value" as expected.
         *
         * Example: { backgroundColor: '\/*red*\/ \/*green*\/' } should disable the backgroundColor, so we will
         * replace it with { backgroundColor: null } in the Canvas.
         */
        return null
      } else {
        return styleProp
      }
    }, style)
    return {
      ...propsAsAny,
      style: fixedStyle,
    }
  } else {
    // no style props, just return the props object without mangling
    return props
  }
}

function renderComponentUsingJsxFactoryFunction(
  inScope: MapLike<any>,
  factoryFunctionName: string | null,
  type: any,
  props: any,
  ...children: Array<any>
): any {
  const fixedProps = fixStyleObjectRemoveCommentOnlyValues(props)
  let factoryFunction: Function = React.createElement
  if (factoryFunctionName != null) {
    if (factoryFunctionName in inScope) {
      factoryFunction = inScope[factoryFunctionName]
    } else {
      throw new Error(`Unable to find factory function ${factoryFunctionName} in scope.`)
    }
  }
  // This is disgusting, but we want to make sure that if there is only one child it isn't wrapped in an array,
  // since that code that uses `React.Children.only`
  const childrenToRender = children.map((innerChildren) =>
    innerChildren != null && Array.isArray(innerChildren) && innerChildren.length === 1
      ? innerChildren[0]
      : innerChildren,
  )
  return factoryFunction.call(null, type, fixedProps, ...childrenToRender)
}

interface MutableUtopiaContextProps {
  requireResult: MapLike<any>
  fileBlobs: UIFileBase64Blobs
  rootScope: MapLike<any>
  reportError: (error: Error, errorInfo?: React.ErrorInfo) => void
  jsxFactoryFunctionName: string | null
}

const MutableUtopiaContext = React.createContext<{ current: MutableUtopiaContextProps }>({
  current: {
    requireResult: {},
    fileBlobs: {},
    rootScope: {},
    reportError: Utils.NO_OP,
    jsxFactoryFunctionName: null,
  },
})
MutableUtopiaContext.displayName = 'MutableUtopiaContext'

interface RerenderUtopiaContextProps {
  topLevelElements: ReadonlyMap<string, UtopiaJSXComponent>
  hiddenInstances: Array<TemplatePath>
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean
}

const RerenderUtopiaContext = React.createContext<RerenderUtopiaContextProps>({
  topLevelElements: new Map(),
  hiddenInstances: [],
  canvasIsLive: false,
  shouldIncludeCanvasRootInTheSpy: false,
})
RerenderUtopiaContext.displayName = 'RerenderUtopiaContext'

interface SceneLevelContextProps {
  validPaths: Array<InstancePath>
  scenePath: ScenePath
}

const SceneLevelUtopiaContext = React.createContext<SceneLevelContextProps>({
  validPaths: [],
  scenePath: EmptyScenePathForStoryboard,
})
SceneLevelUtopiaContext.displayName = 'SceneLevelUtopiaContext'

function updateMutableUtopiaContextWithNewProps(
  ref: React.MutableRefObject<MutableUtopiaContextProps>,
  newProps: MutableUtopiaContextProps,
): void {
  ref.current = newProps
}

export function reorderTopLevelElements(
  elements: Array<TopLevelElement>,
  ordering: Array<string>,
): Array<TopLevelElement> {
  let elementsByKey = Utils.arrayToObject(elements, (element) => {
    if (isUtopiaJSXComponent(element)) {
      return element.name
    } else {
      return element.uniqueID
    }
  })
  let result: Array<TopLevelElement> = []
  // Add elements as they appear in the ordering.
  Utils.fastForEach(ordering, (orderingKey) => {
    if (orderingKey in elementsByKey) {
      const element = elementsByKey[orderingKey]
      result.push(element)
      delete elementsByKey[orderingKey]
    }
  })
  // Cleanup any remaining elements.
  Utils.fastForEach(Object.values(elementsByKey), (element) => {
    result.push(element)
  })
  return result
}

function cssImportsFromImports(imports: Imports): Array<string> {
  let result: Array<string> = []
  Utils.fastForEach(Object.keys(imports), (importSource) => {
    if (importSource.endsWith('.css')) {
      result.push(importSource)
    }
  })
  result.sort()
  return result
}

export const UiJsxCanvas = betterReactMemo(
  'UiJsxCanvas',
  (props: UiJsxCanvasPropsWithErrorCallback) => {
    applyUIDMonkeyPatch()
    const {
      offset,
      scale,
      uiFilePath,
      requireFn,
      hiddenInstances,
      fileBlobs,
      walkDOM,
      reportError,
      onDomReport,
      topLevelElementsIncludingScenes,
      imports,
      dependencyOrdering,
      jsxFactoryFunction,
      clearErrors,
      clearConsoleLogs,
      addToConsoleLogs,
      canvasIsLive,
      linkTags,
    } = props

    // FIXME This is illegal! The two lines below are triggering a re-render
    clearConsoleLogs()
    proxyConsole(console, addToConsoleLogs)

    let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

    // Handle the imports changing, this needs to run _before_ any require function
    // calls as it's modifying the underlying DOM elements. This is somewhat working
    // like useEffect, except that runs after everything has rendered.
    const cssImports = useKeepReferenceEqualityIfPossible(cssImportsFromImports(imports))
    const previousCSSImports = usePrevious(cssImports)

    if (
      uiFilePath != null &&
      previousCSSImports != null &&
      !arrayEquals(cssImports, previousCSSImports)
    ) {
      const removed = removeAll(previousCSSImports, cssImports)
      fastForEach(removed, (toRemove) => {
        unimportCSSFile(normalizeName(uiFilePath, toRemove))
      })
    }

    const reportErrorWithPath = React.useCallback(
      (error: Error, errorInfo?: React.ErrorInfo) => {
        if (uiFilePath == null) {
          console.warn('Reporting an error with no file open.', error, errorInfo)
        } else {
          reportError(uiFilePath, error, errorInfo)
        }
      },
      [uiFilePath, reportError],
    )

    let topLevelComponentRendererComponents = React.useRef<MapLike<ComponentRendererComponent>>({})

    let mutableContextRef = React.useRef<MutableUtopiaContextProps>({
      fileBlobs: fileBlobs,
      reportError: reportErrorWithPath,
      requireResult: {},
      rootScope: {},
      jsxFactoryFunctionName: null,
    })

    if (clearErrors != null) {
      // a new canvas render, a new chance at having no errors
      // FIXME This is illegal! The line below is triggering a re-render
      clearErrors()
    }

    if (uiFilePath == null) {
      return null
    } else {
      if (requireFn != null) {
        const orderedTopLevelElements = reorderTopLevelElements(
          topLevelElementsIncludingScenes,
          dependencyOrdering,
        )

        const customRequire = (importOrigin: string, toImport: string) =>
          requireFn(importOrigin, toImport, false)

        let requireResult: MapLike<any> = {}
        let codeError: Error | null = null
        try {
          requireResult = importResultFromImports(uiFilePath, imports, customRequire)
        } catch (e) {
          codeError = e
        }

        let executionScope: MapLike<any> = { ...requireResult }
        // TODO All of this is run on every interaction o_O

        let topLevelJsxComponents: Map<string, UtopiaJSXComponent> = new Map()

        // Should something have blown up previously, don't execute a bunch of code
        // after now and potentially rewrite this error.
        if (codeError == null) {
          // Make sure there is something in scope for all of the top level components
          Utils.fastForEach(orderedTopLevelElements, (topLevelElement) => {
            if (isUtopiaJSXComponent(topLevelElement)) {
              topLevelJsxComponents.set(topLevelElement.name, topLevelElement)
              if (topLevelComponentRendererComponents.current[topLevelElement.name] == null) {
                topLevelComponentRendererComponents.current[
                  topLevelElement.name
                ] = createComponentRendererComponent({ topLevelElementName: topLevelElement.name })
              }
            }
          })

          executionScope = {
            ...executionScope,
            ...topLevelComponentRendererComponents.current,
          }

          // First make sure everything is in scope
          Utils.fastForEach(orderedTopLevelElements, (topLevelElement) => {
            if (isArbitraryJSBlock(topLevelElement)) {
              runBlockUpdatingScope(requireResult, topLevelElement, executionScope, (error) => {
                codeError = error
                reportErrorWithPath(error)
              })
            }
          })

          updateMutableUtopiaContextWithNewProps(mutableContextRef, {
            requireResult: requireResult,
            rootScope: executionScope,
            fileBlobs: fileBlobs,
            reportError: reportErrorWithPath,
            jsxFactoryFunctionName: jsxFactoryFunction,
          })
        }

        const topLevelElementsMap = new Map(topLevelJsxComponents)

        const {
          StoryboardRootComponent,
          rootValidPaths,
          storyboardRootElementPath,
          storyboardRootSceneMetadata,
          rootScenePath,
        } = getStoryboardRoot(topLevelElementsMap, executionScope)

        if (props.shouldIncludeCanvasRootInTheSpy) {
          metadataContext.current.spyValues.scenes[
            TP.toString(rootScenePath)
          ] = storyboardRootSceneMetadata
        }

        return (
          <CanvasErrorBoundary uiFilePath={uiFilePath} reportError={props.reportError}>
            <Helmet>{parse(linkTags)}</Helmet>
            <MutableUtopiaContext.Provider value={mutableContextRef}>
              <RerenderUtopiaContext.Provider
                value={{
                  hiddenInstances: hiddenInstances,
                  topLevelElements: topLevelElementsMap,
                  canvasIsLive: canvasIsLive,
                  shouldIncludeCanvasRootInTheSpy: props.shouldIncludeCanvasRootInTheSpy,
                }}
              >
                <CanvasContainer
                  walkDOM={walkDOM}
                  scale={scale}
                  offset={offset}
                  onDomReport={onDomReport}
                  codeError={codeError}
                  validRootPaths={rootValidPaths}
                  canvasRootElementTemplatePath={storyboardRootElementPath}
                >
                  <SceneLevelUtopiaContext.Provider
                    value={{ validPaths: rootValidPaths, scenePath: rootScenePath }}
                  >
                    {StoryboardRootComponent == null ? null : <StoryboardRootComponent />}
                  </SceneLevelUtopiaContext.Provider>
                </CanvasContainer>
              </RerenderUtopiaContext.Provider>
            </MutableUtopiaContext.Provider>
          </CanvasErrorBoundary>
        )
      } else {
        return null
      }
    }
  },
)

function getStoryboardRoot(
  topLevelElementsMap: Map<string, UtopiaJSXComponent>,
  executionScope: MapLike<any>,
): {
  StoryboardRootComponent: ComponentRendererComponent | undefined
  storyboardRootSceneMetadata: ComponentMetadataWithoutRootElements
  storyboardRootElementPath: StaticInstancePath
  rootValidPaths: Array<StaticInstancePath>
  rootScenePath: ScenePath
} {
  const StoryboardRootComponent = executionScope[BakedInStoryboardVariableName] as
    | ComponentRendererComponent
    | undefined

  const storyboardRootJsxComponent = topLevelElementsMap.get(BakedInStoryboardVariableName)
  const validPaths =
    storyboardRootJsxComponent == null
      ? []
      : getValidTemplatePaths(storyboardRootJsxComponent, EmptyScenePathForStoryboard)
  const storyboardRootElementPath = validPaths[0] // >:D

  const storyboardRootSceneMetadata: ComponentMetadataWithoutRootElements = {
    component: BakedInStoryboardVariableName,
    sceneResizesContent: false,
    container: {} as any, // TODO BB Hack this is not safe at all, the code expects container props
    scenePath: EmptyScenePathForStoryboard,
    templatePath: TP.instancePath([], []),
    globalFrame: null,
    label: 'Storyboard',
    style: {},
  }

  return {
    StoryboardRootComponent: StoryboardRootComponent,
    storyboardRootSceneMetadata: storyboardRootSceneMetadata,
    storyboardRootElementPath: storyboardRootElementPath,
    rootValidPaths: validPaths,
    rootScenePath: EmptyScenePathForStoryboard,
  }
}

function applyPropsParamToPassedProps(
  inScope: MapLike<any>,
  parentComponentProps: AnyMap,
  requireResult: MapLike<any>,
  onError: (error: Error) => void,
  passedProps: MapLike<unknown>,
  propsParam: Param,
): MapLike<unknown> {
  let output: MapLike<unknown> = {}

  function getParamValue(
    value: unknown,
    defaultExpression: JSXAttributeOtherJavaScript | null,
  ): unknown {
    if (value === undefined && defaultExpression != null) {
      return jsxAttributeToValue(
        inScope,
        parentComponentProps,
        requireResult,
        onError,
      )(defaultExpression)
    } else {
      return value
    }
  }

  function applyBoundParamToOutput(value: unknown, boundParam: BoundParam): void {
    if (isRegularParam(boundParam)) {
      const { paramName } = boundParam
      output[paramName] = getParamValue(value, boundParam.defaultExpression)
    } else if (isDestructuredObject(boundParam)) {
      if (typeof value === 'object' && !Array.isArray(value) && value !== null) {
        let remainingValues = { ...value } as Record<string, unknown>
        let remainingKeys = Object.keys(remainingValues)
        boundParam.parts.forEach((part) => {
          const { propertyName, param } = part
          if (propertyName != null) {
            // e.g. `{ prop: renamedProp }` or `{ prop: { /* further destructuring */ } }`
            // Can't spread if we have a property name
            const innerValue = remainingValues[propertyName]
            applyBoundParamToOutput(innerValue, param.boundParam)
            remainingKeys = remainingKeys.filter((k) => k !== propertyName)
            delete remainingValues[propertyName]
          } else {
            const { dotDotDotToken: spread, boundParam: innerBoundParam } = param
            if (isRegularParam(innerBoundParam)) {
              // e.g. `{ prop }` or `{ ...remainingProps }`
              const { paramName } = innerBoundParam
              if (spread) {
                output[paramName] = remainingValues
                remainingKeys = []
                remainingValues = {}
              } else {
                output[paramName] = getParamValue(
                  remainingValues[paramName],
                  innerBoundParam.defaultExpression,
                )
                remainingKeys = remainingKeys.filter((k) => k !== paramName)
                delete remainingValues[paramName]
              }
            }
            // No other cases are legal
            // TODO Should we throw? Users will already have a lint error
          }
        })
      }
      // TODO Throw, but what?
    } else {
      if (Array.isArray(value)) {
        let remainingValues = [...value]
        boundParam.parts.forEach((param) => {
          if (isOmittedParam(param)) {
            remainingValues.shift()
          } else {
            const { dotDotDotToken: spread, boundParam: innerBoundParam } = param
            if (isRegularParam(innerBoundParam)) {
              const { paramName } = innerBoundParam
              if (spread) {
                output[paramName] = remainingValues
                remainingValues = []
              } else {
                output[paramName] = getParamValue(
                  remainingValues.shift(),
                  innerBoundParam.defaultExpression,
                )
              }
            } else {
              const nextValue = remainingValues.shift()
              applyBoundParamToOutput(nextValue, innerBoundParam)
            }
          }
        })
      }
      // TODO Throw, but what?
    }
  }

  applyBoundParamToOutput(passedProps, propsParam.boundParam)
  return output
}

function createComponentRendererComponent(params: {
  topLevelElementName: string
}): ComponentRendererComponent {
  const Component = (realPassedProps: any) => {
    const { current: mutableContext } = React.useContext(MutableUtopiaContext)
    const rerenderUtopiaContext = React.useContext(RerenderUtopiaContext)
    const sceneContext = React.useContext(SceneLevelUtopiaContext)

    let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

    const utopiaJsxComponent = rerenderUtopiaContext.topLevelElements.get(
      params.topLevelElementName,
    )

    if (utopiaJsxComponent == null) {
      // If this element cannot be found, we want to purposefully cause a 'ReferenceError' to notify the user.
      throw new ReferenceError(`${params.topLevelElementName} is not defined`)
    }

    const appliedProps = optionalMap(
      (param) =>
        applyPropsParamToPassedProps(
          mutableContext.rootScope,
          realPassedProps,
          mutableContext.requireResult,
          mutableContext.reportError,
          realPassedProps,
          param,
        ),
      utopiaJsxComponent.param,
    ) ?? { props: realPassedProps }

    let scope: MapLike<any> = {
      ...mutableContext.rootScope,
      ...appliedProps,
    }

    const scenePath = sceneContext.scenePath
    let codeError: Error | null = null

    if (utopiaJsxComponent.arbitraryJSBlock != null) {
      runBlockUpdatingScope(
        mutableContext.requireResult,
        utopiaJsxComponent.arbitraryJSBlock,
        scope,
        (error) => {
          codeError = error
          mutableContext.reportError(error)
        },
      )
    }

    function buildComponentRenderResult(element: JSXElementChild): React.ReactElement {
      if (isJSXFragment(element)) {
        return <>{element.children.map(buildComponentRenderResult)}</>
      } else {
        // so. this template path is ONLY correct if this component is used as a Scene Root.
        // if this component is used as an instance inside some other component, this template path will be garbage.
        // but! worry not, because in cases this is an instance, we are not running the DOM-walker and we discard the spy results
        // so it is not an issue that we have a false template path
        const ownTemplatePath = TP.instancePath(scenePath, [getUtopiaID(element)])

        return renderCoreElement(
          element,
          ownTemplatePath,
          mutableContext.rootScope,
          scope,
          realPassedProps,
          mutableContext.requireResult,
          rerenderUtopiaContext.hiddenInstances,
          mutableContext.fileBlobs,
          mutableContext.reportError,
          sceneContext.validPaths,
          realPassedProps['data-uid'],
          undefined,
          metadataContext,
          mutableContext.jsxFactoryFunctionName,
          codeError,
          rerenderUtopiaContext.shouldIncludeCanvasRootInTheSpy,
        )
      }
    }

    return buildComponentRenderResult(utopiaJsxComponent.rootElement)
  }
  Component.displayName = `ComponentRenderer(${params.topLevelElementName})`
  Component.topLevelElementName = params.topLevelElementName
  return Component
}

type ComponentRendererComponent = React.ComponentType<any> & {
  topLevelElementName: string
  propertyControls?: PropertyControls
}

interface SceneRootProps extends CanvasReactReportErrorCallback {
  content: ComponentRendererComponent | undefined
  templatePath: InstancePath
  requireResult: MapLike<any>
  inScope: MapLike<any>
  hiddenInstances: Array<TemplatePath>
  componentProps: MapLike<any>
  style: React.CSSProperties
  jsxFactoryFunctionName: string | null
  container: SceneContainer
  component: string | null
  sceneResizesContent: boolean

  // this is even worse: this is secret props that are passed down from a utopia parent View
  // we put this here in case the Scene is inside another View
  parentAbsoluteFrame?: NormalisedFrame
  fileBlobs: UIFileBase64Blobs

  sceneUID: string
  sceneLabel: string | undefined

  'data-uid'?: string // the data uid
}

const SceneRoot: React.FunctionComponent<SceneRootProps> = (props) => {
  const {
    content,
    templatePath,
    requireResult,
    inScope,
    hiddenInstances,
    fileBlobs,
    reportError,
    componentProps,
    style,
    container,
    jsxFactoryFunctionName,
    component,
    sceneResizesContent,
    sceneUID,
    'data-uid': dataUidIgnore,
    ...inputProps
  } = props

  const scenePath = TP.scenePath(TP.elementPathForPath(templatePath))

  const rerenderUtopiaContext = React.useContext(RerenderUtopiaContext)
  let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

  metadataContext.current.spyValues.scenes[TP.toString(scenePath)] = {
    scenePath: scenePath,
    templatePath: templatePath,
    container: container,
    component: component,
    sceneResizesContent: sceneResizesContent,
    globalFrame: null, // the real frame comes from the DOM walker
    label: props.sceneLabel,
    style: style,
  }
  if (rerenderUtopiaContext.shouldIncludeCanvasRootInTheSpy) {
    metadataContext.current.spyValues.metadata[TP.toComponentId(templatePath)] = {
      element: left('Scene'),
      templatePath: templatePath,
      props: {},
      globalFrame: null,
      localFrame: null,
      childrenTemplatePaths: [],
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements, // This is not the nicest, but the results from the DOM walker will override this anyways
      computedStyle: emptyComputedStyle,
    }
  }

  let rootElement = null
  let validPaths: Array<InstancePath> = []
  if (content != null) {
    const defaultProps = defaultPropertiesForComponent(content)
    const passthroughProps = {
      ...defaultProps,
      ...inputProps,
      ...componentProps,
    }

    rootElement = renderComponentUsingJsxFactoryFunction(
      inScope,
      jsxFactoryFunctionName,
      content,
      passthroughProps,
      undefined,
    )

    const utopiaJsxComponent = rerenderUtopiaContext.topLevelElements.get(
      content.topLevelElementName,
    )
    if (utopiaJsxComponent != null) {
      validPaths = getValidTemplatePaths(utopiaJsxComponent, scenePath)
    }
  }

  const sceneStyle: React.CSSProperties = {
    // TODO this should really be a property of the scene that you can change, similar to the preview.
    position: 'absolute',
    backgroundColor: colorTheme.emphasizedBackground.value,
    boxShadow: rerenderUtopiaContext.canvasIsLive
      ? UtopiaStyles.scene.live.boxShadow
      : UtopiaStyles.scene.editing.boxShadow,
    ...style,
  }

  return (
    <SceneLevelUtopiaContext.Provider value={{ validPaths: validPaths, scenePath: scenePath }}>
      <View
        data-utopia-scene-id={TP.toString(scenePath)}
        data-utopia-valid-paths={validPaths.map(TP.toString).join(' ')}
        style={sceneStyle}
        layout={{
          ...container,
        }}
      >
        {rootElement}
      </View>
    </SceneLevelUtopiaContext.Provider>
  )
}
SceneRoot.displayName = 'SceneRoot'

function streamlineInFileBlobs(props: any, fileBlobs: UIFileBase64Blobs): any {
  if (typeof props === 'object' && !Array.isArray(props) && props !== null) {
    const elementID = props['data-uid']
    if (elementID in fileBlobs) {
      return {
        ...props,
        src: `data:;base64,${fileBlobs[elementID].base64}`,
      }
    } else {
      return props
    }
  } else {
    return props
  }
}

function isHidden(hiddenInstances: TemplatePath[], templatePath: TemplatePath): boolean {
  return hiddenInstances.some((path) => TP.pathsEqual(path, templatePath))
}

function hideElement(props: any): any {
  const styleProps = Utils.propOr({}, 'style', props as any)
  return {
    ...props,
    style: {
      ...styleProps,
      visibility: 'hidden',
    },
  } as any
}

function utopiaCanvasJSXLookup(
  elementsWithin: ElementsWithin,
  executionScope: MapLike<any>,
  render: (element: JSXElement, inScope: MapLike<any>) => React.ReactElement,
): (uid: string, inScope: MapLike<any>) => React.ReactElement | null {
  return (uid, inScope) => {
    const element = elementsWithin[uid]
    if (element == null) {
      return null
    } else {
      const combinedScope = { ...executionScope, ...inScope }
      return render(element, combinedScope)
    }
  }
}

function renderCoreElement(
  element: JSXElementChild,
  templatePath: InstancePath,
  rootScope: MapLike<any>,
  inScope: MapLike<any>,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  hiddenInstances: Array<TemplatePath>,
  fileBlobs: UIFileBase64Blobs,
  reportError: (error: Error, errorInfo?: React.ErrorInfo) => void,
  validPaths: Array<InstancePath>,
  uid: string | undefined,
  reactChildren: React.ReactNode | undefined,
  metadataContext: UiJsxCanvasContextData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
): React.ReactElement {
  if (codeError != null) {
    throw codeError
  }
  if (isJSXElement(element) && isSceneElement(element)) {
    const sceneProps = jsxAttributesToProps(
      inScope,
      element.props,
      parentComponentInputProps,
      requireResult,
      (error) => {
        reportError(error)
        throw error
      },
    )

    const rootComponent = sceneProps.component
    const rootComponentName = sceneProps.component?.topLevelElementName
    const resizesContent = Utils.path(PP.getElements(PathForResizeContent), sceneProps) ?? false

    const sceneId: string = sceneProps['data-uid'] || ''
    return (
      <SceneRoot
        content={rootComponent} // this is the child component
        componentProps={sceneProps.props}
        container={sceneProps.layout}
        hiddenInstances={hiddenInstances}
        jsxFactoryFunctionName={jsxFactoryFunctionName}
        fileBlobs={fileBlobs}
        style={sceneProps.style}
        inScope={inScope}
        reportError={Utils.NO_OP}
        requireResult={requireResult}
        templatePath={templatePath}
        component={rootComponentName}
        sceneResizesContent={resizesContent}
        sceneUID={sceneId}
        sceneLabel={sceneProps['data-label']}
      />
    )
  }
  switch (element.type) {
    case 'JSX_ELEMENT': {
      const assembledProps = jsxAttributesToProps(
        inScope,
        element.props,
        parentComponentInputProps,
        requireResult,
        (error) => {
          reportError(error)
          throw error
        },
      )

      const passthroughProps: MapLike<any> = {
        ...assembledProps,
        'data-uid': Utils.defaultIfNull(assembledProps['data-uid'], uid),
        [UTOPIA_ORIGINAL_ID_KEY]: Utils.defaultIfNull(
          assembledProps[UTOPIA_ORIGINAL_ID_KEY],
          parentComponentInputProps[UTOPIA_ORIGINAL_ID_KEY],
        ),
      }
      return renderJSXElement(
        TP.toString(templatePath),
        element,
        templatePath,
        parentComponentInputProps,
        requireResult,
        rootScope,
        inScope,
        hiddenInstances,
        fileBlobs,
        validPaths,
        reportError,
        passthroughProps,
        metadataContext,
        jsxFactoryFunctionName,
        null,
        shouldIncludeCanvasRootInTheSpy,
      )
    }
    case 'JSX_ARBITRARY_BLOCK': {
      let innerIndex: number = 0
      function innerRender(
        innerElement: JSXElement,
        innerInScope: MapLike<any>,
      ): React.ReactElement {
        innerIndex++
        const innerPath = TP.appendToPath(templatePath, `index-${innerIndex}`)

        const innerUID = getUtopiaID(innerElement)
        const withOriginalID = setJSXValueAtPath(
          innerElement.props,
          PP.create([UTOPIA_ORIGINAL_ID_KEY]),
          jsxAttributeValue(innerUID),
        )
        const generatedUID = `${innerUID}-${innerIndex}`
        const withGeneratedUID = flatMapEither(
          (attrs) =>
            setJSXValueAtPath(attrs, PP.create(['data-uid']), jsxAttributeValue(generatedUID)),
          withOriginalID,
        )

        let augmentedInnerElement = innerElement
        forEachRight(withGeneratedUID, (attrs) => {
          augmentedInnerElement = {
            ...augmentedInnerElement,
            props: attrs,
          }
        })
        return renderCoreElement(
          augmentedInnerElement,
          innerPath,
          rootScope,
          innerInScope,
          parentComponentInputProps,
          requireResult,
          hiddenInstances,
          fileBlobs,
          reportError,
          validPaths,
          generatedUID,
          reactChildren,
          metadataContext,
          jsxFactoryFunctionName,
          null,
          shouldIncludeCanvasRootInTheSpy,
        )
      }
      const blockScope = {
        ...inScope,
        [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup(
          element.elementsWithin,
          inScope,
          innerRender,
        ),
      }
      return runJSXArbitraryBlock(requireResult, element, blockScope, reportError)
    }
    case 'JSX_FRAGMENT': {
      let renderedChildren: Array<React.ReactElement> = []
      fastForEach(element.children, (child) => {
        const renderResult = renderCoreElement(
          child,
          templatePath,
          rootScope,
          inScope,
          parentComponentInputProps,
          requireResult,
          hiddenInstances,
          fileBlobs,
          reportError,
          validPaths,
          uid,
          reactChildren,
          metadataContext,
          jsxFactoryFunctionName,
          codeError,
          shouldIncludeCanvasRootInTheSpy,
        )
        renderedChildren.push(renderResult)
      })
      return <>{renderedChildren}</>
    }
    case 'JSX_TEXT_BLOCK': {
      // JSXTextBlock is the final remaining case.
      return renderComponentUsingJsxFactoryFunction(
        inScope,
        jsxFactoryFunctionName,
        React.Fragment,
        { key: TP.toString(templatePath) },
        element.text,
      )
    }
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled type ${JSON.stringify(element)}`)
  }
}

function createMissingElement(jsx: JSXElement): React.ReactElement {
  return (
    <FlexRow
      style={{
        padding: '2px 4px',
        border: `1px solid ${UtopiaTheme.color.subduedBorder}`,
        borderRadius: 1,
        backgroundColor: 'white',
        color: UtopiaTheme.color.secondaryForeground.value,
        flexShrink: 0,
      }}
    >
      <WarningIcon color='gray' tooltipText='Missing element' />
      <span style={{ marginLeft: 4 }}>Can't find {getJSXElementNameAsString(jsx.name)}</span>
    </FlexRow>
  )
}

function buildSpyWrappedElement(
  jsx: JSXElement,
  finalProps: any,
  templatePath: InstancePath,
  metadataContext: UiJsxCanvasContextData,
  childrenTemplatePaths: Array<InstancePath>,
  childrenElements: Array<React.ReactNode>,
  Element: any,
  inScope: MapLike<any>,
  jsxFactoryFunctionName: string | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
): React.ReactElement {
  const props = {
    ...finalProps,
    key: TP.toComponentId(templatePath),
  }
  const childrenElementsOrNull = childrenElements.length > 0 ? childrenElements : null
  const spyCallback = (reportedProps: any) => {
    const instanceMetadata: MetadataWithoutChildren = {
      element: right(jsx),
      templatePath: templatePath,
      props: makeCanvasElementPropsSafe(reportedProps),
      globalFrame: null,
      localFrame: null,
      childrenTemplatePaths: childrenTemplatePaths,
      componentInstance: false,
      specialSizeMeasurements: emptySpecialSizeMeasurements, // This is not the nicest, but the results from the DOM walker will override this anyways
      computedStyle: emptyComputedStyle,
    }
    const isChildOfRootScene = TP.pathsEqual(
      TP.scenePathForPath(templatePath),
      EmptyScenePathForStoryboard,
    )
    if (!isChildOfRootScene || shouldIncludeCanvasRootInTheSpy) {
      metadataContext.current.spyValues.metadata[TP.toComponentId(templatePath)] = instanceMetadata
    }
  }
  const spyWrapperProps: SpyWrapperProps = {
    elementToRender: Element,
    spyCallback: spyCallback,
    inScope: inScope,
    jsxFactoryFunctionName: jsxFactoryFunctionName,
  }
  return renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    SpyWrapper,
    {
      ...props,
      ...spyWrapperProps,
    },
    childrenElementsOrNull,
  )
}

function getElementFromScope(jsxElementToLookup: JSXElement, scope: MapLike<any> | null): any {
  if (scope == null) {
    return undefined
  } else {
    // TODO SCENES remove this when the Scene metadata work is finished
    // this is now needed, otherwise the Storyboard needs to be imported to the ui js file, but the linter will show warnings
    if (jsxElementToLookup.name.baseVariable === 'Storyboard') {
      return Storyboard
    } else if (jsxElementToLookup.name.baseVariable in scope) {
      const fromVar = scope[jsxElementToLookup.name.baseVariable]
      const result = Utils.pathOr(
        undefined,
        PP.getElements(jsxElementToLookup.name.propertyPath),
        fromVar,
      )
      return result
    } else {
      return undefined
    }
  }
}

function filterDataProps(props: MapLike<any>): MapLike<any> {
  return omitWithPredicate(props, (key) => typeof key === 'string' && key.startsWith('data-'))
}

function renderJSXElement(
  key: string,
  jsx: JSXElement,
  templatePath: InstancePath,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  rootScope: MapLike<any>,
  inScope: MapLike<any>,
  hiddenInstances: Array<TemplatePath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Array<InstancePath>,
  reportError: (error: Error, errorInfo?: React.ErrorInfo) => void,
  passthroughProps: MapLike<any>,
  metadataContext: UiJsxCanvasContextData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
): React.ReactElement {
  let elementProps = { key: key, ...passthroughProps }
  if (isHidden(hiddenInstances, templatePath)) {
    elementProps = hideElement(elementProps)
  }
  elementProps = streamlineInFileBlobs(elementProps, fileBlobs)

  const createChildrenElement = (
    child: JSXElementChild,
  ): React.ReactElement | Array<React.ReactElement> => {
    const childPath = TP.appendToPath(templatePath, getUtopiaID(child))
    return renderCoreElement(
      child,
      childPath,
      rootScope,
      inScope,
      parentComponentInputProps,
      requireResult,
      hiddenInstances,
      fileBlobs,
      reportError,
      validPaths,
      undefined,
      undefined,
      metadataContext,
      jsxFactoryFunctionName,
      codeError,
      shouldIncludeCanvasRootInTheSpy,
    )
  }

  const childrenElements = jsx.children.map(createChildrenElement)
  const ElementInScope = getElementFromScope(jsx, inScope)
  const ElementFromImport = getElementFromScope(jsx, requireResult)
  const ElementFromScopeOrImport = Utils.defaultIfNull(ElementFromImport, ElementInScope)
  const elementIsIntrinsic = ElementFromScopeOrImport == null && isIntrinsicElement(jsx.name)
  const elementIsBaseHTML = elementIsIntrinsic && isIntrinsicHTMLElement(jsx.name)
  const FinalElement = elementIsIntrinsic ? jsx.name.baseVariable : ElementFromScopeOrImport
  const finalProps =
    elementIsIntrinsic && !elementIsBaseHTML ? filterDataProps(elementProps) : elementProps

  if (FinalElement == null) {
    return createMissingElement(jsx)
  } else if (TP.containsPath(templatePath, validPaths)) {
    let childrenTemplatePaths: InstancePath[] = []

    Utils.fastForEach(jsx.children, (child) => {
      if (isJSXElement(child)) {
        const childPath = TP.appendToPath(templatePath, getUtopiaID(child))
        if (TP.containsPath(childPath, validPaths)) {
          childrenTemplatePaths.push(childPath)
        }
      }
    })

    return buildSpyWrappedElement(
      jsx,
      finalProps,
      templatePath,
      metadataContext,
      childrenTemplatePaths,
      childrenElements,
      FinalElement,
      inScope,
      jsxFactoryFunctionName,
      shouldIncludeCanvasRootInTheSpy,
    )
  } else {
    const childrenOrNull = childrenElements.length !== 0 ? childrenElements : null
    return renderComponentUsingJsxFactoryFunction(
      inScope,
      jsxFactoryFunctionName,
      FinalElement,
      finalProps,
      childrenOrNull,
    )
  }
}

export interface CanvasContainerProps {
  walkDOM: boolean
  scale: number
  offset: CanvasVector
  onDomReport: (elementMetadata: Array<ElementInstanceMetadata>) => void
  codeError: Error | null
  canvasRootElementTemplatePath: TemplatePath
  validRootPaths: Array<StaticInstancePath>
}

const CanvasContainer: React.FunctionComponent<React.PropsWithChildren<CanvasContainerProps>> = (
  props: React.PropsWithChildren<CanvasContainerProps>,
) => {
  // eslint-disable-next-line react-hooks/rules-of-hooks
  let containerRef = props.walkDOM ? useDomWalker(props) : React.useRef<HTMLDivElement>(null)

  if (props.codeError != null) {
    throw props.codeError
  }

  const { scale, offset } = props
  return (
    <div
      id={'canvas-container'}
      key={'canvas-container'}
      ref={containerRef}
      style={{
        all: 'initial',
        position: 'absolute',
        zoom: scale >= 1 ? `${scale * 100}%` : 1,
        transform:
          (scale < 1 ? `scale(${scale})` : '') + ` translate3d(${offset.x}px, ${offset.y}px, 0)`,
      }}
    >
      {props.children}
    </div>
  )
}

interface SpyWrapperProps {
  spyCallback: (finalProps: any) => void
  elementToRender: React.ComponentType<any>
  inScope: MapLike<any>
  jsxFactoryFunctionName: string | null
}
const SpyWrapper: React.FunctionComponent<SpyWrapperProps> = (props) => {
  const {
    spyCallback,
    elementToRender: ElementToRender,
    inScope,
    jsxFactoryFunctionName,
    ...passThroughProps
  } = props
  spyCallback(passThroughProps)
  return renderComponentUsingJsxFactoryFunction(
    inScope,
    jsxFactoryFunctionName,
    ElementToRender,
    passThroughProps,
  )
}

interface CanvasErrorBoundaryProps extends CanvasReactReportErrorCallback {
  uiFilePath: string
}

function isErrorObject(e: unknown): e is Error {
  return typeof e === 'object' && e != null && 'name' in e && 'message' in e
}

function asErrorObject(e: unknown): Error {
  // Required because JS supports throwing anything at all
  if (isErrorObject(e)) {
    return e
  } else if (typeof e === 'string') {
    return new Error(e)
  } else {
    return new Error(JSON.stringify(e))
  }
}

class CanvasErrorBoundary extends React.PureComponent<CanvasErrorBoundaryProps, {}> {
  componentDidCatch(error: unknown, errorInfo: React.ErrorInfo) {
    this.props.reportError(this.props.uiFilePath, asErrorObject(error), errorInfo)
  }

  render() {
    return this.props.children
  }
}
