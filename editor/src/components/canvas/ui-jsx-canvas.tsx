import * as React from 'react'
import { MapLike } from 'typescript'
// Inject the babel helpers into the global scope
import '../../bundled-dependencies/babelHelpers'
import * as TP from '../../core/shared/template-path'
import {
  ArbitraryJSBlock,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  isUtopiaJSXComponent,
  TopLevelElement,
  UtopiaJSXComponent,
  ComponentMetadataWithoutRootElements,
} from '../../core/shared/element-template'
import { getValidTemplatePaths } from '../../core/model/element-template-utils'
import {
  Imports,
  InstancePath,
  ScenePath,
  TemplatePath,
  isParseSuccess,
  StaticInstancePath,
} from '../../core/shared/project-file-types'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'
import { isRight } from '../../core/shared/either'
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
import { isLiveMode } from '../editor/editor-modes'
import {
  BakedInStoryboardVariableName,
  EmptyScenePathForStoryboard,
} from '../../core/model/scene-utils'
import { EditorDispatch } from '../editor/action-types'
import { usePrevious } from '../editor/hook-utils'
import { arrayEquals, fastForEach } from '../../core/shared/utils'
import { removeAll } from '../../core/shared/array-utils'
import { normalizeName } from '../custom-code/custom-code-utils'
import { getGeneratedExternalLinkText } from '../../printer-parsers/html/external-resources-parser'
import { Helmet } from 'react-helmet'
import parse from 'html-react-parser'
import {
  ComponentRendererComponent,
  createComponentRendererComponent,
} from './ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import {
  MutableUtopiaContext,
  MutableUtopiaContextProps,
  ParentLevelUtopiaContext,
  RerenderUtopiaContext,
  SceneLevelUtopiaContext,
  updateMutableUtopiaContextWithNewProps,
} from './ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import { runBlockUpdatingScope } from './ui-jsx-canvas-renderer/ui-jsx-canvas-scope-utils'
import { CanvasContainerID } from './canvas-types'
import { betterReactMemo, useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import { unimportAllButTheseCSSFiles } from '../../core/webpack-loaders/css-loader'
import { useSelectAndHover } from './controls/select-mode/select-mode-hooks'

const emptyFileBlobs: UIFileBase64Blobs = {}

export type SpyValues = {
  metadata: ElementInstanceMetadataMap
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
  uiFileCode: string
  uiFilePath: string
  requireFn: UtopiaRequireFn | null
  hiddenInstances: TemplatePath[]
  editedTextElement: InstancePath | null
  fileBlobs: UIFileBase64Blobs
  mountCount: number
  onDomReport: (elementMetadata: Array<ElementInstanceMetadata>) => void
  walkDOM: boolean
  imports: Imports
  topLevelElementsIncludingScenes: Array<TopLevelElement>
  jsxFactoryFunction: string | null
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean // FOR ui-jsx-canvas.spec TESTS ONLY!!!! this prevents us from having to update the legacy test snapshots
  clearConsoleLogs: () => void
  addToConsoleLogs: (log: ConsoleLog) => void
  linkTags: string
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null
}

export interface CanvasReactReportErrorCallback {
  reportError: (editedFile: string, error: Error, errorInfo?: React.ErrorInfo) => void
}

export interface CanvasReactClearErrorsCallback {
  clearErrors: () => void
}

export type CanvasReactErrorCallback = CanvasReactReportErrorCallback &
  CanvasReactClearErrorsCallback

export type UiJsxCanvasPropsWithErrorCallback = UiJsxCanvasProps & CanvasReactClearErrorsCallback

const emptyImports: Imports = {}
const emptyTopLevelElements: Array<TopLevelElement> = []

export function pickUiJsxCanvasProps(
  editor: EditorState,
  derived: DerivedState,
  walkDOM: boolean,
  onDomReport: (elementMetadata: Array<ElementInstanceMetadata>) => void,
  clearConsoleLogs: () => void,
  addToConsoleLogs: (log: ConsoleLog) => void,
  dispatch: EditorDispatch,
): UiJsxCanvasProps | null {
  const uiFile = getOpenUIJSFile(editor)
  const uiFilePath = getOpenUIJSFileKey(editor)
  if (uiFile == null || uiFilePath == null) {
    return null
  } else {
    const defaultedFileBlobs = Utils.defaultIfNull(
      emptyFileBlobs,
      Utils.optionalFlatMap((key) => editor.canvas.base64Blobs[key], getOpenUIJSFileKey(editor)),
    )

    let imports: Imports = emptyImports
    let topLevelElementsIncludingScenes: Array<TopLevelElement> = emptyTopLevelElements
    let jsxFactoryFunction: string | null = null
    let combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null = null

    if (uiFile != null && isParseSuccess(uiFile.fileContents.parsed)) {
      const success = uiFile.fileContents.parsed
      const transientCanvasState = derived.canvas.transientState
      imports = uiFile.fileContents.parsed.imports
      topLevelElementsIncludingScenes = success.topLevelElements
      jsxFactoryFunction = success.jsxFactoryFunction
      combinedTopLevelArbitraryBlock = success.combinedTopLevelArbitraryBlock
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
      const parsedLinkTags = getGeneratedExternalLinkText(indexHtml.value.fileContents.code)
      if (isRight(parsedLinkTags)) {
        linkTags = parsedLinkTags.value
      }
    }

    const editedTextElement = Utils.optionalMap(
      (textEd) => textEd.templatePath,
      editor.canvas.textEditor,
    )

    let hiddenInstances = editor.hiddenInstances
    if (editedTextElement != null) {
      hiddenInstances = [...hiddenInstances, editedTextElement]
    }
    return {
      offset: editor.canvas.roundedCanvasOffset,
      scale: editor.canvas.scale,
      uiFileCode: uiFile.fileContents.code,
      uiFilePath: uiFilePath,
      requireFn: requireFn,
      hiddenInstances: hiddenInstances,
      editedTextElement: editedTextElement,
      fileBlobs: defaultedFileBlobs,
      mountCount: editor.canvas.mountCount,
      onDomReport: onDomReport,
      walkDOM: walkDOM,
      imports: imports,
      topLevelElementsIncludingScenes: topLevelElementsIncludingScenes,
      jsxFactoryFunction: jsxFactoryFunction,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      canvasIsLive: isLiveMode(editor.mode),
      shouldIncludeCanvasRootInTheSpy: true,
      linkTags: linkTags,
      combinedTopLevelArbitraryBlock: combinedTopLevelArbitraryBlock,
    }
  }
}

function normalizedCssImportsFromImports(filePath: string, imports: Imports): Array<string> {
  let result: Array<string> = []
  Utils.fastForEach(Object.keys(imports), (importSource) => {
    if (importSource.endsWith('.css')) {
      result.push(normalizeName(filePath, importSource))
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
      onDomReport,
      topLevelElementsIncludingScenes,
      imports,
      jsxFactoryFunction,
      clearErrors,
      clearConsoleLogs,
      addToConsoleLogs,
      canvasIsLive,
      linkTags,
      combinedTopLevelArbitraryBlock,
    } = props

    // FIXME This is illegal! The two lines below are triggering a re-render
    clearConsoleLogs()
    proxyConsole(console, addToConsoleLogs)

    let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)

    // Handle the imports changing, this needs to run _before_ any require function
    // calls as it's modifying the underlying DOM elements. This is somewhat working
    // like useEffect, except that runs after everything has rendered.
    const cssImports = useKeepReferenceEqualityIfPossible(
      normalizedCssImportsFromImports(uiFilePath, imports),
    )
    unimportAllButTheseCSSFiles(cssImports)

    let topLevelComponentRendererComponents = React.useRef<MapLike<ComponentRendererComponent>>({})

    let mutableContextRef = React.useRef<MutableUtopiaContextProps>({
      fileBlobs: fileBlobs,
      requireResult: {},
      rootScope: {},
      jsxFactoryFunctionName: null,
    })

    if (clearErrors != null) {
      // a new canvas render, a new chance at having no errors
      // FIXME This is illegal! The line below is triggering a re-render
      clearErrors()
    }

    if (requireFn != null) {
      const customRequire = React.useCallback(
        (importOrigin: string, toImport: string) => requireFn(importOrigin, toImport, false),
        [requireFn],
      )

      const requireResult: MapLike<any> = importResultFromImports(
        uiFilePath,
        imports,
        customRequire,
      )

      const userRequireFn = React.useCallback(
        (toImport: string) => customRequire(uiFilePath, toImport),
        [uiFilePath, customRequire],
      )
      let executionScope: MapLike<any> = { require: userRequireFn, ...requireResult }
      // TODO All of this is run on every interaction o_O

      let topLevelJsxComponents: Map<string, UtopiaJSXComponent> = new Map()

      // Make sure there is something in scope for all of the top level components
      Utils.fastForEach(topLevelElementsIncludingScenes, (topLevelElement) => {
        if (isUtopiaJSXComponent(topLevelElement)) {
          topLevelJsxComponents.set(topLevelElement.name, topLevelElement)
          if (!(topLevelElement.name in topLevelComponentRendererComponents.current)) {
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
      if (combinedTopLevelArbitraryBlock != null) {
        runBlockUpdatingScope(requireResult, combinedTopLevelArbitraryBlock, executionScope)
      }

      updateMutableUtopiaContextWithNewProps(mutableContextRef, {
        requireResult: requireResult,
        rootScope: executionScope,
        fileBlobs: fileBlobs,
        jsxFactoryFunctionName: jsxFactoryFunction,
      })

      const topLevelElementsMap = new Map(topLevelJsxComponents)

      const {
        StoryboardRootComponent,
        rootValidPaths,
        storyboardRootElementPath,
        storyboardRootSceneMetadata,
        rootScenePath,
      } = useGetStoryboardRoot(topLevelElementsMap, executionScope)

      if (props.shouldIncludeCanvasRootInTheSpy) {
        metadataContext.current.spyValues.scenes[
          TP.toString(rootScenePath)
        ] = storyboardRootSceneMetadata
      }

      return (
        <>
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
                mountCount={props.mountCount}
                walkDOM={walkDOM}
                scale={scale}
                offset={offset}
                onDomReport={onDomReport}
                validRootPaths={rootValidPaths}
                canvasRootElementTemplatePath={storyboardRootElementPath}
              >
                <SceneLevelUtopiaContext.Provider
                  value={{ validPaths: rootValidPaths, scenePath: rootScenePath }}
                >
                  <ParentLevelUtopiaContext.Provider
                    value={{
                      templatePath: storyboardRootElementPath,
                    }}
                  >
                    {StoryboardRootComponent == null ? null : <StoryboardRootComponent />}
                  </ParentLevelUtopiaContext.Provider>
                </SceneLevelUtopiaContext.Provider>
              </CanvasContainer>
            </RerenderUtopiaContext.Provider>
          </MutableUtopiaContext.Provider>
        </>
      )
    } else {
      return null
    }
  },
)

function useGetStoryboardRoot(
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
  const storyboardRootElementPath = useKeepReferenceEqualityIfPossible(validPaths[0]) // >:D

  const storyboardRootSceneMetadata: ComponentMetadataWithoutRootElements = {
    component: BakedInStoryboardVariableName,
    sceneResizesContent: false,
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

export interface CanvasContainerProps {
  walkDOM: boolean
  scale: number
  offset: CanvasVector
  onDomReport: (elementMetadata: Array<ElementInstanceMetadata>) => void
  canvasRootElementTemplatePath: TemplatePath
  validRootPaths: Array<StaticInstancePath>
  mountCount: number
}

const CanvasContainer: React.FunctionComponent<React.PropsWithChildren<CanvasContainerProps>> = (
  props: React.PropsWithChildren<CanvasContainerProps>,
) => {
  // eslint-disable-next-line react-hooks/rules-of-hooks
  let containerRef = props.walkDOM ? useDomWalker(props) : React.useRef<HTMLDivElement>(null)

  const { scale, offset } = props
  return (
    <div
      id={CanvasContainerID}
      key={'canvas-container'}
      ref={containerRef}
      style={{
        all: 'initial',
        position: 'absolute',
        zoom: scale >= 1 ? `${scale * 100}%` : 1,
        transform:
          (scale < 1 ? `scale(${scale})` : '') + ` translate3d(${offset.x}px, ${offset.y}px, 0)`,
      }}
      data-utopia-valid-paths={props.validRootPaths.map(TP.toString).join(' ')}
    >
      {props.children}
    </div>
  )
}
CanvasContainer.displayName = 'CanvasContainer'
