import * as React from 'react'
import { MapLike } from 'typescript'
// Inject the babel helpers into the global scope
import '../../bundled-dependencies/babelHelpers'
import * as EP from '../../core/shared/element-path'
import {
  ArbitraryJSBlock,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  isUtopiaJSXComponent,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import { getValidElementPaths } from '../../core/model/element-template-utils'
import {
  Imports,
  ElementPath,
  isParseSuccess,
  isTextFile,
} from '../../core/shared/project-file-types'
import {
  Either,
  flatMapEither,
  foldEither,
  isRight,
  left,
  mapEither,
  right,
} from '../../core/shared/either'
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
  CanvasBase64Blobs,
  TransientFilesState,
} from '../editor/store/editor-state'
import { proxyConsole } from './console-proxy'
import { useDomWalker } from './dom-walker'
import { isLiveMode } from '../editor/editor-modes'
import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
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
  UtopiaProjectContext,
} from './ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import { runBlockUpdatingScope } from './ui-jsx-canvas-renderer/ui-jsx-canvas-scope-utils'
import { CanvasContainerID } from './canvas-types'
import { betterReactMemo, useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import { unimportAllButTheseCSSFiles } from '../../core/webpack-loaders/css-loader'
import { useSelectAndHover } from './controls/select-mode/select-mode-hooks'
import { UTOPIA_INSTANCE_PATH, UTOPIA_PATHS_KEY } from '../../core/model/utopia-constants'
import {
  createLookupRender,
  utopiaCanvasJSXLookup,
} from './ui-jsx-canvas-renderer/ui-jsx-canvas-element-renderer-utils'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../core/workers/parser-printer/parser-printer-utils'
import { getParseSuccessOrTransientForFilePath } from './ui-jsx-canvas-renderer/ui-jsx-canvas-top-level-elements'
import { ProjectContentTreeRoot, getContentsTreeFileFromString } from '../assets'
import { createExecutionScope } from './ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'

applyUIDMonkeyPatch()

const emptyFileBlobs: UIFileBase64Blobs = {}

export type SpyValues = {
  metadata: ElementInstanceMetadataMap
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
  selectedViews: Array<ElementPath>
  requireFn: UtopiaRequireFn
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
  hiddenInstances: ElementPath[]
  editedTextElement: ElementPath | null
  base64FileBlobs: CanvasBase64Blobs
  mountCount: number
  onDomReport: (
    elementMetadata: ReadonlyArray<ElementInstanceMetadata>,
    cachedTreeRoots: Array<ElementPath>,
  ) => void
  walkDOM: boolean
  imports_KILLME: Imports // FIXME this is the storyboard imports object used only for the cssimport
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean // FOR ui-jsx-canvas.spec TESTS ONLY!!!! this prevents us from having to update the legacy test snapshots
  clearConsoleLogs: () => void
  addToConsoleLogs: (log: ConsoleLog) => void
  linkTags: string
  focusedElementPath: ElementPath | null
  projectContents: ProjectContentTreeRoot
  transientFilesState: TransientFilesState | null
  scrollAnimation: boolean
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

export function pickUiJsxCanvasProps(
  editor: EditorState,
  derived: DerivedState,
  walkDOM: boolean,
  onDomReport: (
    elementMetadata: ReadonlyArray<ElementInstanceMetadata>,
    cachedTreeRoots: Array<ElementPath>,
  ) => void,
  clearConsoleLogs: () => void,
  addToConsoleLogs: (log: ConsoleLog) => void,
): UiJsxCanvasProps | null {
  const uiFile = getOpenUIJSFile(editor)
  const uiFilePath = getOpenUIJSFileKey(editor)
  if (uiFile == null || uiFilePath == null) {
    return null
  } else {
    const { imports: imports_KILLME } = getParseSuccessOrTransientForFilePath(
      uiFilePath,
      editor.projectContents,
      derived.canvas.transientState.filesState,
    )

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
      (textEd) => textEd.elementPath,
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
      selectedViews: editor.selectedViews,
      requireFn: requireFn,
      resolve: editor.codeResultCache.resolve,
      hiddenInstances: hiddenInstances,
      editedTextElement: editedTextElement,
      base64FileBlobs: editor.canvas.base64Blobs,
      mountCount: editor.canvas.mountCount,
      onDomReport: onDomReport,
      walkDOM: walkDOM,
      imports_KILLME: imports_KILLME,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      canvasIsLive: isLiveMode(editor.mode),
      shouldIncludeCanvasRootInTheSpy: true,
      linkTags: linkTags,
      focusedElementPath: editor.focusedElementPath,
      projectContents: editor.projectContents,
      transientFilesState: derived.canvas.transientState.filesState,
      scrollAnimation: editor.canvas.scrollAnimation,
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
    const {
      offset,
      scale,
      uiFilePath,
      requireFn,
      resolve,
      hiddenInstances,
      walkDOM,
      onDomReport,
      imports_KILLME: imports, // FIXME this is the storyboard imports object used only for the cssimport
      clearErrors,
      clearConsoleLogs,
      addToConsoleLogs,
      canvasIsLive,
      linkTags,
      base64FileBlobs,
      projectContents,
      transientFilesState,
      shouldIncludeCanvasRootInTheSpy,
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
    unimportAllButTheseCSSFiles(cssImports) // TODO this needs to support more than just the storyboard file!!!!!

    let mutableContextRef = React.useRef<MutableUtopiaContextProps>({})

    let topLevelComponentRendererComponents = React.useRef<
      MapLike<MapLike<ComponentRendererComponent>>
    >({})

    if (clearErrors != null) {
      // a new canvas render, a new chance at having no errors
      // FIXME This is illegal! The line below is triggering a re-render
      clearErrors()
    }

    // TODO after merge requireFn can never be null
    if (requireFn != null) {
      let resolvedFiles: string[] = []
      const customRequire = React.useCallback(
        (importOrigin: string, toImport: string) => {
          const filePathResolveResult = resolve(importOrigin, toImport)
          const resolvedParseSuccess: Either<string, MapLike<any>> = flatMapEither(
            (resolvedFilePath) => {
              if (resolvedFiles.includes(resolvedFilePath)) {
                // We're inside a cyclic dependency, so bail and the outer call will create the execution scope
                return left('Ignoring inner cyclic dependency')
              } else {
                resolvedFiles.push(resolvedFilePath)
                const projectFile = getContentsTreeFileFromString(projectContents, resolvedFilePath)
                if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
                  const { scope } = createExecutionScope(
                    resolvedFilePath,
                    customRequire,
                    mutableContextRef,
                    topLevelComponentRendererComponents,
                    projectContents,
                    uiFilePath,
                    transientFilesState,
                    base64FileBlobs,
                    hiddenInstances,
                    metadataContext,
                    shouldIncludeCanvasRootInTheSpy,
                  )
                  const exportsDetail = projectFile.fileContents.parsed.exportsDetail
                  let filteredScope: MapLike<any> = {}
                  for (const s of Object.keys(scope)) {
                    if (s in exportsDetail.namedExports) {
                      filteredScope[s] = scope[s]
                    } else if (s === exportsDetail.defaultExport?.name) {
                      filteredScope[s] = scope[s]
                    }
                  }
                  return right(filteredScope)
                } else {
                  return left(`File ${resolvedFilePath} is not a ParseSuccess`)
                }
              }
            },
            filePathResolveResult,
          )
          return foldEither(
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
        },
        // TODO I don't like projectContents and transientFileState here because that means dragging smth on the Canvas would recreate the customRequire fn
        [
          requireFn,
          resolve,
          resolvedFiles,
          projectContents,
          transientFilesState,
          uiFilePath,
          base64FileBlobs,
          hiddenInstances,
          metadataContext,
          shouldIncludeCanvasRootInTheSpy,
        ],
      )

      const { scope, topLevelJsxComponents } = createExecutionScope(
        uiFilePath,
        customRequire,
        mutableContextRef,
        topLevelComponentRendererComponents,
        props.projectContents,
        uiFilePath, // this is the storyboard filepath
        props.transientFilesState,
        base64FileBlobs,
        hiddenInstances,
        metadataContext,
        props.shouldIncludeCanvasRootInTheSpy,
      )

      const executionScope = scope

      const topLevelElementsMap = useKeepReferenceEqualityIfPossible(new Map(topLevelJsxComponents))

      const {
        StoryboardRootComponent,
        rootValidPaths,
        storyboardRootElementPath,
        rootInstancePath,
      } = useGetStoryboardRoot(
        props.focusedElementPath,
        topLevelElementsMap,
        executionScope,
        projectContents,
        uiFilePath,
        resolve,
      )

      const sceneLevelUtopiaContextValue = useKeepReferenceEqualityIfPossible({
        validPaths: rootValidPaths,
      })

      return (
        <>
          <Helmet>{parse(linkTags)}</Helmet>
          <MutableUtopiaContext.Provider value={mutableContextRef}>
            <RerenderUtopiaContext.Provider
              value={{
                hiddenInstances: hiddenInstances,
                canvasIsLive: canvasIsLive,
                shouldIncludeCanvasRootInTheSpy: props.shouldIncludeCanvasRootInTheSpy,
              }}
            >
              <UtopiaProjectContext.Provider
                value={{
                  projectContents: props.projectContents,
                  transientFilesState: props.transientFilesState,
                  openStoryboardFilePathKILLME: props.uiFilePath,
                  resolve: props.resolve,
                }}
              >
                <CanvasContainer
                  mountCount={props.mountCount}
                  walkDOM={walkDOM}
                  selectedViews={props.selectedViews}
                  scale={scale}
                  offset={offset}
                  onDomReport={onDomReport}
                  validRootPaths={rootValidPaths}
                  canvasRootElementElementPath={storyboardRootElementPath}
                  scrollAnimation={props.scrollAnimation}
                >
                  <SceneLevelUtopiaContext.Provider value={sceneLevelUtopiaContextValue}>
                    <ParentLevelUtopiaContext.Provider
                      value={{
                        elementPath: storyboardRootElementPath,
                      }}
                    >
                      {StoryboardRootComponent == null ? null : (
                        <StoryboardRootComponent
                          {...{ [UTOPIA_INSTANCE_PATH]: rootInstancePath }}
                        />
                      )}
                    </ParentLevelUtopiaContext.Provider>
                  </SceneLevelUtopiaContext.Provider>
                </CanvasContainer>
              </UtopiaProjectContext.Provider>
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
  focusedElementPath: ElementPath | null,
  topLevelElementsMap: Map<string, UtopiaJSXComponent>,
  executionScope: MapLike<any>,
  projectContents: ProjectContentTreeRoot,
  uiFilePath: string,
  resolve: (importOrigin: string, toImport: string) => Either<string, string>,
): {
  StoryboardRootComponent: ComponentRendererComponent | undefined
  storyboardRootElementPath: ElementPath
  rootValidPaths: Array<ElementPath>
  rootInstancePath: ElementPath
} {
  const StoryboardRootComponent = executionScope[BakedInStoryboardVariableName] as
    | ComponentRendererComponent
    | undefined

  const storyboardRootJsxComponent = topLevelElementsMap.get(BakedInStoryboardVariableName)
  const validPaths =
    storyboardRootJsxComponent == null
      ? []
      : getValidElementPaths(
          focusedElementPath,
          BakedInStoryboardVariableName,
          EP.emptyElementPath,
          projectContents,
          uiFilePath,
          resolve,
        )
  const storyboardRootElementPath = useKeepReferenceEqualityIfPossible(validPaths[0]) // >:D

  return {
    StoryboardRootComponent: StoryboardRootComponent,
    storyboardRootElementPath: storyboardRootElementPath,
    rootValidPaths: validPaths,
    rootInstancePath: EP.emptyElementPath,
  }
}

export interface CanvasContainerProps {
  walkDOM: boolean
  selectedViews: Array<ElementPath>
  scale: number
  offset: CanvasVector
  onDomReport: (
    elementMetadata: ReadonlyArray<ElementInstanceMetadata>,
    cachedTreeRoots: Array<ElementPath>,
  ) => void
  canvasRootElementElementPath: ElementPath
  validRootPaths: Array<ElementPath>
  mountCount: number
  scrollAnimation: boolean
}

const CanvasContainer: React.FunctionComponent<React.PropsWithChildren<CanvasContainerProps>> = (
  props: React.PropsWithChildren<CanvasContainerProps>,
) => {
  // eslint-disable-next-line react-hooks/rules-of-hooks
  let containerRef = props.walkDOM ? useDomWalker(props) : React.useRef<HTMLDivElement>(null)

  return (
    <div
      id={CanvasContainerID}
      key={'canvas-container'}
      ref={containerRef}
      style={{
        all: 'initial',
        position: 'absolute',
      }}
      data-utopia-valid-paths={props.validRootPaths.map(EP.toString).join(' ')}
    >
      {props.children}
    </div>
  )
}
CanvasContainer.displayName = 'CanvasContainer'
