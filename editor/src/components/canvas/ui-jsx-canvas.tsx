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
import { SetValueCallback, useDomWalker } from './dom-walker'
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
import { ProjectContentTreeRoot, getContentsTreeFileFromString } from '../assets'
import { createExecutionScope } from './ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'
import { getParseSuccessOrTransientForFilePath, getValidElementPaths } from './canvas-utils'
import { NO_OP } from '../../core/shared/utils'
import { useTwind } from '../../core/tailwind/tailwind'

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

export const DomWalkerInvalidateScenesContext = React.createContext<SetValueCallback<Set<string>>>(
  NO_OP,
)
export type DomWalkerInvalidatePathsContextData = SetValueCallback<Set<string>>
export const DomWalkerInvalidatePathsContext = React.createContext<
  DomWalkerInvalidatePathsContextData
>(NO_OP)

export interface UiJsxCanvasProps {
  offset: CanvasVector
  scale: number
  uiFilePath: string
  selectedViews: Array<ElementPath>
  requireFn: UtopiaRequireFn
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
  hiddenInstances: ElementPath[]
  editedTextElement: ElementPath | null
  base64FileBlobs: CanvasBase64Blobs
  mountCount: number
  domWalkerInvalidateCount: number
  onDomReport: (
    elementMetadata: ReadonlyArray<ElementInstanceMetadata>,
    cachedPaths: Array<ElementPath>,
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
    cachedPaths: Array<ElementPath>,
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
      uiFilePath: uiFilePath,
      selectedViews: editor.selectedViews,
      requireFn: requireFn,
      resolve: editor.codeResultCache.resolve,
      hiddenInstances: hiddenInstances,
      editedTextElement: editedTextElement,
      base64FileBlobs: editor.canvas.base64Blobs,
      mountCount: editor.canvas.mountCount,
      domWalkerInvalidateCount: editor.canvas.domWalkerInvalidateCount,
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

function useClearSpyMetadataOnRemount(
  canvasMountCount: number,
  domWalkerInvalidateCount: number,
  metadataContext: UiJsxCanvasContextData,
) {
  const canvasMountCountRef = React.useRef(canvasMountCount)
  const domWalkerInvalidateCountRef = React.useRef(domWalkerInvalidateCount)

  const invalidated =
    canvasMountCountRef.current !== canvasMountCount ||
    domWalkerInvalidateCountRef.current !== domWalkerInvalidateCount

  if (invalidated) {
    metadataContext.current.spyValues.metadata = {}
  }

  canvasMountCountRef.current = canvasMountCount
  domWalkerInvalidateCountRef.current = domWalkerInvalidateCount
}

export const UiJsxCanvas = betterReactMemo(
  'UiJsxCanvas',
  React.forwardRef<HTMLDivElement, UiJsxCanvasPropsWithErrorCallback>((props, ref) => {
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

    if (clearErrors != null) {
      // a new canvas render, a new chance at having no errors
      clearErrors()
    }

    let metadataContext: UiJsxCanvasContextData = React.useContext(UiJsxCanvasContext)
    const updateInvalidatedPaths: DomWalkerInvalidatePathsContextData = React.useContext(
      DomWalkerInvalidatePathsContext,
    )
    useClearSpyMetadataOnRemount(props.mountCount, props.domWalkerInvalidateCount, metadataContext)

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

    // TODO after merge requireFn can never be null
    if (requireFn == null) {
      throw new Error('Utopia Internal Error: requireFn can never be null')
    } else {
      let resolvedFiles: MapLike<Array<string>> = {} // Mapping from importOrigin to an array of toImport
      const customRequire = React.useCallback(
        (importOrigin: string, toImport: string) => {
          if (resolvedFiles[importOrigin] == null) {
            resolvedFiles[importOrigin] = []
          }
          let resolvedFromThisOrigin = resolvedFiles[importOrigin]

          const alreadyResolved = resolvedFromThisOrigin.includes(toImport) // We're inside a cyclic dependency, so trigger the below fallback
          const filePathResolveResult = alreadyResolved
            ? left<string, string>('Already resolved')
            : resolve(importOrigin, toImport)

          const resolvedParseSuccess: Either<string, MapLike<any>> = flatMapEither(
            (resolvedFilePath) => {
              resolvedFromThisOrigin.push(toImport)
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
                  updateInvalidatedPaths,
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
          updateInvalidatedPaths,
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
        updateInvalidatedPaths,
        props.shouldIncludeCanvasRootInTheSpy,
      )

      const executionScope = scope

      useTwind(projectContents, customRequire, '#canvas-container')

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
        transientFilesState,
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
                  ref={ref}
                  mountCount={props.mountCount}
                  domWalkerInvalidateCount={props.domWalkerInvalidateCount}
                  walkDOM={walkDOM}
                  selectedViews={props.selectedViews}
                  scale={scale}
                  offset={offset}
                  onDomReport={onDomReport}
                  validRootPaths={rootValidPaths}
                  canvasRootElementElementPath={storyboardRootElementPath}
                  scrollAnimation={props.scrollAnimation}
                  canvasInteractionHappening={props.transientFilesState != null}
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
    }
  }),
)

function useGetStoryboardRoot(
  focusedElementPath: ElementPath | null,
  topLevelElementsMap: Map<string, UtopiaJSXComponent>,
  executionScope: MapLike<any>,
  projectContents: ProjectContentTreeRoot,
  uiFilePath: string,
  transientFilesState: TransientFilesState | null,
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
          transientFilesState,
          resolve,
        )
  const storyboardRootElementPath = useKeepReferenceEqualityIfPossible(
    validPaths[0] ?? EP.emptyElementPath,
  )

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
    cachedPaths: Array<ElementPath>,
  ) => void
  canvasRootElementElementPath: ElementPath
  validRootPaths: Array<ElementPath>
  mountCount: number
  domWalkerInvalidateCount: number
  scrollAnimation: boolean
  canvasInteractionHappening: boolean
}

const CanvasContainer = React.forwardRef<
  HTMLDivElement,
  React.PropsWithChildren<CanvasContainerProps>
>((props, ref) => {
  return (
    <div
      id={CanvasContainerID}
      key={'canvas-container'}
      ref={ref}
      style={{
        all: 'initial',
        position: 'absolute',
      }}
      data-utopia-valid-paths={props.validRootPaths.map(EP.toString).join(' ')}
      data-utopia-root-element-path={EP.toString(props.canvasRootElementElementPath)}
    >
      {props.children}
    </div>
  )
})
CanvasContainer.displayName = 'CanvasContainer'
