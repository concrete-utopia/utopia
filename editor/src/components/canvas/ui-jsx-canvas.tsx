import React from 'react'
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
  isReexportExportDetail,
  isExportDestructuredAssignment,
} from '../../core/shared/project-file-types'
import {
  Either,
  flatMapEither,
  foldEither,
  forEachRight,
  isRight,
  left,
  mapEither,
  right,
} from '../../core/shared/either'
import Utils from '../../utils/utils'
import { CanvasVector } from '../../core/shared/math-utils'
import {
  CurriedResolveFn,
  CurriedUtopiaRequireFn,
  PropertyControlsInfo,
  UtopiaRequireFn,
} from '../custom-code/code-file'
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
  ElementsToRerender,
  AllElementProps,
} from '../editor/store/editor-state'
import { proxyConsole } from './console-proxy'
import type { UpdateMutableCallback } from './dom-walker'
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
  MutableUtopiaCtxRefData,
  RerenderUtopiaCtxAtom,
  SceneLevelUtopiaCtxAtom,
  updateMutableUtopiaCtxRefWithNewProps,
  UtopiaProjectCtxAtom,
} from './ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import { runBlockUpdatingScope } from './ui-jsx-canvas-renderer/ui-jsx-canvas-scope-utils'
import { CanvasContainerID } from './canvas-types'
import {
  useKeepReferenceEqualityIfPossible,
  useKeepShallowReferenceEquality,
} from '../../utils/react-performance'
import { unimportAllButTheseCSSFiles } from '../../core/webpack-loaders/css-loader'
import { useSelectAndHover } from './controls/select-mode/select-mode-hooks'
import { UTOPIA_INSTANCE_PATH, UTOPIA_PATH_KEY } from '../../core/model/utopia-constants'
import {
  createLookupRender,
  utopiaCanvasJSXLookup,
} from './ui-jsx-canvas-renderer/ui-jsx-canvas-element-renderer-utils'
import { ProjectContentTreeRoot, getContentsTreeFileFromString, walkContentsTree } from '../assets'
import { createExecutionScope } from './ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'
import { getParseSuccessOrTransientForFilePath, getValidElementPaths } from './canvas-utils'
import { fastForEach, NO_OP } from '../../core/shared/utils'
import { useTwind } from '../../core/tailwind/tailwind'
import {
  AlwaysFalse,
  atomWithPubSub,
  usePubSubAtomReadOnly,
} from '../../core/shared/atom-with-pub-sub'
import { omit } from '../../core/shared/object-utils'
import { validateControlsToCheck } from './canvas-globals'
import { EditorDispatch } from '../editor/action-types'
import {
  clearListOfEvaluatedFiles,
  getListOfEvaluatedFiles,
} from '../../core/shared/code-exec-utils'
import { emptySet } from '../../core/shared/set-utils'
import { forceNotNull } from '../../core/shared/optional-utils'

applyUIDMonkeyPatch()

const emptyFileBlobs: UIFileBase64Blobs = {}

// The reason this is not in a React Context, and in a crummy global instead is that sometimes the user code
// will need to bridge across react roots that erase context
export const ElementsToRerenderGLOBAL: { current: ElementsToRerender } = {
  current: 'rerender-all-elements',
}

export type SpyValues = {
  metadata: ElementInstanceMetadataMap
  allElementProps: AllElementProps
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
        allElementProps: {},
      },
    },
  }
}

export const UiJsxCanvasCtxAtom = atomWithPubSub<UiJsxCanvasContextData>({
  key: 'UiJsxCanvasCtxAtom',
  defaultValue: emptyUiJsxCanvasContextData(),
})

export type DomWalkerInvalidatePathsCtxData = UpdateMutableCallback<Set<string>>
export const DomWalkerInvalidatePathsCtxAtom = atomWithPubSub<DomWalkerInvalidatePathsCtxData>({
  key: 'DomWalkerInvalidatePathsCtxAtom',
  defaultValue: NO_OP,
})

export interface UiJsxCanvasProps {
  uiFilePath: string
  curriedRequireFn: CurriedUtopiaRequireFn
  curriedResolveFn: CurriedResolveFn
  hiddenInstances: ElementPath[]
  editedTextElement: ElementPath | null
  base64FileBlobs: CanvasBase64Blobs
  mountCount: number
  domWalkerInvalidateCount: number
  imports_KILLME: Imports // FIXME this is the storyboard imports object used only for the cssimport
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean // FOR ui-jsx-canvas.spec TESTS ONLY!!!! this prevents us from having to update the legacy test snapshots
  clearConsoleLogs: () => void
  addToConsoleLogs: (log: ConsoleLog) => void
  linkTags: string
  focusedElementPath: ElementPath | null
  projectContents: ProjectContentTreeRoot
  transientFilesState: TransientFilesState | null
  propertyControlsInfo: PropertyControlsInfo
  dispatch: EditorDispatch
  domWalkerAdditionalElementsToUpdate: Array<ElementPath>
  elementsToRerender: Array<ElementPath> | 'rerender-all-elements'
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
  dispatch: EditorDispatch,
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
      derived.transientState.filesState,
    )

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
      uiFilePath: uiFilePath,
      curriedRequireFn: editor.codeResultCache.curriedRequireFn,
      curriedResolveFn: editor.codeResultCache.curriedResolveFn,
      hiddenInstances: hiddenInstances,
      editedTextElement: editedTextElement,
      base64FileBlobs: editor.canvas.base64Blobs,
      mountCount: editor.canvas.mountCount,
      domWalkerInvalidateCount: editor.canvas.domWalkerInvalidateCount,
      imports_KILLME: imports_KILLME,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
      canvasIsLive: isLiveMode(editor.mode),
      shouldIncludeCanvasRootInTheSpy: true,
      linkTags: linkTags,
      focusedElementPath: editor.focusedElementPath,
      projectContents: editor.projectContents,
      transientFilesState: derived.transientState.filesState,
      propertyControlsInfo: editor.propertyControlsInfo,
      dispatch: dispatch,
      domWalkerAdditionalElementsToUpdate: editor.canvas.domWalkerAdditionalElementsToUpdate,
      elementsToRerender: editor.canvas.elementsToRerender,
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

function clearSpyCollectorInvalidPaths(
  validPaths: Set<ElementPath>,
  spyCollectorContextRef: UiJsxCanvasContextData,
): void {
  const spyKeys = Object.keys(spyCollectorContextRef.current.spyValues.metadata)
  fastForEach(spyKeys, (elementPathString) => {
    const elementPath =
      spyCollectorContextRef.current.spyValues.metadata[elementPathString].elementPath
    const staticElementPath = EP.makeLastPartOfPathStatic(elementPath)
    if (!validPaths.has(staticElementPath)) {
      // we found a path that is no longer valid. let's delete it from the spy accumulator!
      delete spyCollectorContextRef.current.spyValues.metadata[elementPathString]
    }
  })
}

export const UiJsxCanvas = React.memo<UiJsxCanvasPropsWithErrorCallback>((props) => {
  const {
    uiFilePath,
    curriedRequireFn,
    curriedResolveFn,
    hiddenInstances,
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
    propertyControlsInfo,
    dispatch,
  } = props

  clearListOfEvaluatedFiles()
  let resolvedFileNames = React.useRef<Array<string>>([]) // resolved (i.e. imported) files this render
  resolvedFileNames.current = [uiFilePath]
  let evaluatedFileNames = React.useRef<Array<string>>([]) // evaluated (i.e. not using a cached evaluation) this render
  evaluatedFileNames.current = [uiFilePath]
  React.useEffect(() => {
    setTimeout(() => {
      // wrapping in a setTimeout so we don't dispatch from inside React lifecycle
      validateControlsToCheck(
        dispatch,
        propertyControlsInfo,
        resolvedFileNames.current,
        evaluatedFileNames.current,
      )
    }, 0)
  })

  // FIXME This is illegal! The two lines below are triggering a re-render
  clearConsoleLogs()
  proxyConsole(console, addToConsoleLogs)

  if (clearErrors != null) {
    // a new canvas render, a new chance at having no errors
    clearErrors()
  }

  let metadataContext: UiJsxCanvasContextData = forceNotNull(
    `Missing UiJsxCanvasCtxAtom provider`,
    usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse),
  )

  const updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData = usePubSubAtomReadOnly(
    DomWalkerInvalidatePathsCtxAtom,
    AlwaysFalse,
  )
  useClearSpyMetadataOnRemount(props.mountCount, props.domWalkerInvalidateCount, metadataContext)

  // Handle the imports changing, this needs to run _before_ any require function
  // calls as it's modifying the underlying DOM elements. This is somewhat working
  // like useEffect, except that runs after everything has rendered.
  const cssImports = useKeepReferenceEqualityIfPossible(
    normalizedCssImportsFromImports(uiFilePath, imports),
  )
  unimportAllButTheseCSSFiles(cssImports) // TODO this needs to support more than just the storyboard file!!!!!

  let mutableContextRef = React.useRef<MutableUtopiaCtxRefData>({})

  let topLevelComponentRendererComponents = React.useRef<
    MapLike<MapLike<ComponentRendererComponent>>
  >({})

  const resolve = React.useMemo(
    () => curriedResolveFn(projectContents),
    [curriedResolveFn, projectContents],
  )

  let resolvedFiles = React.useRef<MapLike<Array<string>>>({}) // Mapping from importOrigin to an array of toImport
  resolvedFiles.current = {}
  const requireFn = React.useMemo(
    () => curriedRequireFn(projectContents),
    [curriedRequireFn, projectContents],
  )
  const customRequire = React.useCallback(
    (importOrigin: string, toImport: string) => {
      if (resolvedFiles.current[importOrigin] == null) {
        resolvedFiles.current[importOrigin] = []
      }
      let resolvedFromThisOrigin = resolvedFiles.current[importOrigin]

      const alreadyResolved = resolvedFromThisOrigin.includes(toImport) // We're inside a cyclic dependency, so trigger the below fallback
      const filePathResolveResult = alreadyResolved
        ? left<string, string>('Already resolved')
        : resolve(importOrigin, toImport)

      forEachRight(filePathResolveResult, (filepath) => resolvedFileNames.current.push(filepath))

      const resolvedParseSuccess: Either<string, MapLike<any>> = attemptToResolveParsedComponents(
        resolvedFromThisOrigin,
        toImport,
        projectContents,
        customRequire,
        mutableContextRef,
        topLevelComponentRendererComponents,
        uiFilePath,
        transientFilesState,
        base64FileBlobs,
        hiddenInstances,
        metadataContext,
        updateInvalidatedPaths,
        shouldIncludeCanvasRootInTheSpy,
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

  evaluatedFileNames.current = getListOfEvaluatedFiles()

  const executionScope = scope

  useTwind(projectContents, customRequire, '#canvas-container')

  const topLevelElementsMap = useKeepReferenceEqualityIfPossible(new Map(topLevelJsxComponents))

  const {
    StoryboardRootComponent,
    rootValidPathsArray,
    rootValidPathsSet,
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

  clearSpyCollectorInvalidPaths(rootValidPathsSet, metadataContext)

  const sceneLevelUtopiaContextValue = useKeepReferenceEqualityIfPossible({
    validPaths: rootValidPathsSet,
  })

  const rerenderUtopiaContextValue = useKeepShallowReferenceEquality({
    hiddenInstances: hiddenInstances,
    canvasIsLive: canvasIsLive,
    shouldIncludeCanvasRootInTheSpy: props.shouldIncludeCanvasRootInTheSpy,
  })

  const utopiaProjectContextValue = useKeepShallowReferenceEquality({
    projectContents: props.projectContents,
    transientFilesState: props.transientFilesState,
    openStoryboardFilePathKILLME: props.uiFilePath,
    resolve: resolve,
  })

  const StoryboardRoot = React.useMemo(() => {
    return StoryboardRootComponent == null ? null : (
      <StoryboardRootComponent {...{ [UTOPIA_INSTANCE_PATH]: rootInstancePath }} />
    )
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [StoryboardRootComponent, rootInstancePath, props.domWalkerInvalidateCount, props.mountCount])

  return (
    <div
      style={{
        all: 'initial',
      }}
    >
      <Helmet>{parse(linkTags)}</Helmet>
      <RerenderUtopiaCtxAtom.Provider value={rerenderUtopiaContextValue}>
        <UtopiaProjectCtxAtom.Provider value={utopiaProjectContextValue}>
          <CanvasContainer
            validRootPaths={rootValidPathsArray}
            canvasRootElementElementPath={storyboardRootElementPath}
          >
            <SceneLevelUtopiaCtxAtom.Provider value={sceneLevelUtopiaContextValue}>
              {StoryboardRoot}
            </SceneLevelUtopiaCtxAtom.Provider>
          </CanvasContainer>
        </UtopiaProjectCtxAtom.Provider>
      </RerenderUtopiaCtxAtom.Provider>
    </div>
  )
})

function attemptToResolveParsedComponents(
  resolvedFromThisOrigin: string[],
  toImport: string,
  projectContents: ProjectContentTreeRoot,
  customRequire: (importOrigin: string, toImport: string) => any,
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
  uiFilePath: string,
  transientFilesState: TransientFilesState | null,
  base64FileBlobs: CanvasBase64Blobs,
  hiddenInstances: ElementPath[],
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: UpdateMutableCallback<Set<string>>,
  shouldIncludeCanvasRootInTheSpy: boolean,
  filePathResolveResult: Either<string, string>,
): Either<string, MapLike<any>> {
  return flatMapEither((resolvedFilePath) => {
    resolvedFromThisOrigin.push(toImport)
    const projectFile = getContentsTreeFileFromString(projectContents, resolvedFilePath)
    if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
      const exportsDetail = projectFile.fileContents.parsed.exportsDetail
      // Should only use the full scope and components support if the file contains components
      // or if it does any kind of re-exporting as we can't guarantee that the re-exported
      // files do not contain components.
      // Exclude any file with the destructured assignment export style as they're quite problematic
      // to support.
      const shouldUseFileScope =
        (projectFile.fileContents.parsed.topLevelElements.some(isUtopiaJSXComponent) ||
          exportsDetail.some(isReexportExportDetail)) &&
        !exportsDetail.some(isExportDestructuredAssignment)
      if (shouldUseFileScope) {
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
        let filteredScope: MapLike<any> = {
          ...scope.module.exports,
          __esModule: true,
        }

        function addToFilteredScopeFromSpecificScope(
          filteredScopeKey: string,
          scopeKey: string,
          scopeToWorkWith: MapLike<any>,
        ): void {
          if (scopeKey in scopeToWorkWith) {
            filteredScope[filteredScopeKey] = scopeToWorkWith[scopeKey]
          }
        }

        function addToFilteredScope(filteredScopeKey: string, scopeKey: string): void {
          addToFilteredScopeFromSpecificScope(filteredScopeKey, scopeKey, scope)
        }

        for (const exportDetail of exportsDetail) {
          switch (exportDetail.type) {
            case 'EXPORT_DEFAULT_FUNCTION_OR_CLASS':
              if (exportDetail.name == null) {
                addToFilteredScope('default', 'default')
              } else {
                addToFilteredScope('default', exportDetail.name)
              }
              break
            case 'EXPORT_IDENTIFIER':
              addToFilteredScope('default', exportDetail.name)
              break
            case 'EXPORT_CLASS':
              addToFilteredScope(exportDetail.className, exportDetail.className)
              break
            case 'EXPORT_FUNCTION':
              addToFilteredScope(exportDetail.functionName, exportDetail.functionName)
              break
            case 'EXPORT_VARIABLES':
              for (const exportVar of exportDetail.variables) {
                const exportName = exportVar.variableAlias ?? exportVar.variableName
                addToFilteredScope(exportName, exportVar.variableName)
              }
              break
            case 'EXPORT_DESTRUCTURED_ASSIGNMENT':
              throw new Error(
                `EXPORT_DESTRUCTURED_ASSIGNMENT cases should not be handled this way.`,
              )
            case 'REEXPORT_WILDCARD':
              {
                const reexportedModule = customRequire(
                  resolvedFilePath,
                  exportDetail.reexportedModule,
                )
                if (typeof reexportedModule === 'object') {
                  if (exportDetail.namespacedVariable == null) {
                    filteredScope = {
                      ...filteredScope,
                      ...omit(['default'], reexportedModule),
                    }
                  } else {
                    filteredScope = {
                      ...filteredScope,
                      [exportDetail.namespacedVariable]: {
                        ...omit(['default'], reexportedModule),
                      },
                    }
                  }
                } else {
                  if (exportDetail.namespacedVariable == null) {
                    return left(
                      `Unable to re-export ${exportDetail.reexportedModule} as it does not return an object.`,
                    )
                  } else {
                    filteredScope = {
                      ...filteredScope,
                      [exportDetail.namespacedVariable]: omit(['default'], reexportedModule),
                    }
                  }
                }
              }
              break
            case 'REEXPORT_VARIABLES':
              {
                const reexportedModule = customRequire(
                  resolvedFilePath,
                  exportDetail.reexportedModule,
                )
                if (typeof reexportedModule === 'object') {
                  for (const exportVar of exportDetail.variables) {
                    const exportName = exportVar.variableAlias ?? exportVar.variableName
                    addToFilteredScopeFromSpecificScope(
                      exportName,
                      exportVar.variableName,
                      reexportedModule,
                    )
                  }
                } else {
                  return left(
                    `Unable to re-export ${exportDetail.reexportedModule} as it does not return an object.`,
                  )
                }
              }
              break
            case 'EXPORT_VARIABLES_WITH_MODIFIER':
              for (const exportVar of exportDetail.variables) {
                addToFilteredScope(exportVar, exportVar)
              }
              break
            default:
              const _exhaustiveCheck: never = exportDetail
              throw new Error(`Unhandled type ${JSON.stringify(exportDetail)}`)
          }
        }

        return right(filteredScope)
      } else {
        return left(`File ${resolvedFilePath} contains no components`)
      }
    } else {
      return left(`File ${resolvedFilePath} is not a ParseSuccess`)
    }
  }, filePathResolveResult)
}

function useGetStoryboardRoot(
  focusedElementPath: ElementPath | null,
  topLevelElementsMap: Map<string | null, UtopiaJSXComponent>,
  executionScope: MapLike<any>,
  projectContents: ProjectContentTreeRoot,
  uiFilePath: string,
  transientFilesState: TransientFilesState | null,
  resolve: (importOrigin: string, toImport: string) => Either<string, string>,
): {
  StoryboardRootComponent: ComponentRendererComponent | undefined
  storyboardRootElementPath: ElementPath
  rootValidPathsSet: Set<ElementPath>
  rootValidPathsArray: Array<ElementPath>
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

  const rootValidPathsArray = validPaths.map(EP.makeLastPartOfPathStatic)
  const rootValidPathsSet = new Set(rootValidPathsArray)

  return {
    StoryboardRootComponent: StoryboardRootComponent,
    storyboardRootElementPath: storyboardRootElementPath,
    rootValidPathsSet: rootValidPathsSet,
    rootValidPathsArray: rootValidPathsArray,
    rootInstancePath: EP.emptyElementPath,
  }
}

export interface CanvasContainerProps {
  canvasRootElementElementPath: ElementPath
  validRootPaths: Array<ElementPath>
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
