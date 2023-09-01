import React from 'react'
import type { MapLike } from 'typescript'
// Inject the babel helpers into the global scope
import '../../bundled-dependencies/babelHelpers'
import * as EP from '../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import { isUtopiaJSXComponent } from '../../core/shared/element-template'
import type { Imports, ElementPath } from '../../core/shared/project-file-types'
import {
  isParseSuccess,
  isTextFile,
  isReexportExportDetail,
  isExportDestructuredAssignment,
} from '../../core/shared/project-file-types'
import type { Either } from '../../core/shared/either'
import {
  flatMapEither,
  foldEither,
  forEachRight,
  isRight,
  left,
  right,
} from '../../core/shared/either'
import Utils from '../../utils/utils'
import type {
  CurriedResolveFn,
  CurriedUtopiaRequireFn,
  PropertyControlsInfo,
} from '../custom-code/code-file'
import type {
  DerivedState,
  EditorState,
  ConsoleLog,
  CanvasBase64Blobs,
  ElementsToRerender,
  AllElementProps,
} from '../editor/store/editor-state'
import {
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  UIFileBase64Blobs,
  getIndexHtmlFileFromEditorState,
  TransientFilesState,
} from '../editor/store/editor-state'
import { proxyConsole } from './console-proxy'
import type { UpdateMutableCallback } from './dom-walker'
import { isLiveMode, isTextEditMode } from '../editor/editor-modes'
import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { normalizeName } from '../custom-code/custom-code-utils'
import { getGeneratedExternalLinkText } from '../../printer-parsers/html/external-resources-parser'
import { Helmet } from 'react-helmet'
import parse from 'html-react-parser'
import type { ComponentRendererComponent } from './ui-jsx-canvas-renderer/ui-jsx-canvas-component-renderer'
import type { MutableUtopiaCtxRefData } from './ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import {
  RerenderUtopiaCtxAtom,
  SceneLevelUtopiaCtxAtom,
  UtopiaProjectCtxAtom,
} from './ui-jsx-canvas-renderer/ui-jsx-canvas-contexts'
import { CanvasContainerID } from './canvas-types'
import {
  useKeepReferenceEqualityIfPossible,
  useKeepShallowReferenceEquality,
} from '../../utils/react-performance'
import { unimportAllButTheseCSSFiles } from '../../core/webpack-loaders/css-loader'
import { UTOPIA_INSTANCE_PATH } from '../../core/model/utopia-constants'
import type { ProjectContentTreeRoot } from '../assets'
import { getProjectFileByFilePath } from '../assets'
import { createExecutionScope } from './ui-jsx-canvas-renderer/ui-jsx-canvas-execution-scope'
import { applyUIDMonkeyPatch } from '../../utils/canvas-react-utils'
import { getParseSuccessForFilePath, getValidElementPaths } from './canvas-utils'
import { arrayEqualsByValue, fastForEach, NO_OP } from '../../core/shared/utils'
import { useTwind } from '../../core/tailwind/tailwind'
import {
  AlwaysFalse,
  atomWithPubSub,
  usePubSubAtomReadOnly,
} from '../../core/shared/atom-with-pub-sub'
import { omit } from '../../core/shared/object-utils'
import { validateControlsToCheck } from './canvas-globals'
import type { EditorDispatch } from '../editor/action-types'
import {
  clearListOfEvaluatedFiles,
  getListOfEvaluatedFiles,
} from '../../core/shared/code-exec-utils'
import { forceNotNull } from '../../core/shared/optional-utils'

applyUIDMonkeyPatch()

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
  displayNoneInstances: Array<ElementPath>
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
  propertyControlsInfo: PropertyControlsInfo
  dispatch: EditorDispatch
  domWalkerAdditionalElementsToUpdate: Array<ElementPath>
  editedText: ElementPath | null
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
    const { imports: imports_KILLME } = getParseSuccessForFilePath(
      uiFilePath,
      editor.projectContents,
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

    const editedText = isTextEditMode(editor.mode) ? editor.mode.editedText : null

    return {
      uiFilePath: uiFilePath,
      curriedRequireFn: editor.codeResultCache.curriedRequireFn,
      curriedResolveFn: editor.codeResultCache.curriedResolveFn,
      hiddenInstances: hiddenInstances,
      displayNoneInstances: editor.displayNoneInstances,
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
      propertyControlsInfo: editor.propertyControlsInfo,
      dispatch: dispatch,
      domWalkerAdditionalElementsToUpdate: editor.canvas.domWalkerAdditionalElementsToUpdate,
      editedText: editedText,
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
  validPaths: Set<string>,
  spyCollectorContextRef: UiJsxCanvasContextData,
): void {
  const spyKeys = Object.keys(spyCollectorContextRef.current.spyValues.metadata)
  fastForEach(spyKeys, (elementPathString) => {
    const elementPath =
      spyCollectorContextRef.current.spyValues.metadata[elementPathString].elementPath
    const staticElementPath = EP.makeLastPartOfPathStatic(elementPath)
    if (!validPaths.has(EP.toString(staticElementPath))) {
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
    displayNoneInstances,
    imports_KILLME: imports, // FIXME this is the storyboard imports object used only for the cssimport
    clearErrors,
    clearConsoleLogs,
    addToConsoleLogs,
    canvasIsLive,
    linkTags,
    base64FileBlobs,
    projectContents,
    shouldIncludeCanvasRootInTheSpy,
    propertyControlsInfo,
    dispatch,
    editedText,
  } = props

  clearListOfEvaluatedFiles()
  let resolvedFileNames = React.useRef<Array<string>>([]) // resolved (i.e. imported) files this render
  resolvedFileNames.current = [uiFilePath]
  let evaluatedFileNames = React.useRef<Array<string>>([]) // evaluated (i.e. not using a cached evaluation) this render
  evaluatedFileNames.current = [uiFilePath]
  React.useEffect(() => {
    setTimeout(() => {
      // wrapping in a setTimeout so we don't dispatch from inside React lifecycle
      void validateControlsToCheck(
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

  React.useEffect(() => {
    if (clearErrors != null) {
      // a new canvas render, a new chance at having no errors
      clearErrors()
    }
  }, [clearErrors])

  let metadataContext: UiJsxCanvasContextData = forceNotNull(
    `Missing UiJsxCanvasCtxAtom provider`,
    usePubSubAtomReadOnly(UiJsxCanvasCtxAtom, AlwaysFalse),
  )

  const updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData = usePubSubAtomReadOnly(
    DomWalkerInvalidatePathsCtxAtom,
    AlwaysFalse,
  )
  useClearSpyMetadataOnRemount(props.mountCount, props.domWalkerInvalidateCount, metadataContext)

  const elementsToRerenderRef = React.useRef(ElementsToRerenderGLOBAL.current)
  const shouldRerenderRef = React.useRef(false)
  shouldRerenderRef.current =
    ElementsToRerenderGLOBAL.current === 'rerender-all-elements' ||
    elementsToRerenderRef.current === 'rerender-all-elements' || // TODO this means the first drag frame will still be slow, figure out a nicer way to immediately switch to true. probably this should live in a dedicated a function
    !arrayEqualsByValue(
      ElementsToRerenderGLOBAL.current,
      elementsToRerenderRef.current,
      EP.pathsEqual,
    ) // once we get here, we know that both `ElementsToRerenderGLOBAL.current` and `elementsToRerenderRef.current` are arrays
  elementsToRerenderRef.current = ElementsToRerenderGLOBAL.current

  const maybeOldProjectContents = React.useRef(projectContents)
  if (shouldRerenderRef.current) {
    maybeOldProjectContents.current = projectContents
  }

  const projectContentsForRequireFn = maybeOldProjectContents.current
  const requireFn = React.useMemo(
    () => curriedRequireFn(projectContentsForRequireFn),
    [curriedRequireFn, projectContentsForRequireFn],
  )

  const resolve = React.useMemo(
    () => curriedResolveFn(projectContentsForRequireFn),
    [curriedResolveFn, projectContentsForRequireFn],
  )

  let mutableContextRef = React.useRef<MutableUtopiaCtxRefData>({})

  let topLevelComponentRendererComponents = React.useRef<
    MapLike<MapLike<ComponentRendererComponent>>
  >({})

  let resolvedFiles = React.useRef<MapLike<Array<string>>>({}) // Mapping from importOrigin to an array of toImport
  resolvedFiles.current = {}

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
        projectContentsForRequireFn,
        customRequire,
        mutableContextRef,
        topLevelComponentRendererComponents,
        uiFilePath,
        base64FileBlobs,
        hiddenInstances,
        displayNoneInstances,
        metadataContext,
        updateInvalidatedPaths,
        shouldIncludeCanvasRootInTheSpy,
        filePathResolveResult,
        editedText,
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
    [
      requireFn,
      resolve,
      projectContentsForRequireFn,
      uiFilePath,
      base64FileBlobs,
      hiddenInstances,
      displayNoneInstances,
      metadataContext,
      updateInvalidatedPaths,
      shouldIncludeCanvasRootInTheSpy,
      editedText,
    ],
  )

  const { scope, topLevelJsxComponents } = React.useMemo(() => {
    const executionScope = createExecutionScope(
      uiFilePath,
      customRequire,
      mutableContextRef,
      topLevelComponentRendererComponents,
      projectContentsForRequireFn,
      uiFilePath, // this is the storyboard filepath
      base64FileBlobs,
      hiddenInstances,
      displayNoneInstances,
      metadataContext,
      updateInvalidatedPaths,
      props.shouldIncludeCanvasRootInTheSpy,
      editedText,
    )

    // IMPORTANT this assumes createExecutionScope ran and did a full walk of the transitive imports!!
    if (shouldRerenderRef.current) {
      // since rerender-all-elements means we did a full rebuild of the canvas scope,
      // any CSS file that was not resolved during this rerender can be unimported
      unimportAllButTheseCSSFiles(resolvedFileNames.current)
    }
    return executionScope
  }, [
    base64FileBlobs,
    customRequire,
    displayNoneInstances,
    hiddenInstances,
    metadataContext,
    projectContentsForRequireFn,
    props.shouldIncludeCanvasRootInTheSpy,
    editedText,
    uiFilePath,
    updateInvalidatedPaths,
  ])

  evaluatedFileNames.current = getListOfEvaluatedFiles()

  const executionScope = scope

  useTwind(projectContentsForRequireFn, customRequire, '#canvas-container')

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
    projectContentsForRequireFn,
    uiFilePath,
    resolve,
  )

  clearSpyCollectorInvalidPaths(rootValidPathsSet, metadataContext)

  const sceneLevelUtopiaContextValue = useKeepReferenceEqualityIfPossible({
    validPaths: rootValidPathsSet,
  })

  const rerenderUtopiaContextValue = useKeepShallowReferenceEquality({
    hiddenInstances: hiddenInstances,
    displayNoneInstances: displayNoneInstances,
    canvasIsLive: canvasIsLive,
    shouldIncludeCanvasRootInTheSpy: props.shouldIncludeCanvasRootInTheSpy,
    editedText: props.editedText,
  })

  const utopiaProjectContextValue = useKeepShallowReferenceEquality({
    projectContents: props.projectContents,
    openStoryboardFilePathKILLME: props.uiFilePath,
    resolve: resolve,
  })

  const StoryboardRoot = React.useMemo(() => {
    return StoryboardRootComponent == null ? null : (
      <StoryboardRootComponent {...{ [UTOPIA_INSTANCE_PATH]: rootInstancePath }} />
    )
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [
    StoryboardRootComponent,
    rootInstancePath,
    props.domWalkerInvalidateCount,
    props.mountCount,
    // eslint-disable-next-line react-hooks/exhaustive-deps
    ElementsToRerenderGLOBAL.current,
  ])

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
  base64FileBlobs: CanvasBase64Blobs,
  hiddenInstances: ElementPath[],
  displayNoneInstances: Array<ElementPath>,
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: UpdateMutableCallback<Set<string>>,
  shouldIncludeCanvasRootInTheSpy: boolean,
  filePathResolveResult: Either<string, string>,
  editedText: ElementPath | null,
): Either<string, MapLike<any>> {
  return flatMapEither((resolvedFilePath) => {
    resolvedFromThisOrigin.push(toImport)
    const projectFile = getProjectFileByFilePath(projectContents, resolvedFilePath)
    if (
      projectFile != null &&
      isTextFile(projectFile) &&
      isParseSuccess(projectFile.fileContents.parsed)
    ) {
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
          base64FileBlobs,
          hiddenInstances,
          displayNoneInstances,
          metadataContext,
          updateInvalidatedPaths,
          shouldIncludeCanvasRootInTheSpy,
          editedText,
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
  resolve: (importOrigin: string, toImport: string) => Either<string, string>,
): {
  StoryboardRootComponent: ComponentRendererComponent | undefined
  storyboardRootElementPath: ElementPath
  rootValidPathsSet: Set<string>
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
          resolve,
        )
  const storyboardRootElementPath = useKeepReferenceEqualityIfPossible(
    validPaths[0] ?? EP.emptyElementPath,
  )

  const rootValidPathsArray = validPaths.map(EP.makeLastPartOfPathStatic)
  const rootValidPathsSet = new Set(rootValidPathsArray.map(EP.toString))

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
