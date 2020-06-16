import * as deepEquals from 'fast-deep-equal'
import { produce } from 'immer'
import * as R from 'ramda'
import * as React from 'react'
import * as ReactDOMServer from 'react-dom/server'
import { PRODUCTION_ENV } from '../../../common/env-vars'
import { convertMetadataMap, MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ComponentMetadata } from '../../../core/shared/element-template'
import { getAllUniqueUids } from '../../../core/model/element-template-utils'
import { fileTypeFromFileName, updateParseResultCode } from '../../../core/model/project-file-utils'
import {
  TemplatePath,
  UIJSFile,
  isCodeOrUiJsFile,
  isParseSuccess,
} from '../../../core/shared/project-file-types'
import {
  codeNeedsParsing,
  codeNeedsPrinting,
} from '../../../core/workers/common/project-file-utils'
import { getParseResult, printCodeAsync } from '../../../core/workers/parser-printer/parser-printer'
import { isJsOrTsFile, isCssFile } from '../../../core/workers/ts/ts-worker'
import { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import { runLocalCanvasAction } from '../../../templates/editor-canvas'
import { runLocalNavigatorAction } from '../../../templates/editor-navigator'
import { optionalDeepFreeze } from '../../../utils/deep-freeze'
import { bimapEither } from '../../../core/shared/either'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import Utils from '../../../utils/utils'
import { CanvasAction } from '../../canvas/canvas-types'
import { produceCanvasTransientState } from '../../canvas/canvas-utils'
import { removeConsoleProxy } from '../../canvas/console-proxy'
import {
  pickUiJsxCanvasProps,
  UiJsxCanvas,
  UiJsxCanvasContext,
  UiJsxCanvasContextData,
} from '../../canvas/ui-jsx-canvas'
import { LocalNavigatorAction } from '../../navigator/actions'
import { PreviewIframeId } from '../../preview/preview-pane'
import * as TP from '../../../core/shared/template-path'
import { EditorAction, EditorDispatch, isLoggedIn, LoginState } from '../action-types'
import { isTransientAction, isUndoOrRedo, isParsedModelUpdate } from '../actions/action-utils'
import * as EditorActions from '../actions/actions'
import * as History from '../history'
import { StateHistory } from '../history'
import { saveToLocalStorage, saveToServer } from '../persistence'
import { saveStoredState } from '../stored-state'
import {
  DerivedState,
  deriveState,
  EditorState,
  EditorStore,
  getAllBuildErrors,
  getAllErrorsFromFiles,
  getAllLintErrors,
  getOpenFilename,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  getOpenUtopiaJSXComponentsFromState,
  PersistentModel,
  persistentModelFromEditorModel,
  reconstructJSXMetadata,
  storedEditorStateFromEditorState,
} from './editor-state'
import { runLocalEditorAction } from './editor-update'
import { arrayEquals, isBrowserEnvironment } from '../../../core/shared/utils'
import { getDependencyTypeDefinitions } from '../../../core/es-modules/package-manager/package-manager'

interface DispatchResult extends EditorStore {
  nothingChanged: boolean
  entireUpdateFinished: Promise<any>
}

type SaveType = 'model' | 'name' | 'both'

function simpleStringifyAction(action: EditorAction): string {
  switch (action.action) {
    case 'TRANSIENT_ACTIONS':
      return `Transient: ${simpleStringifyActions(action.transientActions)}`
    case 'ATOMIC':
      return `Atomic: ${simpleStringifyActions(action.actions)}`
    default:
      return action.action
  }
}

export function simpleStringifyActions(actions: ReadonlyArray<EditorAction>): string {
  return `[\n\t${actions.map(simpleStringifyAction).join(',\n')}\n]`
}

function processAction(
  dispatchEvent: EditorDispatch,
  working: EditorStore,
  action: EditorAction,
): EditorStore {
  const workingHistory = working.history
  // Sidestep around the local actions so that we definitely run them locally.
  if (action.action === 'TRANSIENT_ACTIONS') {
    // Drill into the array.
    return processActions(dispatchEvent, working, action.transientActions)
  } else if (action.action === 'ATOMIC') {
    // Drill into the array.
    return processActions(dispatchEvent, working, action.actions)
  } else if (action.action === 'UNDO' && !History.canUndo(workingHistory)) {
    // Bail early and make no changes.
    return working
  } else if (action.action === 'REDO' && !History.canRedo(workingHistory)) {
    // Bail early and make no changes.
    return working
  } else {
    // Process action on the JS side.
    const editorAfterUpdateFunction = runLocalEditorAction(
      working.editor,
      working.derived,
      working.loginState,
      working.workers,
      action as EditorAction,
      workingHistory,
      dispatchEvent,
    )
    const editorAfterCanvas = runLocalCanvasAction(
      editorAfterUpdateFunction,
      working.derived,
      action as CanvasAction,
    )
    let editorAfterNavigator = runLocalNavigatorAction(
      editorAfterCanvas,
      working.derived,
      action as LocalNavigatorAction,
    )

    let newStateHistory: StateHistory
    switch (action.action) {
      case 'UNDO':
        newStateHistory = History.undo(workingHistory)
        break
      case 'REDO':
        newStateHistory = History.redo(workingHistory)
        break
      case 'NEW':
      case 'LOAD':
        const derivedResult = deriveState(editorAfterNavigator, null, false)
        editorAfterNavigator = derivedResult.editor
        newStateHistory = History.init(derivedResult.editor, derivedResult.derived)
        break
      default:
        newStateHistory = workingHistory
        break
    }

    return {
      editor: editorAfterNavigator,
      derived: working.derived,
      history: newStateHistory,
      loginState: working.loginState,
      workers: working.workers,
      dispatch: dispatchEvent,
    }
  }
}

function processActions(
  dispatchEvent: EditorDispatch,
  working: EditorStore,
  actions: Array<EditorAction>,
): EditorStore {
  return actions.reduce((workingFuture: EditorStore, action: EditorAction) => {
    return processAction(dispatchEvent, workingFuture, action)
  }, working)
}

export function updateFloaterPreview(modelId: string | null, model: PersistentModel): void {
  const floaterElement = document.getElementById(PreviewIframeId)
  if (floaterElement != null) {
    const iFrameFloaterElement = (floaterElement as any) as HTMLIFrameElement
    const contentWindow = iFrameFloaterElement.contentWindow
    if (contentWindow != null) {
      const modelWithId: PersistentModel = {
        ...model,
        appID: modelId,
      }
      try {
        contentWindow.postMessage(modelWithId, '*')
      } catch (exception) {
        // Don't nuke the editor if there's an exception posting the message.
        // This can happen if a value can't be cloned when posted.
        console.error('Error updating preview.', exception)
      }
    }
  }
}

function maybeRequestModelUpdate(
  file: UIJSFile,
  filePath: string,
  workers: UtopiaTsWorkers,
  dispatch: EditorDispatch,
): { modelUpdateRequested: boolean; parseOrPrintFinished: Promise<boolean> } {
  if (codeNeedsParsing(file.revisionsState)) {
    const code = file.fileContents.value.code
    const parseFinished = getParseResult(workers, filePath, code)
      .then((parseResult) => {
        const parseResultRestoredCode = bimapEither(
          (failure) => {
            return {
              ...failure,
              code: code,
            }
          },
          (success) => success,
          parseResult,
        )
        const updatedFile: UIJSFile = {
          ...file,
          fileContents: parseResultRestoredCode,
        }

        dispatch([EditorActions.updateFromWorker(filePath, updatedFile, 'Model')])
        return true
      })
      .catch((e) => {
        console.error('error during parse', e)
        dispatch([EditorActions.clearParseOrPrintInFlight()])
        return true
      })
    return { modelUpdateRequested: true, parseOrPrintFinished: parseFinished }
  } else if (codeNeedsPrinting(file.revisionsState) && isParseSuccess(file.fileContents)) {
    const printFinished = printCodeAsync(workers, file.fileContents.value)
      .then((printResult) => {
        const updatedContents = updateParseResultCode(
          file.fileContents,
          printResult.code,
          printResult.highlightBounds,
        )
        const updatedFile: UIJSFile = {
          ...file,
          fileContents: updatedContents,
        }

        dispatch([EditorActions.updateFromWorker(filePath, updatedFile, 'Code')])

        return true
      })
      .catch((e) => {
        console.error('error during print', e)
        dispatch([EditorActions.clearParseOrPrintInFlight()])
        return true
      })
    return { modelUpdateRequested: true, parseOrPrintFinished: printFinished }
  }

  return { modelUpdateRequested: false, parseOrPrintFinished: Promise.resolve(true) }
}

function maybeRequestModelUpdateOnEditor(
  editor: EditorState,
  workers: UtopiaTsWorkers,
  dispatch: EditorDispatch,
): { editorState: EditorState; modelUpdateFinished: Promise<boolean> } {
  if (editor.parseOrPrintInFlight) {
    // Prevent repeated requests
    return { editorState: editor, modelUpdateFinished: Promise.resolve(true) }
  }

  const openUIJSFile = getOpenUIJSFile(editor)
  const openUIJSFilePath = getOpenUIJSFileKey(editor)
  if (openUIJSFile == null || openUIJSFilePath == null) {
    return { editorState: editor, modelUpdateFinished: Promise.resolve(true) }
  } else {
    const modelUpdateRequested = maybeRequestModelUpdate(
      openUIJSFile,
      openUIJSFilePath,
      workers,
      dispatch,
    )
    return {
      editorState: {
        ...editor,
        parseOrPrintInFlight: modelUpdateRequested.modelUpdateRequested,
      },
      modelUpdateFinished: modelUpdateRequested.parseOrPrintFinished,
    }
  }
}

export function editorDispatch(
  boundDispatch: EditorDispatch,
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStore,
): DispatchResult {
  const isLoadAction = dispatchedActions.some((a) => a.action === 'LOAD')
  const nameUpdated = dispatchedActions.some((action) => action.action === 'SET_PROJECT_NAME')
  const forceSave = dispatchedActions.some((action) => action.action === 'SAVE_CURRENT_FILE')
  const onlyNameUpdated = nameUpdated && dispatchedActions.length === 1
  const allTransient = dispatchedActions.every(isTransientAction)
  const updateCodeResultCache = dispatchedActions.some(
    (action) => action.action === 'UPDATE_CODE_RESULT_CACHE',
  )

  const allBuildErrorsInState = getAllBuildErrors(storedState.editor)

  const allLintErrorsInState = getAllLintErrors(storedState.editor)

  const updateCodeEditorErrors = dispatchedActions.some(
    (action) =>
      (action.action === 'SET_CODE_EDITOR_BUILD_ERRORS' &&
        !arrayEquals(
          allBuildErrorsInState,
          getAllErrorsFromFiles(action.buildErrors),
          (a, b) => a.message !== b.message,
        )) ||
      (action.action === 'SET_CODE_EDITOR_LINT_ERRORS' &&
        !arrayEquals(
          allLintErrorsInState,
          getAllErrorsFromFiles(action.lintErrors),
          (a, b) => a.message !== b.message,
        )),
  )

  const anyUndoOrRedo = dispatchedActions.some(isUndoOrRedo)
  const anySendPreviewModel = dispatchedActions.some(EditorActions.isSendPreviewModel)

  // With this reducer we can split the actions into groups (arrays) which can be dispatched together without rebuilding the derived state.
  // Between the different group derived state rebuild is needed
  const reducerToSplitToActionGroups = (
    actionGroups: EditorAction[][],
    currentAction: EditorAction,
  ): EditorAction[][] => {
    if (currentAction.action === `TRANSIENT_ACTIONS`) {
      // if this is a transient action we need to split its sub-actions into groups which can be dispatched together
      const transientActionGroups = currentAction.transientActions.reduce(
        reducerToSplitToActionGroups,
        [[]],
      )
      const wrappedTransientActionGroups = transientActionGroups.map((actionGroup) => [
        EditorActions.transientActions(actionGroup),
      ])
      return [...actionGroups, ...wrappedTransientActionGroups]
    } else {
      // if this action does not need a rebuilt derived state we can just push it into the last action group to dispatch them together
      let updatedGroups = actionGroups
      updatedGroups[actionGroups.length - 1].push(currentAction)
      return updatedGroups
    }
  }
  const actionGroupsToProcess = dispatchedActions.reduce(reducerToSplitToActionGroups, [[]])

  const result: DispatchResult = actionGroupsToProcess.reduce(
    (working: DispatchResult, actions) => {
      return editorDispatchInner(boundDispatch, actions, working, allTransient)
    },
    { ...storedState, entireUpdateFinished: Promise.resolve(true), nothingChanged: true },
  )

  const transientOrNoChange = allTransient || result.nothingChanged
  const workerUpdatedModel = dispatchedActions.some(
    (action) => action.action === 'UPDATE_FROM_WORKER',
  )

  const editorWithModelChecked =
    !anyUndoOrRedo && transientOrNoChange && !workerUpdatedModel
      ? { editorState: result.editor, modelUpdateFinished: Promise.resolve(true) }
      : maybeRequestModelUpdateOnEditor(result.editor, storedState.workers, boundDispatch)

  const editorFilteredForFiles = filterEditorForFiles(editorWithModelChecked.editorState)

  const frozenEditorState = editorFilteredForFiles
  const frozenDerivedState = result.derived

  let newHistory: StateHistory
  if (transientOrNoChange) {
    newHistory = result.history
  } else {
    newHistory = History.add(result.history, frozenEditorState, frozenDerivedState)
  }

  let saveType: SaveType = 'model'
  if (nameUpdated) {
    if (onlyNameUpdated) {
      saveType = 'name'
    } else {
      saveType = 'both'
    }
  }

  const isLoaded = frozenEditorState.isLoaded
  const shouldSave =
    isLoaded &&
    !isLoadAction &&
    (!allTransient || anyUndoOrRedo || updateCodeResultCache || updateCodeEditorErrors) &&
    isBrowserEnvironment
  if (shouldSave) {
    save(frozenEditorState, boundDispatch, storedState.loginState, saveType, forceSave)
    const stateToStore = storedEditorStateFromEditorState(storedState.editor)
    saveStoredState(storedState.editor.id, stateToStore)
    notifyTsWorker(frozenEditorState, storedState.editor, storedState.workers)
  }

  if (shouldSave || anySendPreviewModel) {
    updateFloaterPreview(frozenEditorState.id, persistentModelFromEditorModel(frozenEditorState))
  }

  if (frozenEditorState.id != null && frozenEditorState.id != storedState.editor.id) {
    storedState.workers.initWatchdogWorker(frozenEditorState.id)
  }

  return {
    editor: frozenEditorState,
    derived: frozenDerivedState,
    history: newHistory,
    loginState: storedState.loginState,
    workers: storedState.workers,
    dispatch: boundDispatch,
    nothingChanged: result.nothingChanged,
    entireUpdateFinished: Promise.all([
      result.entireUpdateFinished,
      editorWithModelChecked.modelUpdateFinished,
    ]),
  }
}

function editorDispatchInner(
  boundDispatch: EditorDispatch,
  dispatchedActions: EditorAction[],
  storedState: DispatchResult,
  transient: boolean,
): DispatchResult {
  // console.log('DISPATCH', simpleStringifyActions(dispatchedActions))

  if (!PRODUCTION_ENV && typeof window.performance.mark === 'function') {
    window.performance.mark('dispatch_begin')
  }
  if (dispatchedActions.length > 0) {
    // Run everything in a big chain.
    let result = processActions(boundDispatch, storedState, dispatchedActions)

    const anyUndoOrRedo = R.any(isUndoOrRedo, dispatchedActions)

    if (!PRODUCTION_ENV && typeof window.performance.mark === 'function') {
      window.performance.mark('derived_state_begin')
    }

    const editorStayedTheSame = storedState.nothingChanged && storedState.editor === result.editor

    // IMPORTANT This code assumes only a single ui file can be open at a time. If we ever want to
    // support multiple ui files side by side in a multi-tabbed view we'll need to rethink
    // how and where we store the jsx metadata
    const isUiJsFileSelected = fileTypeFromFileName(getOpenFilename(result.editor)) === 'UI_JS_FILE'
    let spyResult: ComponentMetadata[]

    if (isUiJsFileSelected) {
      // Needs to run here as changes may have been made which need to be reflected in the
      // spy result, which only runs if the canvas props are determined to have changed.
      result.derived = {
        ...result.derived,
        canvas: {
          ...result.derived.canvas,
          transientState: produceCanvasTransientState(result.editor, true),
        },
      }

      const spyCollector: UiJsxCanvasContextData = {
        current: {
          spyValues: {
            metadata: {},
            scenes: {},
          },
        },
      }
      let canvasProps = pickUiJsxCanvasProps(
        result.editor,
        result.derived,
        false,
        true,
        Utils.NO_OP,
        Utils.NO_OP,
        Utils.NO_OP,
        boundDispatch,
      )
      let priorCanvasProps = pickUiJsxCanvasProps(
        storedState.editor,
        storedState.derived,
        false,
        true,
        Utils.NO_OP,
        Utils.NO_OP,
        Utils.NO_OP,
        boundDispatch,
      )
      try {
        if (deepEquals(canvasProps, priorCanvasProps)) {
          spyResult = result.editor.spyMetadataKILLME
        } else {
          // this function call needs to be in a try-catch because react error boundaries are not working in ReactDOMServer
          removeConsoleProxy(window.console)
          ReactDOMServer.renderToString(
            <UiJsxCanvasContext.Provider value={spyCollector}>
              <UiJsxCanvas {...canvasProps} clearErrors={Utils.NO_OP} reportError={Utils.NO_OP} />
            </UiJsxCanvasContext.Provider>,
          )
          const transientState = result.derived.canvas.transientState
          if (transientState.fileState == null) {
            // See below word of warning
            spyResult = result.editor.spyMetadataKILLME
          } else {
            spyResult = convertMetadataMap(
              spyCollector.current.spyValues.metadata,
              spyCollector.current.spyValues.scenes,
            )
          }
        }
      } catch (e) {
        // TODO We should protect ourselves when trying to actually read from the metadata in this case
        spyResult = result.editor.spyMetadataKILLME
      }
    } else {
      spyResult = result.editor.spyMetadataKILLME
    }

    result = keepDeepReferenceEqualityIfPossible(result, {
      ...result,
      editor: {
        ...result.editor,
        spyMetadataKILLME: spyResult,
      },
    })

    const domMetadataChanged =
      storedState.editor.domMetadataKILLME !== result.editor.domMetadataKILLME
    const spyMetadataChanged =
      storedState.editor.spyMetadataKILLME !== result.editor.spyMetadataKILLME
    const dragStateLost =
      storedState.editor.canvas.dragState != null && result.editor.canvas.dragState == null
    const metadataChanged = domMetadataChanged || spyMetadataChanged || dragStateLost
    // TODO: Should this condition actually be `&&`?
    // Tested quickly and it broke selection, but I'm mostly certain
    // it should only merge when both have changed.
    if (metadataChanged) {
      result = produce(result, (r) => {
        if (r.editor.canvas.dragState == null) {
          r.editor.jsxMetadataKILLME = reconstructJSXMetadata(result.editor)
        } else {
          r.editor.canvas.dragState.metadata = reconstructJSXMetadata(result.editor)
        }

        const allLostElements = lostElements(r.editor.selectedViews, r.editor.jsxMetadataKILLME)
        const newLostElements = TP.filterPaths(allLostElements, r.editor.warnedInstances)
        if (newLostElements.length > 0 && isBrowserEnvironment) {
          // FIXME The above `isBrowserEnvironment` check is required because this is tripped by tests that don't update the metadata
          // correctly. Rather than preventing this code running during tests, we should make sure tests are all updating metadata correctly.
          const toastAction = EditorActions.showToast({
            message: `Some elements are no longer being rendered`,
            level: 'WARNING',
          })
          setTimeout(() => boundDispatch([toastAction], 'everyone'), 0)
        }

        r.editor.warnedInstances = allLostElements
      })
    }

    const cleanedEditor = metadataChanged
      ? removeNonExistingViewReferencesFromState(result.editor)
      : result.editor

    let frozenEditorState: EditorState = optionalDeepFreeze(cleanedEditor)

    let frozenDerivedState: DerivedState
    if (anyUndoOrRedo) {
      frozenDerivedState = optionalDeepFreeze(EditorActions.restoreDerivedState(result.history))
      // TODO BB put inspector and navigator back to history
    } else if (editorStayedTheSame) {
      // !! We completely skip creating a new derived state, since the editor state stayed the exact same
      frozenDerivedState = storedState.derived
    } else {
      const parsedModelUpdated = R.any(isParsedModelUpdate, dispatchedActions)
      const derivedResult = deriveState(frozenEditorState, storedState.derived, parsedModelUpdated)
      frozenEditorState = derivedResult.editor
      frozenDerivedState = optionalDeepFreeze(derivedResult.derived)
    }

    if (!PRODUCTION_ENV) {
      const actionNames = dispatchedActions.map((action) => action.action).join(',')
      getAllUniqueUids(getOpenUtopiaJSXComponentsFromState(frozenEditorState), actionNames)

      if (typeof window.performance.mark === 'function') {
        window.performance.mark('dispatch_end')
        window.performance.measure(
          `Momentum Dispatch: [${actionNames}]`,
          'dispatch_begin',
          'dispatch_end',
        )
        window.performance.measure(
          'Momentum Editor State Update',
          'dispatch_begin',
          'derived_state_begin',
        )
        window.performance.measure(
          'Momentum Editor Derived State',
          'derived_state_begin',
          'dispatch_end',
        )
      }
    }

    return {
      editor: frozenEditorState,
      derived: frozenDerivedState,
      history: result.history,
      loginState: storedState.loginState,
      workers: storedState.workers,
      dispatch: boundDispatch,
      nothingChanged: editorStayedTheSame,
      entireUpdateFinished: Promise.all([storedState.entireUpdateFinished]),
    }
  } else {
    //empty return
    return {
      ...storedState,
      nothingChanged: true,
    }
  }
}

function filterEditorForFiles(editor: EditorState) {
  const allFiles = Object.keys(editor.projectContents)
  return {
    ...editor,
    codeResultCache:
      editor.codeResultCache == null
        ? null
        : {
            ...editor.codeResultCache,
            cache: R.pick(allFiles, editor.codeResultCache.cache),
          },
    codeEditorErrors: {
      buildErrors: R.pick(allFiles, editor.codeEditorErrors.buildErrors),
      lintErrors: R.pick(allFiles, editor.codeEditorErrors.lintErrors),
    },
  }
}

async function save(
  state: EditorState,
  dispatch: EditorDispatch,
  loginState: LoginState,
  saveType: SaveType,
  forceServerSave: boolean,
) {
  const persistentModel =
    saveType === 'model' || saveType === 'both' ? persistentModelFromEditorModel(state) : null
  const projectName = state.projectName
  try {
    if (isLoggedIn(loginState)) {
      saveToServer(dispatch, state.id, persistentModel, projectName, forceServerSave)
    } else {
      saveToLocalStorage(dispatch, state.id, persistentModel, projectName)
    }
  } catch (error) {
    console.error('Save not successful', error)
  }
}

function notifyTsWorker(
  newEditorState: EditorState,
  oldEditorState: EditorState,
  workers: UtopiaTsWorkers,
) {
  let shouldInitTsWorker = false
  let filesToUpdateInTsWorker: string[] = []
  // notify the ts worker if any file is new or has been changed compared to the previous state
  Utils.fastForEach(Object.keys(newEditorState.projectContents), (filename) => {
    const file = newEditorState.projectContents[filename]
    const oldFile = oldEditorState.projectContents[filename]
    if (oldFile == null) {
      shouldInitTsWorker = true
    } else {
      if (isCodeOrUiJsFile(file) && oldFile != file) {
        filesToUpdateInTsWorker.push(filename)
      }
    }
  })
  // notify the ts worker if any file has been removed compared to the previous state
  Utils.fastForEach(Object.keys(oldEditorState.projectContents), (filename) => {
    const file = newEditorState.projectContents[filename]
    if (file == null) {
      shouldInitTsWorker = true
    }
  })

  if (shouldInitTsWorker) {
    workers.sendInitMessage(
      getDependencyTypeDefinitions(newEditorState.nodeModules.files),
      newEditorState.projectContents,
    )
  } else {
    Utils.fastForEach(filesToUpdateInTsWorker, (filename) => {
      const file = newEditorState.projectContents[filename]
      if (isCodeOrUiJsFile(file) && (isJsOrTsFile(filename) || isCssFile(filename))) {
        workers.sendUpdateFileMessage(filename, file, true)
      }
    })
  }
}

function removeNonExistingViewReferencesFromState(editorState: EditorState): EditorState {
  const rootComponents = editorState.jsxMetadataKILLME
  const allPaths = MetadataUtils.getAllPaths(rootComponents)
  const updatedSelectedViews = filterNonExistingViews(allPaths, editorState.selectedViews)
  const updatedHighlightedViews = filterNonExistingViews(allPaths, editorState.highlightedViews)
  const updatedHiddenInstances = filterNonExistingViews(allPaths, editorState.hiddenInstances)
  return {
    ...editorState,
    selectedViews: updatedSelectedViews,
    highlightedViews: updatedHighlightedViews,
    hiddenInstances: updatedHiddenInstances,
  }
}

function lostElements(
  elementsToCheck: TemplatePath[],
  metadata: ComponentMetadata[],
): TemplatePath[] {
  return elementsToCheck.filter((path) => {
    const renderedFrame = MetadataUtils.getFrame(path, metadata)
    return renderedFrame == null || renderedFrame.height <= 0 || renderedFrame.width <= 0
  })
}

function filterNonExistingViews(
  allPaths: Array<TemplatePath>,
  views: Array<TemplatePath>,
): Array<TemplatePath> {
  const filtered = views.filter((path) => TP.containsPath(path, allPaths))
  if (filtered.length !== views.length) {
    return filtered
  } else {
    return views
  }
}
