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
  ProjectContents,
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
import Utils from '../../../utils/utils'
import { CanvasAction } from '../../canvas/canvas-types'
import { LocalNavigatorAction } from '../../navigator/actions'
import { PreviewIframeId, projectContentsUpdateMessage } from '../../preview/preview-pane'
import * as TP from '../../../core/shared/template-path'
import { EditorAction, EditorDispatch, isLoggedIn, LoginState } from '../action-types'
import { isTransientAction, isUndoOrRedo, isParsedModelUpdate } from '../actions/action-utils'
import * as EditorActions from '../actions/actions'
import * as History from '../history'
import { StateHistory } from '../history'
import { saveToLocalStorage, saveToServer, pushProjectURLToBrowserHistory } from '../persistence'
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
import { UiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import {
  getContentsTreeFileFromString,
  ProjectContentTreeRoot,
  walkContentsTree,
} from '../../assets'

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
  spyCollector: UiJsxCanvasContextData,
): EditorStore {
  const workingHistory = working.history
  // Sidestep around the local actions so that we definitely run them locally.
  if (action.action === 'TRANSIENT_ACTIONS') {
    // Drill into the array.
    return processActions(dispatchEvent, working, action.transientActions, spyCollector)
  } else if (action.action === 'ATOMIC') {
    // Drill into the array.
    return processActions(dispatchEvent, working, action.actions, spyCollector)
  } else if (action.action === 'UNDO' && !History.canUndo(workingHistory)) {
    // Bail early and make no changes.
    return working
  } else if (action.action === 'REDO' && !History.canRedo(workingHistory)) {
    // Bail early and make no changes.
    return working
  } else if (action.action === 'SET_SHORTCUT') {
    return {
      ...working,
      userState: EditorActions.UPDATE_FNS.SET_SHORTCUT(action, working.userState),
    }
  } else {
    // Process action on the JS side.
    const editorAfterUpdateFunction = runLocalEditorAction(
      working.editor,
      working.derived,
      working.userState,
      working.workers,
      action as EditorAction,
      workingHistory,
      dispatchEvent,
      spyCollector,
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
      userState: working.userState,
      workers: working.workers,
      dispatch: dispatchEvent,
    }
  }
}

function processActions(
  dispatchEvent: EditorDispatch,
  working: EditorStore,
  actions: Array<EditorAction>,
  spyCollector: UiJsxCanvasContextData,
): EditorStore {
  return actions.reduce((workingFuture: EditorStore, action: EditorAction) => {
    return processAction(dispatchEvent, workingFuture, action, spyCollector)
  }, working)
}

export function updateEmbeddedPreview(
  modelId: string | null,
  projectContents: ProjectContentTreeRoot,
): void {
  const embeddedPreviewElement = document.getElementById(PreviewIframeId)
  if (embeddedPreviewElement != null) {
    const embeddedPreviewIframe = (embeddedPreviewElement as any) as HTMLIFrameElement
    const contentWindow = embeddedPreviewIframe.contentWindow
    if (contentWindow != null) {
      try {
        contentWindow.postMessage(projectContentsUpdateMessage(projectContents), '*')
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
  spyCollector: UiJsxCanvasContextData,
): DispatchResult {
  const isLoadAction = dispatchedActions.some((a) => a.action === 'LOAD')
  const nameUpdated = dispatchedActions.some((action) => action.action === 'SET_PROJECT_NAME')
  const forceSave =
    nameUpdated || dispatchedActions.some((action) => action.action === 'SAVE_CURRENT_FILE')
  const onlyNameUpdated = nameUpdated && dispatchedActions.length === 1
  const allTransient = dispatchedActions.every(isTransientAction)
  const anyFinishCheckpointTimer = dispatchedActions.some((action) => {
    return action.action === 'FINISH_CHECKPOINT_TIMER'
  })
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
      return editorDispatchInner(boundDispatch, actions, working, allTransient, spyCollector)
    },
    { ...storedState, entireUpdateFinished: Promise.resolve(true), nothingChanged: true },
  )

  // The FINISH_CHECKPOINT_TIMER action effectively overrides the case where nothing changed,
  // as it's likely that action on it's own didn't change anything, but the actions that paired with
  // START_CHECKPOINT_TIMER likely did.
  const transientOrNoChange = (allTransient || result.nothingChanged) && !anyFinishCheckpointTimer
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
    (!transientOrNoChange || anyUndoOrRedo || updateCodeResultCache || updateCodeEditorErrors) &&
    isBrowserEnvironment
  if (shouldSave) {
    save(frozenEditorState, boundDispatch, storedState.userState.loginState, saveType, forceSave)
    const stateToStore = storedEditorStateFromEditorState(storedState.editor)
    saveStoredState(storedState.editor.id, stateToStore)
    notifyTsWorker(frozenEditorState, storedState.editor, storedState.workers)
  }

  if (nameUpdated && frozenEditorState.id != null) {
    pushProjectURLToBrowserHistory(
      `Utopia ${frozenEditorState.projectName}`,
      frozenEditorState.id,
      frozenEditorState.projectName,
    )
  }

  const shouldUpdatePreview =
    anySendPreviewModel || frozenEditorState.projectContents !== storedState.editor.projectContents
  if (shouldUpdatePreview) {
    updateEmbeddedPreview(frozenEditorState.id, frozenEditorState.projectContents)
  }

  if (frozenEditorState.id != null && frozenEditorState.id != storedState.editor.id) {
    storedState.workers.initWatchdogWorker(frozenEditorState.id)
  }

  return {
    editor: frozenEditorState,
    derived: frozenDerivedState,
    history: newHistory,
    userState: result.userState,
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
  spyCollector: UiJsxCanvasContextData,
): DispatchResult {
  // console.log('DISPATCH', simpleStringifyActions(dispatchedActions))

  if (!PRODUCTION_ENV && typeof window.performance.mark === 'function') {
    window.performance.mark('dispatch_begin')
  }
  if (dispatchedActions.length > 0) {
    // Run everything in a big chain.
    let result = processActions(boundDispatch, storedState, dispatchedActions, spyCollector)

    const anyUndoOrRedo = R.any(isUndoOrRedo, dispatchedActions)

    if (!PRODUCTION_ENV && typeof window.performance.mark === 'function') {
      window.performance.mark('derived_state_begin')
    }

    const editorStayedTheSame =
      storedState.nothingChanged &&
      storedState.editor === result.editor &&
      storedState.userState === result.userState

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
        // TODO Re-enable this once we have addressed the root cause of the false positives
        // (these were firing frequently even on elements that remained > 0x0 dimensions)
        //
        // const allLostElements = lostElements(r.editor.selectedViews, r.editor.jsxMetadataKILLME)
        // const newLostElements = TP.filterPaths(allLostElements, r.editor.warnedInstances)
        // if (newLostElements.length > 0 && isBrowserEnvironment) {
        //   // FIXME The above `isBrowserEnvironment` check is required because this is tripped by tests that don't update the metadata
        //   // correctly. Rather than preventing this code running during tests, we should make sure tests are all updating metadata correctly.
        //   const toastAction = EditorActions.showToast({
        //     message: `Some elements are no longer being rendered`,
        //     level: 'WARNING',
        //   })
        //   setTimeout(() => boundDispatch([toastAction], 'everyone'), 0)
        // }

        // r.editor.warnedInstances = allLostElements
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
      userState: result.userState,
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
    codeResultCache: {
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
  const modelChange =
    saveType === 'model' || saveType === 'both' ? persistentModelFromEditorModel(state) : null
  const nameChange = saveType === 'name' || saveType === 'both' ? state.projectName : null
  try {
    if (isLoggedIn(loginState)) {
      saveToServer(dispatch, state.id, state.projectName, modelChange, nameChange, forceServerSave)
    } else {
      saveToLocalStorage(dispatch, state.id, state.projectName, modelChange, nameChange)
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
  walkContentsTree(newEditorState.projectContents, (filename, file) => {
    const oldFile = getContentsTreeFileFromString(oldEditorState.projectContents, filename)
    if (oldFile == null) {
      shouldInitTsWorker = true
    } else {
      if (file != null && isCodeOrUiJsFile(file) && oldFile != file) {
        filesToUpdateInTsWorker.push(filename)
      }
    }
  })
  // notify the ts worker if any file has been removed compared to the previous state
  walkContentsTree(oldEditorState.projectContents, (filename, _) => {
    const file = getContentsTreeFileFromString(newEditorState.projectContents, filename)
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
      const file = getContentsTreeFileFromString(newEditorState.projectContents, filename)
      if (
        file != null &&
        isCodeOrUiJsFile(file) &&
        (isJsOrTsFile(filename) || isCssFile(filename))
      ) {
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
