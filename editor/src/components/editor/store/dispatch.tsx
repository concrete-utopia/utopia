import * as deepEquals from 'fast-deep-equal'
import { produce } from 'immer'
import * as React from 'react'
import * as ReactDOMServer from 'react-dom/server'
import { PRODUCTION_ENV } from '../../../common/env-vars'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { getAllUniqueUids } from '../../../core/model/element-template-utils'
import {
  ElementPath,
  isParseSuccess,
  isTextFile,
  ParseSuccess,
  ProjectFile,
} from '../../../core/shared/project-file-types'
import {
  codeNeedsParsing,
  codeNeedsPrinting,
} from '../../../core/workers/common/project-file-utils'
import { getParseResult } from '../../../core/workers/parser-printer/parser-printer'
import { isJsOrTsFile, isCssFile, MultiFileBuildResult } from '../../../core/workers/ts/ts-worker'
import { UtopiaTsWorkers } from '../../../core/workers/common/worker-types'
import { runLocalCanvasAction } from '../../../templates/editor-canvas'
import { runLocalNavigatorAction } from '../../../templates/editor-navigator'
import { optionalDeepFreeze } from '../../../utils/deep-freeze'
import { bimapEither } from '../../../core/shared/either'
import Utils from '../../../utils/utils'
import { CanvasAction } from '../../canvas/canvas-types'
import { LocalNavigatorAction } from '../../navigator/actions'
import { PreviewIframeId, projectContentsUpdateMessage } from '../../preview/preview-pane'
import * as EP from '../../../core/shared/element-path'
import { EditorAction, EditorDispatch, isLoggedIn, LoginState } from '../action-types'
import {
  isTransientAction,
  isUndoOrRedo,
  isParsedModelUpdate,
  isFromVSCode,
} from '../actions/action-utils'
import * as EditorActions from '../actions/action-creators'
import * as History from '../history'
import { StateHistory } from '../history'
import {
  saveToLocalStorage,
  saveToServer,
  pushProjectURLToBrowserHistory,
  SaveType,
  save,
} from '../persistence'
import { saveStoredState } from '../stored-state'
import {
  DerivedState,
  deriveState,
  EditorState,
  EditorStore,
  getAllBuildErrors,
  getAllErrorsFromFiles,
  getAllLintErrors,
  getHighlightBoundsForUids,
  persistentModelFromEditorModel,
  reconstructJSXMetadata,
  storedEditorStateFromEditorState,
} from './editor-state'
import { runLocalEditorAction } from './editor-update'
import { arrayEquals, isBrowserEnvironment } from '../../../core/shared/utils'
import {
  EvaluationCache,
  getDependencyTypeDefinitions,
} from '../../../core/es-modules/package-manager/package-manager'
import { UiJsxCanvasContextData } from '../../canvas/ui-jsx-canvas'
import {
  getContentsTreeFileFromString,
  getProjectFileFromTree,
  isProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
  treeToContents,
  walkContentsTree,
  zipContentsTree,
} from '../../assets'
import { isSendPreviewModel, restoreDerivedState, UPDATE_FNS } from '../actions/actions'
import { ElementPathArrayKeepDeepEquality } from '../../../utils/deep-equality-instances'
import { mapDropNulls } from '../../../core/shared/array-utils'
import {
  createParseFile,
  createPrintCode,
  ParseOrPrint,
} from '../../../core/workers/parser-printer/parser-printer-worker'
import {
  getTransitiveReverseDependencies,
  identifyFilesThatHaveChanged,
} from '../../../core/shared/project-contents-dependencies'
import { CodeResultCache, generateCodeResultCache } from '../../custom-code/code-file'
import {
  reduxDevtoolsLogMessage,
  reduxDevtoolsSendActions,
  reduxDevtoolsUpdateState,
} from '../../../core/shared/redux-devtools'
import { pick } from '../../../core/shared/object-utils'
import {
  getSavedCodeFromTextFile,
  getUnsavedCodeFromTextFile,
} from '../../../core/model/project-file-utils'
import {
  AccumulatedVSCodeChanges,
  emptyAccumulatedVSCodeChanges,
  combineAccumulatedVSCodeChanges,
  getVSCodeChanges,
  sendVSCodeChanges,
} from './vscode-changes'

export interface DispatchResult extends EditorStore {
  nothingChanged: boolean
  entireUpdateFinished: Promise<any>
}

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
      userState: UPDATE_FNS.SET_SHORTCUT(action, working.userState),
    }
  } else if (action.action === 'SET_LOGIN_STATE') {
    return {
      ...working,
      userState: UPDATE_FNS.SET_LOGIN_STATE(action, working.userState),
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
        const derivedState = deriveState(editorAfterNavigator, null)
        newStateHistory = History.init(editorAfterNavigator, derivedState)
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
      alreadySaved: working.alreadySaved,
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
  projectContents: ProjectContentTreeRoot,
  workers: UtopiaTsWorkers,
  dispatch: EditorDispatch,
): { modelUpdateRequested: boolean; parseOrPrintFinished: Promise<boolean> } {
  // Walk the project contents to see if anything needs to be sent across.
  let filesToUpdate: Array<ParseOrPrint> = []
  walkContentsTree(projectContents, (fullPath, file) => {
    if (isTextFile(file)) {
      if (codeNeedsParsing(file.fileContents.revisionsState)) {
        const lastParseSuccess = isParseSuccess(file.fileContents.parsed)
          ? file.fileContents.parsed
          : file.lastParseSuccess
        filesToUpdate.push(
          createParseFile(fullPath, file.fileContents.code, lastParseSuccess, file.lastRevisedTime),
        )
      } else if (
        codeNeedsPrinting(file.fileContents.revisionsState) &&
        isParseSuccess(file.fileContents.parsed)
      ) {
        filesToUpdate.push(
          createPrintCode(fullPath, file.fileContents.parsed, PRODUCTION_ENV, file.lastRevisedTime),
        )
      }
    }
  })

  // Should anything need to be sent across, do so here.
  if (filesToUpdate.length > 0) {
    const parseFinished = getParseResult(workers, filesToUpdate)
      .then((parseResult) => {
        const updates = parseResult.map((fileResult) => {
          switch (fileResult.type) {
            case 'parsefileresult':
              return EditorActions.workerParsedUpdate(
                fileResult.filename,
                fileResult.parseResult,
                fileResult.lastRevisedTime,
              )
            case 'printcoderesult':
              return EditorActions.workerCodeUpdate(
                fileResult.filename,
                fileResult.printResult,
                fileResult.highlightBounds,
                fileResult.lastRevisedTime,
              )
            default:
              const _exhaustiveCheck: never = fileResult
              throw new Error(`Unhandled file result ${JSON.stringify(fileResult)}`)
          }
        })

        dispatch([EditorActions.updateFromWorker(updates)])
        return true
      })
      .catch((e) => {
        console.error('error during parse', e)
        dispatch([EditorActions.clearParseOrPrintInFlight()])
        return true
      })
    return { modelUpdateRequested: true, parseOrPrintFinished: parseFinished }
  } else {
    return { modelUpdateRequested: false, parseOrPrintFinished: Promise.resolve(true) }
  }
}

function maybeRequestModelUpdateOnEditor(
  editor: EditorState,
  workers: UtopiaTsWorkers,
  dispatch: EditorDispatch,
): { editorState: EditorState; modelUpdateFinished: Promise<boolean> } {
  if (editor.parseOrPrintInFlight) {
    // Prevent repeated requests
    return { editorState: editor, modelUpdateFinished: Promise.resolve(true) }
  } else {
    const modelUpdateRequested = maybeRequestModelUpdate(editor.projectContents, workers, dispatch)
    return {
      editorState: {
        ...editor,
        parseOrPrintInFlight: modelUpdateRequested.modelUpdateRequested,
      },
      modelUpdateFinished: modelUpdateRequested.parseOrPrintFinished,
    }
  }
}

let currentVSCodeChanges: AccumulatedVSCodeChanges = emptyAccumulatedVSCodeChanges
let applyProjectChangesCoordinator: Promise<void> = Promise.resolve()

export function editorDispatch(
  boundDispatch: EditorDispatch,
  dispatchedActions: readonly EditorAction[],
  storedState: EditorStore,
  spyCollector: UiJsxCanvasContextData,
): DispatchResult {
  const isLoadAction = dispatchedActions.some((a) => a.action === 'LOAD')
  const nameUpdated = dispatchedActions.some(
    (action) => action.action === 'SET_PROJECT_NAME' || action.action === 'SET_PROJECT_ID',
  )
  const forceSave =
    nameUpdated || dispatchedActions.some((action) => action.action === 'SAVE_CURRENT_FILE')
  const onlyNameUpdated = nameUpdated && dispatchedActions.length === 1
  const allTransient = dispatchedActions.every(isTransientAction)
  const anyFinishCheckpointTimer = dispatchedActions.some((action) => {
    return action.action === 'FINISH_CHECKPOINT_TIMER'
  })
  const anyWorkerUpdates = dispatchedActions.some(
    (action) => action.action === 'UPDATE_FROM_WORKER',
  )
  const anyUndoOrRedo = dispatchedActions.some(isUndoOrRedo)
  const anySendPreviewModel = dispatchedActions.some(isSendPreviewModel)

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
      const newStore = editorDispatchInner(
        boundDispatch,
        actions,
        working,
        allTransient,
        spyCollector,
      )
      return newStore
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

  const alreadySaved = result.alreadySaved

  const isLoaded = frozenEditorState.isLoaded
  const shouldSave =
    isLoaded &&
    !isLoadAction &&
    (!transientOrNoChange || anyUndoOrRedo || (anyWorkerUpdates && alreadySaved)) &&
    isBrowserEnvironment

  const finalStore = {
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
    alreadySaved: alreadySaved || shouldSave,
  }

  if (!finalStore.nothingChanged) {
    /**
     * Heads up: we do not log dispatches that resulted in a NO_OP. This is to avoid clogging up the
     * history with a million CLEAR_HIGHLIGHTED_VIEWS and other such actions.
     *  */

    reduxDevtoolsSendActions(actionGroupsToProcess, finalStore)
  }

  if (shouldSave) {
    save(frozenEditorState, boundDispatch, storedState.userState.loginState, saveType, forceSave)
    const stateToStore = storedEditorStateFromEditorState(storedState.editor)
    saveStoredState(storedState.editor.id, stateToStore)
    notifyTsWorker(frozenEditorState, storedState.editor, storedState.workers)
    reduxDevtoolsUpdateState('Save Editor', finalStore)
  }

  const updatedFromVSCode = dispatchedActions.some(isFromVSCode)
  if (updatedFromVSCode && !dispatchedActions.every(isFromVSCode)) {
    console.error(
      `VS Code actions mixed with Utopia actions`,
      simpleStringifyActions(dispatchedActions),
    )
  }

  // Chain off of the previous one to ensure the ordering is maintained.
  currentVSCodeChanges = combineAccumulatedVSCodeChanges(
    currentVSCodeChanges,
    getVSCodeChanges(storedState.editor, frozenEditorState, updatedFromVSCode),
  )
  applyProjectChangesCoordinator = applyProjectChangesCoordinator.then(async () => {
    const changesToSend = currentVSCodeChanges
    currentVSCodeChanges = emptyAccumulatedVSCodeChanges
    return sendVSCodeChanges(changesToSend).catch((error) => {
      console.error('Error sending updates to VS Code', error)
    })
  })

  if ((isLoadAction || nameUpdated) && frozenEditorState.id != null) {
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

  return finalStore
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

    const anyUndoOrRedo = dispatchedActions.some(isUndoOrRedo)

    if (!PRODUCTION_ENV && typeof window.performance.mark === 'function') {
      window.performance.mark('derived_state_begin')
    }

    const editorStayedTheSame =
      storedState.nothingChanged &&
      storedState.editor === result.editor &&
      storedState.userState === result.userState

    const domMetadataChanged = storedState.editor.domMetadata !== result.editor.domMetadata
    const spyMetadataChanged = storedState.editor.spyMetadata !== result.editor.spyMetadata
    const dragStateLost =
      storedState.editor.canvas.dragState != null && result.editor.canvas.dragState == null
    const metadataChanged = domMetadataChanged || spyMetadataChanged || dragStateLost
    // TODO: Should this condition actually be `&&`?
    // Tested quickly and it broke selection, but I'm mostly certain
    // it should only merge when both have changed.
    if (metadataChanged) {
      if (result.editor.canvas.dragState == null) {
        result = {
          ...result,
          editor: {
            ...result.editor,
            jsxMetadata: reconstructJSXMetadata(result.editor),
          },
        }
      } else {
        result = {
          ...result,
          editor: {
            ...result.editor,
            canvas: {
              ...result.editor.canvas,
              dragState: {
                ...result.editor.canvas.dragState,
                metadata: reconstructJSXMetadata(result.editor),
              },
            },
          },
        }
      }
    }

    let frozenEditorState: EditorState = optionalDeepFreeze(result.editor)

    let frozenDerivedState: DerivedState
    if (anyUndoOrRedo) {
      frozenDerivedState = optionalDeepFreeze(restoreDerivedState(result.history))
      // TODO BB put inspector and navigator back to history
    } else if (editorStayedTheSame) {
      // !! We completely skip creating a new derived state, since the editor state stayed the exact same
      frozenDerivedState = storedState.derived
    } else {
      const derivedState = deriveState(frozenEditorState, storedState.derived)
      frozenDerivedState = optionalDeepFreeze(derivedState)
    }

    const actionNames = dispatchedActions.map((action) => action.action).join(',')
    getAllUniqueUids(frozenEditorState.projectContents, actionNames)

    if (!PRODUCTION_ENV) {
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
      alreadySaved: storedState.alreadySaved,
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
  // FIXME: Reimplement this in a way that doesn't require converting from `ProjectContents`.
  const projectContents = treeToContents(editor.projectContents)
  const allFiles = Object.keys(projectContents)
  return {
    ...editor,
    codeResultCache: {
      ...editor.codeResultCache,
      cache: pick(allFiles, editor.codeResultCache.cache),
    },
    codeEditorErrors: {
      buildErrors: pick(allFiles, editor.codeEditorErrors.buildErrors),
      lintErrors: pick(allFiles, editor.codeEditorErrors.lintErrors),
    },
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
      if (file != null && isTextFile(file) && oldFile != file) {
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
      if (file != null && isTextFile(file) && (isJsOrTsFile(filename) || isCssFile(filename))) {
        workers.sendUpdateFileMessage(filename, file, true)
      }
    })
  }
}

function removeNonExistingViewReferencesFromState(editorState: EditorState): EditorState {
  const rootComponents = editorState.jsxMetadata
  const updatedSelectedViews = ElementPathArrayKeepDeepEquality(
    editorState.selectedViews,
    mapDropNulls(
      (selectedView) => elementPathStillExists(rootComponents, selectedView),
      editorState.selectedViews,
    ),
  ).value
  const updatedHighlightedViews = ElementPathArrayKeepDeepEquality(
    editorState.highlightedViews,
    mapDropNulls(
      (highlightedView) => elementPathStillExists(rootComponents, highlightedView),
      editorState.highlightedViews,
    ),
  ).value
  const updatedHiddenInstances = ElementPathArrayKeepDeepEquality(
    editorState.hiddenInstances,
    mapDropNulls(
      (hiddenInstance) => elementPathStillExists(rootComponents, hiddenInstance),
      editorState.hiddenInstances,
    ),
  ).value
  return {
    ...editorState,
    selectedViews: updatedSelectedViews,
    highlightedViews: updatedHighlightedViews,
    hiddenInstances: updatedHiddenInstances,
  }
}

function elementPathStillExists(
  newComponents: ElementInstanceMetadataMap,
  pathToUpdate: ElementPath,
): ElementPath | null {
  const pathStillExists =
    MetadataUtils.findElementByElementPath(newComponents, pathToUpdate) != null
  if (pathStillExists) {
    return pathToUpdate
  } else {
    return null
  }
}
