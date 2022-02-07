import { PERFORMANCE_MARKS_ALLOWED, PRODUCTION_ENV } from '../../../common/env-vars'
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
import {
  createParseFile,
  createPrintCode,
  getParseResult,
  ParseOrPrint,
  UtopiaTsWorkers,
} from '../../../core/workers/common/worker-types'
import { runLocalCanvasAction } from '../../../templates/editor-canvas'
import { runLocalNavigatorAction } from '../../../templates/editor-navigator'
import { optionalDeepFreeze } from '../../../utils/deep-freeze'
import Utils from '../../../utils/utils'
import { CanvasAction } from '../../canvas/canvas-types'
import { LocalNavigatorAction } from '../../navigator/actions'
import { PreviewIframeId, projectContentsUpdateMessage } from '../../preview/preview-pane'
import { EditorAction, EditorDispatch, isLoggedIn, LoginState } from '../action-types'
import {
  isTransientAction,
  isUndoOrRedo,
  isParsedModelUpdate,
  isFromVSCode,
  isClearInteractionState,
  shouldApplyClearInteractionStateResult,
  strategyWasOverridden,
} from '../actions/action-utils'
import * as EditorActions from '../actions/action-creators'
import * as History from '../history'
import { StateHistory } from '../history'
import { saveStoredState } from '../stored-state'
import {
  DerivedState,
  deriveState,
  EditorState,
  EditorStatePatch,
  EditorStore,
  getAllBuildErrors,
  getAllErrorsFromFiles,
  getAllLintErrors,
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
  getTransitiveReverseDependencies,
  identifyFilesThatHaveChanged,
} from '../../../core/shared/project-contents-dependencies'
import {
  reduxDevtoolsLogMessage,
  reduxDevtoolsSendActions,
  reduxDevtoolsUpdateState,
} from '../../../core/shared/redux-devtools'
import { pick } from '../../../core/shared/object-utils'
import {
  ProjectChanges,
  emptyProjectChanges,
  combineProjectChanges,
  getProjectChanges,
  sendVSCodeChanges,
  WriteProjectFileChange,
  ProjectFileChange,
} from './vscode-changes'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { isJsOrTsFile, isCssFile } from '../../../core/shared/file-utils'
import {
  applyStatePatches,
  CanvasCommand,
  foldCommands,
  strategySwitched,
} from '../../canvas/commands/commands'
import {
  applyCanvasStrategy,
  findCanvasStrategy,
  getStrategyByName,
  strategiesPartOfSameGroup,
  strategySwitchInteractionStateReset,
} from '../../canvas/canvas-strategies/canvas-strategies'
import {
  CanvasState,
  createEmptySessionStateState,
  SessionStateState,
} from '../../../interactions_proposal'
import { c } from 'tar'

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
      working.unpatchedEditor,
      working.derived,
      working.userState,
      working.workers,
      action as EditorAction,
      workingHistory,
      dispatchEvent,
      spyCollector,
      working.builtInDependencies,
    )
    const editorAfterCanvas = runLocalCanvasAction(
      dispatchEvent,
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
      unpatchedEditor: editorAfterNavigator,
      editor: editorAfterNavigator,
      derived: working.derived,
      sessionStateState: working.sessionStateState, // this means the actions cannot update sessionStateState – this piece of state lives outside our "redux" state
      history: newStateHistory,
      userState: working.userState,
      workers: working.workers,
      persistence: working.persistence,
      dispatch: dispatchEvent,
      alreadySaved: working.alreadySaved,
      builtInDependencies: working.builtInDependencies,
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
  forceParseFiles: Array<string>,
  dispatch: EditorDispatch,
): {
  modelUpdateRequested: boolean
  parseOrPrintFinished: Promise<boolean>
  forciblyParsedFiles: Array<string>
} {
  // Walk the project contents to see if anything needs to be sent across.
  let filesToUpdate: Array<ParseOrPrint> = []
  let forciblyParsedFiles: Array<string> = []
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
      } else if (forceParseFiles.includes(fullPath)) {
        forciblyParsedFiles.push(fullPath)
        const lastParseSuccess = isParseSuccess(file.fileContents.parsed)
          ? file.fileContents.parsed
          : file.lastParseSuccess
        filesToUpdate.push(
          createParseFile(fullPath, file.fileContents.code, lastParseSuccess, file.lastRevisedTime),
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
    return {
      modelUpdateRequested: true,
      parseOrPrintFinished: parseFinished,
      forciblyParsedFiles: forciblyParsedFiles,
    }
  } else {
    return {
      modelUpdateRequested: false,
      parseOrPrintFinished: Promise.resolve(true),
      forciblyParsedFiles: forciblyParsedFiles,
    }
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
    const modelUpdateRequested = maybeRequestModelUpdate(
      editor.projectContents,
      workers,
      editor.forceParseFiles,
      dispatch,
    )
    const remainingForceParseFiles = editor.forceParseFiles.filter(
      (filePath) => !modelUpdateRequested.forciblyParsedFiles.includes(filePath),
    )
    return {
      editorState: {
        ...editor,
        parseOrPrintInFlight: modelUpdateRequested.modelUpdateRequested,
        forceParseFiles: remainingForceParseFiles,
      },
      modelUpdateFinished: modelUpdateRequested.parseOrPrintFinished,
    }
  }
}

let accumulatedProjectChanges: ProjectChanges = emptyProjectChanges
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
    nameUpdated ||
    dispatchedActions.some((action) => action.action === 'SAVE_CURRENT_FILE') ||
    dispatchedActions.some(
      (action) => action.action === 'UPDATE_FROM_CODE_EDITOR' && action.unsavedContent == null,
    )
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

  let frozenEditorState = editorFilteredForFiles
  const frozenDerivedState = result.derived

  let newHistory: StateHistory
  if (transientOrNoChange) {
    newHistory = result.history
  } else {
    newHistory = History.add(result.history, frozenEditorState, frozenDerivedState)
  }

  const alreadySaved = result.alreadySaved

  const isLoaded = frozenEditorState.isLoaded
  const shouldSave =
    isLoaded &&
    !isLoadAction &&
    (!transientOrNoChange || anyUndoOrRedo || (anyWorkerUpdates && alreadySaved)) &&
    isBrowserEnvironment

  // Create commands from the interaction state.
  let patchCommands: Array<CanvasCommand> = []
  if (
    frozenEditorState.canvas.interactionState != null &&
    (frozenDerivedState.canvas.transientState.selectedViews != null ||
      frozenDerivedState.canvas.transientState.filesState != null) &&
    isFeatureEnabled('Canvas Strategies') // only throw error if Canvas Strategies are enabled to begin with, to allow an escape hatch for insertion
  ) {
    throw new Error('transient canvas state is not allowed while an interaction state is active')
  }
  const clearInteractionStateActionDispatched = dispatchedActions.some(isClearInteractionState)
  const shouldApplyChanges = dispatchedActions.some(shouldApplyClearInteractionStateResult)
  const shouldDiscardChanges = clearInteractionStateActionDispatched && !shouldApplyChanges
  let strategyName: string | null = null
  let strategyChanged: boolean = false
  let partOfSameGroup: boolean = false

  let didResetInteractionData: boolean = false // please if someone is changing the code around strategySwitchInteractionStateReset, make me nicer and not a variable floating around
  if (frozenEditorState.canvas.interactionState != null) {
    const commandResultCurrent = foldCommands(
      frozenEditorState,
      result.sessionStateState,
      [...result.sessionStateState.accumulatedCommands],
      shouldApplyChanges ? 'permanent' : 'transient',
    )
    const patchedEditorStateCurrent = applyStatePatches(
      frozenEditorState,
      storedState.editor,
      shouldDiscardChanges ? [] : commandResultCurrent.editorStatePatches,
    )

    const canvasState: CanvasState = {
      selectedElements: patchedEditorStateCurrent.selectedViews,
      metadata: patchedEditorStateCurrent.jsxMetadata,
      projectContents: patchedEditorStateCurrent.projectContents,
      openFile: patchedEditorStateCurrent.canvas.openFile?.filename,
      scale: patchedEditorStateCurrent.canvas.scale,
      canvasOffset: patchedEditorStateCurrent.canvas.roundedCanvasOffset,
    }

    const strategy = findCanvasStrategy(
      canvasState,
      frozenEditorState.canvas.interactionState,
      result.sessionStateState,
    )
    strategyName = strategy?.name ?? null

    strategyChanged = strategyName != result.sessionStateState.currentStrategy
    partOfSameGroup = strategiesPartOfSameGroup(
      result.sessionStateState.currentStrategy,
      strategyName,
    )

    if (strategyChanged && !partOfSameGroup) {
      didResetInteractionData = true
      frozenEditorState = {
        ...frozenEditorState,
        canvas: {
          ...frozenEditorState.canvas,
          interactionState: strategySwitchInteractionStateReset(
            frozenEditorState.canvas.interactionState,
          ),
        },
      }
    }
    if (strategy != null && frozenEditorState.canvas.interactionState != null) {
      const commands = applyCanvasStrategy(
        strategy,
        canvasState,
        frozenEditorState.canvas.interactionState,
        result.sessionStateState,
      )
      patchCommands = commands
    }
  }

  const strategyHasBeenOverriden = dispatchedActions.some(strategyWasOverridden)
  const shouldKeepCommands = strategyChanged && !strategyHasBeenOverriden && !partOfSameGroup // TODO if the user deliberately changes the strategy, make sure we don't keep any commands around
  const strategyChangedLogCommand = strategyChanged
    ? [
        strategySwitched(
          strategyHasBeenOverriden ? 'user-input' : 'automatic',
          strategyName!,
          shouldKeepCommands,
          didResetInteractionData,
        ),
      ]
    : []
  const updatedAccumulatedCommands = shouldKeepCommands
    ? [
        ...result.sessionStateState.accumulatedCommands,
        ...result.sessionStateState.currentStrategyCommands,
        ...strategyChangedLogCommand,
      ]
    : [...result.sessionStateState.accumulatedCommands, ...strategyChangedLogCommand]

  const workingSessionStateState: SessionStateState = {
    currentStrategy: strategyName,
    currentStrategyCommands: patchCommands,
    accumulatedCommands: updatedAccumulatedCommands,
    commandDescriptions: [],
    strategyState: result.sessionStateState.strategyState,
    startingMetadata: result.sessionStateState.startingMetadata,
  }

  const commandResult = foldCommands(
    frozenEditorState,
    workingSessionStateState,
    [
      ...workingSessionStateState.accumulatedCommands,
      ...workingSessionStateState.currentStrategyCommands,
    ],
    shouldApplyChanges ? 'permanent' : 'transient',
  )

  // FIXME if shouldDiscardChanges, should this just become the previous unpatchedEditorState?
  const patchedEditorState = applyStatePatches(
    frozenEditorState,
    storedState.editor,
    shouldDiscardChanges ? [] : commandResult.editorStatePatches,
  )

  let newSessionStateState: SessionStateState = clearInteractionStateActionDispatched
    ? createEmptySessionStateState() // QUESTION should we make this NULL instead?
    : {
        ...workingSessionStateState,
        strategyState: commandResult.newStrategyState,
        commandDescriptions: commandResult.commandDescriptions,
      }

  // Should the strategy be changed, checkpoint the metadata into `startingMetadata` for
  // future reference by the strategies.
  if (strategyChanged) {
    // TODO if the user deliberately changes the strategy, do not reset startingMetadata
    newSessionStateState = {
      ...newSessionStateState,
      startingMetadata: frozenEditorState.jsxMetadata,
    }
  }

  const finalStore: DispatchResult = {
    unpatchedEditor: shouldApplyChanges ? patchedEditorState : frozenEditorState,
    editor: patchedEditorState,
    derived: frozenDerivedState,
    sessionStateState: optionalDeepFreeze(newSessionStateState),
    history: newHistory,
    userState: result.userState,
    workers: storedState.workers,
    persistence: storedState.persistence,
    dispatch: boundDispatch,
    nothingChanged: result.nothingChanged,
    entireUpdateFinished: Promise.all([
      result.entireUpdateFinished,
      editorWithModelChecked.modelUpdateFinished,
    ]),
    alreadySaved: alreadySaved || shouldSave,
    builtInDependencies: storedState.builtInDependencies,
  }

  if (!finalStore.nothingChanged) {
    /**
     * Heads up: we do not log dispatches that resulted in a NO_OP. This is to avoid clogging up the
     * history with a million CLEAR_HIGHLIGHTED_VIEWS and other such actions.
     *  */

    reduxDevtoolsSendActions(actionGroupsToProcess, finalStore)
  }

  if (storedState.userState.loginState.type !== result.userState.loginState.type) {
    if (isLoggedIn(result.userState.loginState)) {
      storedState.persistence.login()
    } else {
      storedState.persistence.logout()
    }
  }

  if (shouldSave) {
    storedState.persistence.save(
      frozenEditorState.projectName,
      persistentModelFromEditorModel(frozenEditorState),
      forceSave ? 'force' : 'throttle',
    )
    const stateToStore = storedEditorStateFromEditorState(storedState.editor)
    saveStoredState(storedState.editor.id, stateToStore)
    reduxDevtoolsUpdateState('Save Editor', finalStore)
  }

  const updatedFromVSCode = dispatchedActions.some(isFromVSCode)
  if (updatedFromVSCode && !dispatchedActions.every(isFromVSCode)) {
    console.error(
      `VS Code actions mixed with Utopia actions`,
      simpleStringifyActions(dispatchedActions),
    )
  }

  const projectChanges = getProjectChanges(storedState.editor, frozenEditorState)
  applyProjectChanges(frozenEditorState, projectChanges, updatedFromVSCode)

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

function applyProjectChanges(
  frozenEditorState: EditorState,
  projectChanges: ProjectChanges,
  updatedFromVSCode: boolean,
) {
  accumulatedProjectChanges = combineProjectChanges(
    accumulatedProjectChanges,
    updatedFromVSCode ? { ...projectChanges, fileChanges: [] } : projectChanges,
  )

  if (frozenEditorState.vscodeReady) {
    // Chain off of the previous one to ensure the ordering is maintained.
    applyProjectChangesCoordinator = applyProjectChangesCoordinator.then(async () => {
      const changesToSend = accumulatedProjectChanges
      accumulatedProjectChanges = emptyProjectChanges
      return sendVSCodeChanges(changesToSend).catch((error) => {
        console.error('Error sending updates to VS Code', error)
      })
    })
  }

  const updatedFileNames = projectChanges.fileChanges.map((fileChange) => fileChange.fullPath)
  const updatedAndReverseDepFilenames = getTransitiveReverseDependencies(
    frozenEditorState.projectContents,
    frozenEditorState.nodeModules.files,
    updatedFileNames,
  )

  // Mutating the evaluation cache.
  for (const fileToDelete of updatedAndReverseDepFilenames) {
    delete frozenEditorState.codeResultCache.evaluationCache[fileToDelete]
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

  const MeasureDispatchTime =
    isFeatureEnabled('Debug mode – Performance Marks') && PERFORMANCE_MARKS_ALLOWED

  if (MeasureDispatchTime) {
    window.performance.mark('dispatch_begin')
  }
  if (dispatchedActions.length > 0) {
    // Run everything in a big chain.
    let result = processActions(boundDispatch, storedState, dispatchedActions, spyCollector)

    const anyUndoOrRedo = dispatchedActions.some(isUndoOrRedo)

    if (MeasureDispatchTime) {
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
      if (result.editor.canvas.dragState != null && 'metadata' in result.editor.canvas.dragState) {
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
      } else {
        result = {
          ...result,
          editor: {
            ...result.editor,
            jsxMetadata: reconstructJSXMetadata(result.editor),
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

    if (MeasureDispatchTime) {
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

    return {
      unpatchedEditor: frozenEditorState,
      editor: frozenEditorState,
      derived: frozenDerivedState,
      sessionStateState: result.sessionStateState,
      history: result.history,
      userState: result.userState,
      workers: storedState.workers,
      persistence: storedState.persistence,
      dispatch: boundDispatch,
      nothingChanged: editorStayedTheSame,
      entireUpdateFinished: Promise.all([storedState.entireUpdateFinished]),
      alreadySaved: storedState.alreadySaved,
      builtInDependencies: storedState.builtInDependencies,
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
