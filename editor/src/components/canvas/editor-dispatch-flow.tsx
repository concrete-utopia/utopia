import type { EditorDispatch } from '../editor/action-types'
import { updateMetadataInEditorState } from '../editor/actions/action-creators'
import type { DispatchResult } from '../editor/store/dispatch'
import { editorDispatchActionRunner } from '../editor/store/dispatch'
import type { EditorStoreFull } from '../editor/store/editor-state'
import { runDomSamplerRegular } from './dom-sampler'
import { resubscribeObservers } from './dom-walker'
import { ElementsToRerenderGLOBAL, type UiJsxCanvasContextData } from './ui-jsx-canvas'

export function carryDispatchResultFields(
  firstDispatchResult: DispatchResult,
  secondDispatchResult: DispatchResult,
): DispatchResult {
  const nothingChanged = firstDispatchResult.nothingChanged && secondDispatchResult.nothingChanged
  const entireUpdateFinished = Promise.all([
    firstDispatchResult.entireUpdateFinished,
    secondDispatchResult.entireUpdateFinished,
  ])
  return {
    ...secondDispatchResult,
    nothingChanged: nothingChanged,
    entireUpdateFinished: entireUpdateFinished,
  }
}

export function runDomSamplerAndSaveResults(
  boundDispatch: EditorDispatch,
  storedState: EditorStoreFull,
  domWalkerMutableState: {
    mutationObserver: MutationObserver
    resizeObserver: ResizeObserver
    gridControlObserver: MutationObserver
  },
  spyCollector: UiJsxCanvasContextData,
) {
  const metadataResult = runDomSamplerRegular({
    elementsToFocusOn: ElementsToRerenderGLOBAL.current,
    domWalkerAdditionalElementsToFocusOn:
      storedState.patchedEditor.canvas.domWalkerAdditionalElementsToUpdate,
    scale: storedState.patchedEditor.canvas.scale,
    selectedViews: storedState.patchedEditor.selectedViews,
    metadataToUpdate: storedState.elementMetadata,
    spyCollector: spyCollector,
  })

  const storedStateWithNewMetadata: EditorStoreFull = {
    ...storedState,
    elementMetadata: metadataResult.metadata,
  }

  const newFullStore = editorDispatchActionRunner(
    boundDispatch,
    [updateMetadataInEditorState(metadataResult.metadata, metadataResult.tree)],
    storedStateWithNewMetadata,
    spyCollector,
  )

  resubscribeObservers(domWalkerMutableState)

  return newFullStore
}
