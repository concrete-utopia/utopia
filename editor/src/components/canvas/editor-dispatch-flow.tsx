import type { EditorDispatch } from '../editor/action-types'
import { saveDOMReport, updateMetadataInEditorState } from '../editor/actions/action-creators'
import type { DispatchResult } from '../editor/store/dispatch'
import { editorDispatchActionRunner } from '../editor/store/dispatch'
import type { EditorStoreFull, ElementsToRerender } from '../editor/store/editor-state'
import { runDomSampler } from './dom-sampler'
import type { DomWalkerMutableStateData } from './dom-walker'
import { resubscribeObservers, runDomWalker } from './dom-walker'
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

export function runDomWalkerAndSaveResults(
  dispatch: EditorDispatch,
  domWalkerMutableState: DomWalkerMutableStateData,
  storedState: EditorStoreFull,
  spyCollector: UiJsxCanvasContextData,
  elementsToFocusOn: ElementsToRerender,
): DispatchResult | null {
  const domWalkerResult = runDomWalker({
    domWalkerMutableState: domWalkerMutableState,
    selectedViews: storedState.patchedEditor.selectedViews,
    elementsToFocusOn: elementsToFocusOn,
    scale: storedState.patchedEditor.canvas.scale,
    additionalElementsToUpdate:
      storedState.patchedEditor.canvas.domWalkerAdditionalElementsToUpdate,
    rootMetadataInStateRef: {
      current: storedState.patchedEditor.domMetadata,
    },
  })

  if (domWalkerResult == null) {
    return null
  }

  const dispatchResultWithMetadata = editorDispatchActionRunner(
    dispatch,
    [
      saveDOMReport(
        domWalkerResult.metadata,
        domWalkerResult.cachedPaths,
        domWalkerResult.invalidatedPaths,
      ),
    ],
    storedState,
    spyCollector,
    domWalkerResult.reconstructedMetadata,
  )
  return dispatchResultWithMetadata
}

export function runDomSamplerAndSaveResults(
  boundDispatch: EditorDispatch,
  storedState: EditorStoreFull,
  domWalkerMutableState: {
    mutationObserver: MutationObserver
    resizeObserver: ResizeObserver
  },
  spyCollector: UiJsxCanvasContextData,
) {
  // we inject domWalkerAdditionalElementsToUpdate into ElementsToRerenderGLOBAL so that we can collect metadata for elements affected by Group resizing
  const elementsToCollect =
    ElementsToRerenderGLOBAL.current === 'rerender-all-elements'
      ? 'rerender-all-elements'
      : [
          ...ElementsToRerenderGLOBAL.current,
          ...storedState.patchedEditor.canvas.domWalkerAdditionalElementsToUpdate,
        ]

  const metadataResult = runDomSampler(elementsToCollect, {
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
    {},
  )

  resubscribeObservers(domWalkerMutableState)

  return newFullStore
}
