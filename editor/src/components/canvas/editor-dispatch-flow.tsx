import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import type { EditorDispatch } from '../editor/action-types'
import { saveDOMReport } from '../editor/actions/action-creators'
import type { DispatchResult } from '../editor/store/dispatch'
import { editorDispatchActionRunner } from '../editor/store/dispatch'
import type { EditorStoreFull, ElementsToRerender } from '../editor/store/editor-state'
import type { DomWalkerMutableStateData } from './dom-walker'
import { runDomWalker } from './dom-walker'
import type { UiJsxCanvasContextData } from './ui-jsx-canvas'

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
