import { EditorDispatch } from '../editor/action-types'
import { saveDOMReport } from '../editor/actions/action-creators'
import { DispatchResult, editorDispatch } from '../editor/store/dispatch'
import { EditorStoreFull, ElementsToRerender } from '../editor/store/editor-state'
import { DomWalkerMutableStateData, runDomWalker } from './dom-walker'
import { UiJsxCanvasContextData } from './ui-jsx-canvas'

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

  const dispatchResultWithMetadata = editorDispatch(
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
  )
  return dispatchResultWithMetadata
}
