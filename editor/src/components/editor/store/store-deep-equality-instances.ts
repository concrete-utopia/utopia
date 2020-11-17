import {
  KeepDeepEqualityResult,
  keepDeepEqualityResult,
  KeepDeepEqualityCall,
} from '../../../utils/deep-equality'
import {
  TemplatePathArrayKeepDeepEquality,
  HigherOrderControlArrayKeepDeepEquality,
} from '../../../utils/deep-equality-instances'
import { createCallFromIntrospectiveKeepDeep } from '../../../utils/react-performance'
import {
  TransientCanvasState,
  TransientFileState,
  transientCanvasState,
  DerivedState,
} from './editor-state'

export function TransientCanvasStateKeepDeepEquality(
  oldTransientState: TransientCanvasState,
  newTransientState: TransientCanvasState,
): KeepDeepEqualityResult<TransientCanvasState> {
  let areEqual: boolean = true

  const selectedViewsResult = TemplatePathArrayKeepDeepEquality(
    oldTransientState.selectedViews,
    newTransientState.selectedViews,
  )
  const selectedViews = selectedViewsResult.value
  areEqual = areEqual && selectedViewsResult.areEqual

  const highlightedViewsResult = TemplatePathArrayKeepDeepEquality(
    oldTransientState.highlightedViews,
    newTransientState.highlightedViews,
  )
  const highlightedViews = highlightedViewsResult.value
  areEqual = areEqual && highlightedViewsResult.areEqual

  const fileStateEquality = createCallFromIntrospectiveKeepDeep<TransientFileState | null>()
  const fileStateResult = fileStateEquality(
    oldTransientState.fileState,
    newTransientState.fileState,
  )
  const fileState = fileStateResult.value
  areEqual = areEqual && fileStateResult.areEqual

  if (areEqual) {
    return keepDeepEqualityResult(oldTransientState, true)
  } else {
    const transientState: TransientCanvasState = transientCanvasState(
      selectedViews,
      highlightedViews,
      fileState,
    )
    return keepDeepEqualityResult(transientState, false)
  }
}

export function DerivedStateKeepDeepEquality(
  oldDerivedState: DerivedState,
  newDerivedState: DerivedState,
): KeepDeepEqualityResult<DerivedState> {
  let areEqual: boolean = true

  const navigatorTargetsResult = TemplatePathArrayKeepDeepEquality(
    oldDerivedState.navigatorTargets,
    newDerivedState.navigatorTargets,
  )
  const navigatorTargets = navigatorTargetsResult.value
  areEqual = areEqual && navigatorTargetsResult.areEqual

  const visibleNavigatorTargetsResult = TemplatePathArrayKeepDeepEquality(
    oldDerivedState.visibleNavigatorTargets,
    newDerivedState.visibleNavigatorTargets,
  )
  const visibleNavigatorTargets = visibleNavigatorTargetsResult.value
  areEqual = areEqual && visibleNavigatorTargetsResult.areEqual

  let canvasAreEqual: boolean = true
  const descendantsOfHiddenInstancesResult = TemplatePathArrayKeepDeepEquality(
    oldDerivedState.canvas.descendantsOfHiddenInstances,
    newDerivedState.canvas.descendantsOfHiddenInstances,
  )
  const descendantsOfHiddenInstances = descendantsOfHiddenInstancesResult.value
  canvasAreEqual = canvasAreEqual && descendantsOfHiddenInstancesResult.areEqual

  const controlsResult = HigherOrderControlArrayKeepDeepEquality(
    oldDerivedState.canvas.controls,
    newDerivedState.canvas.controls,
  )
  const controls = controlsResult.value
  canvasAreEqual = canvasAreEqual && controlsResult.areEqual

  const transientStateResult = TransientCanvasStateKeepDeepEquality(
    oldDerivedState.canvas.transientState,
    newDerivedState.canvas.transientState,
  )
  const transientState = transientStateResult.value
  canvasAreEqual = canvasAreEqual && transientStateResult.areEqual

  let canvas: DerivedState['canvas']
  if (canvasAreEqual) {
    canvas = oldDerivedState.canvas
  } else {
    canvas = {
      descendantsOfHiddenInstances: descendantsOfHiddenInstances,
      controls: controls,
      transientState: transientState,
    }
  }
  areEqual = areEqual && canvasAreEqual

  const elementWarningsEquality: KeepDeepEqualityCall<
    DerivedState['elementWarnings']
  > = createCallFromIntrospectiveKeepDeep()
  const elementWarningsResult = elementWarningsEquality(
    oldDerivedState.elementWarnings,
    newDerivedState.elementWarnings,
  )
  const elementWarnings = elementWarningsResult.value
  areEqual = areEqual && elementWarningsResult.areEqual

  if (areEqual) {
    return keepDeepEqualityResult(oldDerivedState, true)
  } else {
    const derivedState: DerivedState = {
      navigatorTargets: navigatorTargets,
      visibleNavigatorTargets: visibleNavigatorTargets,
      canvas: canvas,
      elementWarnings: elementWarnings,
    }
    return keepDeepEqualityResult(derivedState, false)
  }
}
