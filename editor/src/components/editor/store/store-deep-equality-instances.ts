import {
  KeepDeepEqualityResult,
  keepDeepEqualityResult,
  KeepDeepEqualityCall,
  combine3EqualityCalls,
  combine6EqualityCalls,
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

export function TransientCanvasStateKeepDeepEquality(): KeepDeepEqualityCall<TransientCanvasState> {
  return combine3EqualityCalls(
    (state) => state.selectedViews,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.highlightedViews,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.fileState,
    createCallFromIntrospectiveKeepDeep<TransientFileState | null>(),
    transientCanvasState,
  )
}

export function DerivedStateKeepDeepEquality(): KeepDeepEqualityCall<DerivedState> {
  return combine6EqualityCalls(
    (state) => state.navigatorTargets,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.visibleNavigatorTargets,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.canvas.descendantsOfHiddenInstances,
    TemplatePathArrayKeepDeepEquality,
    (state) => state.canvas.controls,
    HigherOrderControlArrayKeepDeepEquality,
    (state) => state.canvas.transientState,
    TransientCanvasStateKeepDeepEquality(),
    (state) => state.elementWarnings,
    createCallFromIntrospectiveKeepDeep(),
    (
      navigatorTargets,
      visibleNavigatorTargets,
      descendantsOfHiddenInstances,
      controls,
      transientState,
      elementWarnings,
    ) => {
      return {
        navigatorTargets: navigatorTargets,
        visibleNavigatorTargets: visibleNavigatorTargets,
        canvas: {
          descendantsOfHiddenInstances: descendantsOfHiddenInstances,
          controls: controls,
          transientState: transientState,
        },
        elementWarnings: elementWarnings,
      }
    },
  )
}
