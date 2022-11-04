import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isRootElementOfInstance } from '../../../../core/shared/element-path'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import {
  DragOutlineControl,
  dragTargetsElementPaths,
} from '../../controls/select-mode/drag-outline-control'
import { CanvasStrategyFactory, MetaCanvasStrategy } from '../canvas-strategies'
import {
  CustomStrategyState,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  CanvasStrategy,
  emptyStrategyApplicationResult,
  controlWithProps,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { flexReorderStrategy } from './flex-reorder-strategy'
import { flowReorderStrategy } from './flow-reorder-strategy'
import { relativeMoveStrategy } from './relative-move-strategy'
import { reparentMetaStrategy } from './reparent-metastrategy'
import { getDragTargets } from './shared-move-strategies-helpers'

// + ancestor strategy !!???
const baseMoveStrategyFactories: Array<CanvasStrategyFactory> = [
  absoluteMoveStrategy,
  flexReorderStrategy,
  flowReorderStrategy,
  relativeMoveStrategy,
  doNothingStrategy,
]

export const moveReorderMetaStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  const selectedElements = getDragTargets(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )
  const anyDraggedElementsRootElements = selectedElements.some(isRootElementOfInstance)

  if (
    selectedElements.length === 0 ||
    anyDraggedElementsRootElements ||
    interactionSession == null ||
    interactionSession.activeControl.type !== 'BOUNDING_AREA' ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null ||
    interactionSession.interactionData.modifiers.alt
  ) {
    return []
  }

  const reparentStrategy = reparentMetaStrategy(
    canvasState,
    interactionSession,
    customStrategyState,
  )
  if (reparentStrategy.length === 0) {
    return mapDropNulls(
      (factory) => factory(canvasState, interactionSession, customStrategyState),
      baseMoveStrategyFactories,
    )
  } else {
    return reparentStrategy
  }
}

export function doNothingStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  return {
    id: 'DO_NOTHING',
    name: 'Placeholder',
    controlsToRender: [
      controlWithProps({
        control: DragOutlineControl,
        props: dragTargetsElementPaths(selectedElements),
        key: 'ghost-outline-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: selectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: selectedElements },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
        ? 1.5
        : 0,
    apply: () => {
      return emptyStrategyApplicationResult
    },
  }
}
