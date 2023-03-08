import { mapDropNulls } from '../../../../core/shared/array-utils'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
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
  MoveStrategy,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { appendCommandsToApplyResult } from './ancestor-metastrategy'
import { flexReorderStrategy } from './flex-reorder-strategy'
import { flowReorderStrategy } from './flow-reorder-strategy'
import { relativeMoveStrategy } from './relative-move-strategy'
import { reparentMetaStrategy } from './reparent-metastrategy'
import { getDragTargets } from './shared-move-strategies-helpers'
import * as EP from '../../../../core/shared/element-path'

type MoveStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => MoveStrategy | null

const baseMoveStrategyFactories: Array<MoveStrategyFactory> = [
  absoluteMoveStrategy,
  flexReorderStrategy,
  flowReorderStrategy,
  relativeMoveStrategy,
]

export const dragToMoveMetaStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): Array<CanvasStrategy> => {
  const selectedElements = getDragTargets(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )

  if (
    selectedElements.length === 0 ||
    selectedElements.some(EP.isRootElementOfInstance) ||
    interactionSession == null ||
    interactionSession.activeControl.type !== 'BOUNDING_AREA' ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.modifiers.alt
  ) {
    return []
  }

  const reparentStrategies = reparentMetaStrategy(
    canvasState,
    interactionSession,
    customStrategyState,
  )
  const dragStrategies = mapDropNulls(
    (factory) => factory(canvasState, interactionSession, customStrategyState),
    baseMoveStrategyFactories,
  )
  if (reparentStrategies.length > 0 || dragStrategies.length > 0) {
    const dragToMoveStrategies = [
      ...reparentStrategies,
      ...dragStrategies.map((strategy) => {
        const indicatorCommand = wildcardPatch('mid-interaction', {
          canvas: {
            controls: {
              dragToMoveIndicatorFlags: {
                $set: {
                  showIndicator: true,
                  dragType: strategy.dragType,
                  reparent: 'none',
                  ancestor: false,
                },
              },
            },
          },
        })
        return {
          ...strategy.strategy,
          apply: appendCommandsToApplyResult(strategy.strategy.apply, [], [indicatorCommand]),
        }
      }),
    ]
    return filterStrategiesWhileSpacePressed(
      interactionSession.interactionData.spacePressed,
      dragToMoveStrategies,
    )
  } else {
    return filterStrategiesWhileSpacePressed(interactionSession.interactionData.spacePressed, [
      doNothingStrategy(canvasState, interactionSession, customStrategyState),
    ])
  }
}

export function filterStrategiesWhileSpacePressed(
  spacePressed: boolean,
  strategies: Array<CanvasStrategy>,
): Array<CanvasStrategy> {
  if (spacePressed) {
    return strategies.filter((strat) => strat.id === 'ABSOLUTE_MOVE')
  } else {
    return strategies
  }
}

export const DoNothingStrategyID = 'DO_NOTHING'

export function doNothingStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  return {
    id: DoNothingStrategyID,
    name: 'No Default Available',
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
    fitness: 1.5,
    apply: () => {
      return strategyApplicationResult([
        wildcardPatch('mid-interaction', {
          canvas: {
            controls: {
              dragToMoveIndicatorFlags: {
                $set: {
                  showIndicator: true,
                  dragType: 'none',
                  reparent: 'none',
                  ancestor: false,
                },
              },
            },
          },
        }),
      ])
    },
  }
}
