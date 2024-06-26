import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { zeroRectIfNullOrInfinity } from '../../../../core/shared/math-utils'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import { deleteProperties } from '../../commands/delete-properties-command'
import { setActiveFrames, activeFrameTargetPath } from '../../commands/set-active-frames-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { GridControls } from '../../controls/grid-controls'
import { recurseIntoChildrenOfMapOrFragment, cursorFromFlexDirection } from '../../gap-utils'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
  controlForStrategyMemoized,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { SetFlexGapStrategyId } from './set-flex-gap-strategy'

export const rearrangeGridStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  if (
    !MetadataUtils.isGridLayoutedContainer(
      MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        EP.parentPath(selectedElement),
      ),
    )
  ) {
    return null
  }

  const children = recurseIntoChildrenOfMapOrFragment(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    selectedElement,
  )

  return {
    id: SetFlexGapStrategyId,
    name: 'Set flex gap',
    descriptiveLabel: 'Changing Flex Gap',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [
      {
        control: GridControls,
        props: {},
        key: `grid-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CELL_HANDLE', 1),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      // TODO
      return emptyStrategyApplicationResult
    },
  }
}
