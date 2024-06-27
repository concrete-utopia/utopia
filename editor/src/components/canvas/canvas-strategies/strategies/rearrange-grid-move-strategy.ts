import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridElementProperties } from '../../../../core/shared/element-template'
import { create } from '../../../../core/shared/property-path'
import type { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import { TargetGridCell, GridControls } from '../../controls/grid-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'

export const rearrangeGridMoveStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  const ok = MetadataUtils.isGridLayoutedContainer(
    MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      EP.parentPath(selectedElement),
    ),
  )
  if (!ok) {
    return null
  }

  return {
    id: 'rearrange-grid-move-strategy',
    name: 'Rearrange Grid (Move)',
    descriptiveLabel: 'Rearrange Grid (Move)',
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
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CELL_HANDLE', 1) + 1, // add one so it wins over the swap
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      let commands: CanvasCommand[] = []

      if (TargetGridCell.current.row > 0 && TargetGridCell.current.column > 0) {
        const metadata = MetadataUtils.findElementByElementPath(
          canvasState.startingMetadata,
          selectedElement,
        )

        function getGridProperty(field: keyof GridElementProperties, fallback: number) {
          const propValue = metadata?.specialSizeMeasurements.elementGridProperties[field]
          return propValue == null || propValue === 'auto'
            ? 0
            : propValue.numericalPosition ?? fallback
        }

        const gridColumnStart = getGridProperty('gridColumnStart', 0)
        const gridColumnEnd = getGridProperty('gridColumnEnd', 1)
        const gridRowStart = getGridProperty('gridRowStart', 0)
        const gridRowEnd = getGridProperty('gridRowEnd', 1)

        if (metadata != null) {
          commands.push(
            setProperty(
              'always',
              selectedElement,
              create('style', 'gridColumnStart'),
              TargetGridCell.current.column,
            ),
            setProperty(
              'always',
              selectedElement,
              create('style', 'gridColumnEnd'),
              Math.max(
                TargetGridCell.current.column,
                TargetGridCell.current.column + (gridColumnEnd - gridColumnStart),
              ),
            ),
            setProperty(
              'always',
              selectedElement,
              create('style', 'gridRowStart'),
              TargetGridCell.current.row,
            ),
            setProperty(
              'always',
              selectedElement,
              create('style', 'gridRowEnd'),
              Math.max(
                TargetGridCell.current.row,
                TargetGridCell.current.row + (gridRowEnd - gridRowStart),
              ),
            ),
          )
        }
      }

      if (commands == null) {
        return emptyStrategyApplicationResult
      }

      return strategyApplicationResult(commands)
    },
  }
}
