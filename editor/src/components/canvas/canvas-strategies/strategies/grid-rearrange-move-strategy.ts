import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridAutoOrTemplateBase } from '../../../../core/shared/element-template'
import * as PP from '../../../../core/shared/property-path'
import { printGridAutoOrTemplateBase } from '../../../inspector/common/css-utils'
import type { PropertyToUpdate } from '../../commands/set-property-command'
import {
  propertyToDelete,
  propertyToSet,
  updateBulkProperties,
} from '../../commands/set-property-command'
import { GridControls } from '../../controls/grid-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { CustomStrategyState, InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { runGridRearrangeMove } from './grid-helpers'

export const gridRearrangeMoveStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customState: CustomStrategyState,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length === 0 ||
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null ||
    interactionSession.activeControl.type !== 'GRID_CELL_HANDLE' ||
    interactionSession.interactionData.modifiers.alt
  ) {
    return null
  }

  const selectedElement = selectedElements[0]
  if (!MetadataUtils.isGridCell(canvasState.startingMetadata, selectedElement)) {
    return null
  }

  const gridPath = EP.parentPath(selectedElement)

  const grid = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, gridPath)
  if (grid == null) {
    return null
  }

  const templateFromProps = grid.specialSizeMeasurements.containerGridPropertiesFromProps
  const templateRowsFromProps = templateFromProps.gridTemplateRows
  if (templateRowsFromProps == null) {
    return null
  }
  const templateColsFromProps = templateFromProps.gridTemplateColumns
  if (templateColsFromProps == null) {
    return null
  }

  const templateCalculated = grid.specialSizeMeasurements.containerGridProperties
  const templateRowsCalculated = templateCalculated.gridTemplateRows
  if (templateRowsCalculated == null) {
    return null
  }
  const templateColsCalculated = templateCalculated.gridTemplateColumns
  if (templateColsCalculated == null) {
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
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CELL_HANDLE', 2),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const targetElement = selectedElement

      const {
        commands: moveCommands,
        targetCell: targetGridCell,
        draggingFromCell,
        originalRootCell,
        targetRootCell,
      } = runGridRearrangeMove(
        targetElement,
        selectedElement,
        canvasState.startingMetadata,
        interactionSession.interactionData,
        canvasState.scale,
        canvasState.canvasOffset,
        customState.grid,
        false,
      )
      if (moveCommands.length === 0) {
        return emptyStrategyApplicationResult
      }

      const midInteractionCommands = [
        // during the interaction, freeze the template with the calculated values…
        updateBulkProperties('mid-interaction', gridPath, [
          propertyToSet(
            PP.create('style', 'gridTemplateColumns'),
            printGridAutoOrTemplateBase(templateColsCalculated),
          ),
          propertyToSet(
            PP.create('style', 'gridTemplateRows'),
            printGridAutoOrTemplateBase(templateRowsCalculated),
          ),
        ]),
      ]

      const onCompleteCommands = [
        // …eventually, restore the grid template on complete.
        updateBulkProperties(
          'on-complete',
          gridPath,
          restoreGridTemplateFromProps({
            columns: templateColsFromProps,
            rows: templateRowsFromProps,
          }),
        ),
      ]

      return strategyApplicationResult(
        [...moveCommands, ...midInteractionCommands, ...onCompleteCommands],
        {
          grid: {
            targetCell: targetGridCell,
            draggingFromCell: draggingFromCell,
            originalRootCell: originalRootCell,
            currentRootCell: targetRootCell,
          },
        },
      )
    },
  }
}

function restoreGridTemplateFromProps(params: {
  columns: GridAutoOrTemplateBase
  rows: GridAutoOrTemplateBase
}): PropertyToUpdate[] {
  let properties: PropertyToUpdate[] = []
  const newCols = printGridAutoOrTemplateBase(params.columns)
  const newRows = printGridAutoOrTemplateBase(params.rows)
  if (newCols === '') {
    properties.push(propertyToDelete(PP.create('style', 'gridTemplateColumns')))
  } else {
    properties.push(propertyToSet(PP.create('style', 'gridTemplateColumns'), newCols))
  }
  if (newRows === '') {
    properties.push(propertyToDelete(PP.create('style', 'gridTemplateRows')))
  } else {
    properties.push(propertyToSet(PP.create('style', 'gridTemplateRows'), newRows))
  }
  return properties
}
