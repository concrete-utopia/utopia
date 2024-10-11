import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { controlsForGridPlaceholders } from '../../controls/grid-controls-for-strategies'
import type {
  ElementInstanceMetadataMap,
  GridAutoOrTemplateBase,
} from '../../../../core/shared/element-template'
import * as PP from '../../../../core/shared/property-path'
import { printGridAutoOrTemplateBase } from '../../../inspector/common/css-utils'
import type { PropertyToUpdate } from '../../commands/set-property-command'
import {
  propertyToDelete,
  propertyToSet,
  updateBulkProperties,
} from '../../commands/set-property-command'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { ControlWithProps, InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { DragInteractionData, InteractionSession } from '../interaction-state'
import { runGridRearrangeMove } from './grid-helpers'
import { isInfinityRectangle } from '../../../../core/shared/math-utils'
import type { CanvasCommand } from '../../commands/commands'

export const gridRearrangeMoveStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
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

  const parentGridPath = EP.parentPath(selectedElement)
  const gridFrame = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    parentGridPath,
  )?.globalFrame
  if (gridFrame == null || isInfinityRectangle(gridFrame)) {
    return null
  }

  const initialTemplates = getGridTemplates(canvasState.startingMetadata, parentGridPath)
  if (initialTemplates == null) {
    return null
  }

  const strategyToApply = getStrategyToApply(
    interactionSession.interactionData,
    parentGridPath,
    canvasState.startingMetadata,
    selectedElement,
  )
  if (strategyToApply == null) {
    return null
  }

  return {
    id: 'rearrange-grid-move-strategy',
    name: strategyToApply.name,
    descriptiveLabel: strategyToApply.name,
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: strategyToApply.controlsToRender,
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CELL_HANDLE', 2),
    apply: (strategyLifecycle) => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const midInteractionCommands = [
        // during the interaction, freeze the template with the calculated values…
        updateBulkProperties('mid-interaction', parentGridPath, [
          propertyToSet(
            PP.create('style', 'gridTemplateColumns'),
            printGridAutoOrTemplateBase(initialTemplates.calculated.columns),
          ),
          propertyToSet(
            PP.create('style', 'gridTemplateRows'),
            printGridAutoOrTemplateBase(initialTemplates.calculated.rows),
          ),
        ]),
      ]

      const onCompleteCommands = [
        // …eventually, restore the grid template on complete.
        updateBulkProperties(
          'on-complete',
          parentGridPath,
          restoreGridTemplateFromProps(initialTemplates.fromProps),
        ),
      ]

      const { commands, elementsToRerender } = getCommandsAndPatchForGridRearrange(
        canvasState,
        interactionSession.interactionData,
        selectedElement,
      )

      if (commands.length === 0) {
        return emptyStrategyApplicationResult
      }

      return strategyApplicationResult(
        [...midInteractionCommands, ...onCompleteCommands, ...commands],
        elementsToRerender,
      )
    },
  }
}

function getCommandsAndPatchForGridRearrange(
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  selectedElement: ElementPath,
): {
  commands: CanvasCommand[]
  elementsToRerender: ElementPath[]
} {
  if (interactionData.drag == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const grid = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    EP.parentPath(selectedElement),
  )
  if (grid == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const commands = runGridRearrangeMove(
    selectedElement,
    selectedElement,
    canvasState.startingMetadata,
    interactionData,
    grid,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement)],
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

function getGridTemplates(jsxMetadata: ElementInstanceMetadataMap, gridPath: ElementPath) {
  const grid = MetadataUtils.findElementByElementPath(jsxMetadata, gridPath)
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
    calculated: {
      columns: templateColsCalculated,
      rows: templateRowsCalculated,
    },
    fromProps: {
      columns: templateColsFromProps,
      rows: templateRowsFromProps,
    },
  }
}

type StrategyToApply = {
  controlsToRender: ControlWithProps<any>[]
  name: string
}

function getStrategyToApply(
  interactionData: DragInteractionData,
  parentGridPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  cell: ElementPath,
): StrategyToApply | null {
  if (interactionData.drag == null) {
    return null
  }

  const element = MetadataUtils.findElementByElementPath(jsxMetadata, cell)

  const name =
    MetadataUtils.isPositionAbsolute(element) &&
    !MetadataUtils.isGridCellWithPositioning(jsxMetadata, cell)
      ? 'Grid Move (Abs)'
      : 'Rearrange Grid (Move)'

  return {
    name: name,
    controlsToRender: [controlsForGridPlaceholders(parentGridPath, 'visible-only-while-active')],
  }
}
