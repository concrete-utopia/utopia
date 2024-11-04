import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  GridAutoOrTemplateBase,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import * as PP from '../../../../core/shared/property-path'
import { printGridAutoOrTemplateBase } from '../../../inspector/common/css-utils'
import type { CanvasCommand } from '../../commands/commands'
import type { PropertyToUpdate } from '../../commands/set-property-command'
import {
  propertyToDelete,
  propertyToSet,
  updateBulkProperties,
} from '../../commands/set-property-command'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import type { DragInteractionData } from '../interaction-state'
import {
  findOriginalGrid,
  runGridMoveAbsolute,
  runGridMoveRearrange,
  runGridMoveReorder,
} from './grid-helpers'

export function getCommandsAndPatchForGridRearrange(
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

  const selectedElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (selectedElementMetadata == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const gridPath = findOriginalGrid(canvasState.startingMetadata, EP.parentPath(selectedElement)) // TODO don't use EP.parentPath
  if (gridPath == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const { parentGridCellGlobalFrames, parentContainerGridProperties } =
    selectedElementMetadata.specialSizeMeasurements
  if (parentGridCellGlobalFrames == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const commands = runGridMoveRearrange(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    gridPath,
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
    null,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement), selectedElement],
  }
}

export function getCommandsAndPatchForGridAbsoluteMove(
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

  const selectedElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (selectedElementMetadata == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const gridPath = findOriginalGrid(canvasState.startingMetadata, EP.parentPath(selectedElement)) // TODO don't use EP.parentPath
  if (gridPath == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const { parentGridCellGlobalFrames, parentContainerGridProperties } =
    selectedElementMetadata.specialSizeMeasurements
  if (parentGridCellGlobalFrames == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const commands = runGridMoveAbsolute(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    gridPath,
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement), selectedElement],
  }
}

export function getCommandsAndPatchForGridReorder(
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

  const selectedElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (selectedElementMetadata == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const gridPath = findOriginalGrid(canvasState.startingMetadata, EP.parentPath(selectedElement)) // TODO don't use EP.parentPath
  if (gridPath == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const { parentGridCellGlobalFrames, parentContainerGridProperties } =
    selectedElementMetadata.specialSizeMeasurements
  if (parentGridCellGlobalFrames == null) {
    return { commands: [], elementsToRerender: [] }
  }

  const commands = runGridMoveReorder(
    canvasState.startingMetadata,
    interactionData,
    selectedElementMetadata,
    gridPath,
    parentGridCellGlobalFrames,
    parentContainerGridProperties,
  )

  return {
    commands: commands,
    elementsToRerender: [EP.parentPath(selectedElement), selectedElement],
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

type GridInitialTemplates = {
  calculated: {
    columns: GridAutoOrTemplateBase
    rows: GridAutoOrTemplateBase
  }
  fromProps: {
    columns: GridAutoOrTemplateBase
    rows: GridAutoOrTemplateBase
  }
}

export function getParentGridTemplatesFromChildMeasurements(
  specialSizeMeasurements: SpecialSizeMeasurements,
): GridInitialTemplates | null {
  const parentTemplateCalculated = specialSizeMeasurements.parentContainerGridProperties
  const parentTemplateFromProps = specialSizeMeasurements.parentContainerGridPropertiesFromProps

  const templateColsCalculated = parentTemplateCalculated.gridTemplateColumns
  if (templateColsCalculated == null) {
    return null
  }
  const templateRowsCalculated = parentTemplateCalculated.gridTemplateRows
  if (templateRowsCalculated == null) {
    return null
  }

  const templateColsFromProps = parentTemplateFromProps.gridTemplateColumns
  if (templateColsFromProps == null) {
    return null
  }
  const templateRowsFromProps = parentTemplateFromProps.gridTemplateRows
  if (templateRowsFromProps == null) {
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

export function gridMoveStrategiesExtraCommands(
  parentGridPath: ElementPath,
  initialTemplates: GridInitialTemplates,
) {
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

  return { midInteractionCommands, onCompleteCommands }
}
