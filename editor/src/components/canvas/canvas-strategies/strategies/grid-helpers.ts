import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  gridPositionValue,
  type ElementInstanceMetadata,
  type GridContainerProperties,
  type GridElementProperties,
  type GridPosition,
} from '../../../../core/shared/element-template'
import type { CanvasVector, WindowRectangle } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  isInfinityRectangle,
  offsetPoint,
  scaleVector,
  windowPoint,
  windowVector,
  type WindowPoint,
} from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import type { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import type { DragInteractionData } from '../interaction-state'
import type { GridCustomStrategyState, InteractionCanvasState } from '../canvas-strategy-types'
import * as EP from '../../../../core/shared/element-path'
import { deleteProperties } from '../../commands/delete-properties-command'
import { cssNumber, isCSSKeyword } from '../../../inspector/common/css-utils'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
import type { GridCellCoordinates } from './grid-cell-bounds'
import {
  getGridCellUnderMouse,
  getGridCellUnderMouseRecursive,
  gridCellCoordinates,
} from './grid-cell-bounds'

export function runGridRearrangeMove(
  targetElement: ElementPath,
  selectedElement: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  canvasScale: number,
  canvasOffset: CanvasVector,
  customState: GridCustomStrategyState,
  duplicating: boolean,
): {
  commands: CanvasCommand[]
  targetCell: TargetGridCellData | null
  draggingFromCell: GridCellCoordinates | null
  originalRootCell: GridCellCoordinates | null
  targetRootCell: GridCellCoordinates | null
} {
  if (interactionData.drag == null) {
    return {
      commands: [],
      targetCell: null,
      originalRootCell: null,
      draggingFromCell: null,
      targetRootCell: null,
    }
  }

  const mouseWindowPoint = canvasPointToWindowPoint(
    offsetPoint(interactionData.dragStart, interactionData.drag),
    canvasScale,
    canvasOffset,
  )

  const targetCellData =
    getTargetCell(
      customState.targetCellData?.gridCellCoordinates ?? null,
      duplicating,
      mouseWindowPoint,
    ) ?? customState.targetCellData

  if (targetCellData == null) {
    return {
      commands: [],
      targetCell: null,
      draggingFromCell: null,
      originalRootCell: null,
      targetRootCell: null,
    }
  }

  const targetCellUnderMouse = targetCellData?.gridCellCoordinates ?? null

  const absoluteMoveCommands =
    targetCellData == null
      ? []
      : gridChildAbsoluteMoveCommands(
          MetadataUtils.findElementByElementPath(jsxMetadata, targetElement),
          targetCellData.cellWindowRectangle,
          interactionData,
          { scale: canvasScale, canvasOffset: canvasOffset },
        )

  const originalElementMetadata = MetadataUtils.findElementByElementPath(
    jsxMetadata,
    selectedElement,
  )
  if (originalElementMetadata == null) {
    return {
      commands: [],
      targetCell: null,
      originalRootCell: null,
      draggingFromCell: null,
      targetRootCell: null,
    }
  }

  const containerMetadata = MetadataUtils.findElementByElementPath(
    jsxMetadata,
    EP.parentPath(selectedElement),
  )
  if (containerMetadata == null) {
    return {
      commands: [],
      targetCell: null,
      originalRootCell: null,
      draggingFromCell: null,
      targetRootCell: null,
    }
  }

  const gridTemplate = containerMetadata.specialSizeMeasurements.containerGridProperties

  const cellGridProperties = getElementGridProperties(originalElementMetadata, targetCellUnderMouse)

  // calculate the difference between the cell the mouse started the interaction from, and the "root"
  // cell of the element, meaning the top-left-most cell the element occupies.
  const draggingFromCell = customState.draggingFromCell ?? targetCellUnderMouse
  const rootCell =
    customState.originalRootCell ??
    gridCellCoordinates(cellGridProperties.row, cellGridProperties.column)
  const coordsDiff = getCellCoordsDelta(draggingFromCell, rootCell)

  // get the new adjusted row
  const row = getCoordBounds(targetCellUnderMouse, 'row', cellGridProperties.width, coordsDiff.row)
  // get the new adjusted column
  const column = getCoordBounds(
    targetCellUnderMouse,
    'column',
    cellGridProperties.height,
    coordsDiff.column,
  )

  const targetRootCell = gridCellCoordinates(row.start, column.start)

  const gridCellMoveCommands = setGridPropsCommands(targetElement, gridTemplate, {
    gridColumnStart: gridPositionValue(column.start),
    gridColumnEnd: gridPositionValue(column.end),
    gridRowEnd: gridPositionValue(row.end),
    gridRowStart: gridPositionValue(row.start),
  })

  return {
    commands: [...gridCellMoveCommands, ...absoluteMoveCommands],
    targetCell: targetCellData ?? customState.targetCellData,
    originalRootCell: rootCell,
    draggingFromCell: draggingFromCell,
    targetRootCell: targetRootCell,
  }
}

export function gridPositionToValue(p: GridPosition | null | undefined): string | number | null {
  if (p == null) {
    return null
  }
  if (isCSSKeyword(p)) {
    return p.value
  }

  return p.numericalPosition
}

export function setGridPropsCommands(
  elementPath: ElementPath,
  gridTemplate: GridContainerProperties,
  gridProps: Partial<GridElementProperties>,
): CanvasCommand[] {
  let commands: CanvasCommand[] = [
    deleteProperties('always', elementPath, [
      PP.create('style', 'gridRow'),
      PP.create('style', 'gridArea'),
      PP.create('style', 'gridColumn'),
      PP.create('style', 'gridColumnStart'),
      PP.create('style', 'gridColumnEnd'),
      PP.create('style', 'gridRowStart'),
      PP.create('style', 'gridRowEnd'),
    ]),
  ]
  const columnStart = gridPositionToValue(gridProps.gridColumnStart)
  const columnEnd = gridPositionToValue(gridProps.gridColumnEnd)
  const rowStart = gridPositionToValue(gridProps.gridRowStart)
  const rowEnd = gridPositionToValue(gridProps.gridRowEnd)

  const areaColumnStart = asMaybeNamedAreaOrValue(gridTemplate, 'column', columnStart)
  const areaColumnEnd = asMaybeNamedAreaOrValue(gridTemplate, 'column', columnEnd)
  const areaRowStart = asMaybeNamedAreaOrValue(gridTemplate, 'row', rowStart)
  const areaRowEnd = asMaybeNamedAreaOrValue(gridTemplate, 'row', rowEnd)

  if (columnStart != null && columnStart === columnEnd) {
    commands.push(
      setProperty('always', elementPath, PP.create('style', 'gridColumn'), areaColumnStart),
    )
  } else if (
    columnStart != null &&
    typeof columnStart === 'number' &&
    columnEnd != null &&
    typeof columnEnd === 'number' &&
    columnStart === columnEnd - 1
  ) {
    commands.push(
      setProperty('always', elementPath, PP.create('style', 'gridColumn'), areaColumnStart),
    )
  } else {
    if (columnStart != null) {
      commands.push(
        setProperty('always', elementPath, PP.create('style', 'gridColumnStart'), areaColumnStart),
      )
    }
    if (columnEnd != null) {
      commands.push(
        setProperty('always', elementPath, PP.create('style', 'gridColumnEnd'), areaColumnEnd),
      )
    }
  }

  if (rowStart != null && rowStart === rowEnd) {
    commands.push(setProperty('always', elementPath, PP.create('style', 'gridRow'), areaRowStart))
  } else if (
    rowStart != null &&
    typeof rowStart === 'number' &&
    rowEnd != null &&
    typeof rowEnd === 'number' &&
    rowStart === rowEnd - 1
  ) {
    commands.push(setProperty('always', elementPath, PP.create('style', 'gridRow'), areaRowStart))
  } else {
    if (rowStart != null) {
      commands.push(
        setProperty('always', elementPath, PP.create('style', 'gridRowStart'), areaRowStart),
      )
    }
    if (rowEnd != null) {
      commands.push(
        setProperty('always', elementPath, PP.create('style', 'gridRowEnd'), areaRowEnd),
      )
    }
  }

  return commands
}

export interface TargetGridCellData {
  gridCellCoordinates: GridCellCoordinates
  cellWindowRectangle: WindowRectangle
}

export function getTargetCell(
  previousTargetCell: GridCellCoordinates | null,
  duplicating: boolean,
  mouseWindowPoint: WindowPoint,
): TargetGridCellData | null {
  let cell = previousTargetCell ?? null
  const cellUnderMouse = duplicating
    ? getGridCellUnderMouseRecursive(mouseWindowPoint)
    : getGridCellUnderMouse(mouseWindowPoint)
  if (cellUnderMouse == null) {
    return null
  }
  cell = cellUnderMouse.coordinates
  if (cell.row < 1 || cell.column < 1) {
    return null
  }
  return {
    gridCellCoordinates: cell,
    cellWindowRectangle: cellUnderMouse.cellWindowRectangle,
  }
}

function getElementGridProperties(
  element: ElementInstanceMetadata,
  cellUnderMouse: { row: number; column: number },
): {
  row: number
  width: number
  column: number
  height: number
} {
  // get the grid fixtures (start and end for column and row) from the element metadata
  function getGridProperty(field: keyof GridElementProperties, fallback: number) {
    const propValue = element.specialSizeMeasurements.elementGridProperties[field]
    if (propValue == null || isCSSKeyword(propValue)) {
      return fallback
    }
    return propValue.numericalPosition ?? fallback
  }
  const column = getGridProperty('gridColumnStart', cellUnderMouse.column)
  const height = getGridProperty('gridColumnEnd', cellUnderMouse.column + 1) - column
  const row = getGridProperty('gridRowStart', cellUnderMouse.row)
  const width = getGridProperty('gridRowEnd', cellUnderMouse.row + 1) - row

  return {
    row,
    width,
    column,
    height,
  }
}

function getCellCoordsDelta(
  dragFrom: GridCellCoordinates,
  rootCell: GridCellCoordinates,
): GridCellCoordinates {
  const rowDiff = dragFrom.row - rootCell.row
  const columnDiff = dragFrom.column - rootCell.column

  return gridCellCoordinates(rowDiff, columnDiff)
}

function getCoordBounds(
  cell: GridCellCoordinates,
  coord: 'column' | 'row',
  size: number, // width or height
  adjustOffset: number, // adjustment based on the difference between the initial dragging cell and the root cell
): { start: number; end: number } {
  // the start is the first cell's coord the element will occupy
  const start = Math.max(1, cell[coord] - adjustOffset)
  // the end is the last cell's coord the element will occupy
  const end = Math.max(1, start + size)
  return { start, end }
}

function asMaybeNamedAreaOrValue(
  grid: GridContainerProperties,
  axis: 'row' | 'column',
  value: number | string | null,
): string | number {
  if (value == null) {
    return 1
  } else if (typeof value === 'number') {
    const template = axis === 'row' ? grid.gridTemplateRows : grid.gridTemplateColumns
    if (template?.type === 'DIMENSIONS') {
      const maybeAreaStart = template.dimensions.at(value - 1)
      if (maybeAreaStart != null && maybeAreaStart.areaName != null) {
        return maybeAreaStart.areaName
      }
    }
    return value === 0 ? 1 : value
  }
  return value
}

function gridChildAbsoluteMoveCommands(
  targetMetadata: ElementInstanceMetadata | null,
  targetCellWindowRect: WindowRectangle,
  dragInteractionData: DragInteractionData,
  canvasContext: Pick<InteractionCanvasState, 'scale' | 'canvasOffset'>,
): CanvasCommand[] {
  if (
    targetMetadata == null ||
    targetMetadata.globalFrame == null ||
    isInfinityRectangle(targetMetadata.globalFrame) ||
    !MetadataUtils.isPositionAbsolute(targetMetadata)
  ) {
    return []
  }

  const targetFrameWindow = canvasPointToWindowPoint(
    canvasPoint({
      x: targetMetadata.globalFrame.x,
      y: targetMetadata.globalFrame.y,
    }),
    canvasContext.scale,
    canvasContext.canvasOffset,
  )

  const dragStartWindow = canvasPointToWindowPoint(
    canvasPoint({
      x: dragInteractionData.originalDragStart.x,
      y: dragInteractionData.originalDragStart.y,
    }),
    canvasContext.scale,
    canvasContext.canvasOffset,
  )

  const offsetInTarget = windowPoint({
    x: dragStartWindow.x - targetFrameWindow.x,
    y: dragStartWindow.y - targetFrameWindow.y,
  })

  const dragWindowOffset = canvasPointToWindowPoint(
    offsetPoint(
      dragInteractionData.originalDragStart,
      dragInteractionData.drag ?? canvasPoint({ x: 0, y: 0 }),
    ),
    canvasContext.scale,
    canvasContext.canvasOffset,
  )

  const offset = scaleVector(
    windowVector({
      x: dragWindowOffset.x - targetCellWindowRect.x - offsetInTarget.x,
      y: dragWindowOffset.y - targetCellWindowRect.y - offsetInTarget.y,
    }),
    1 / canvasContext.scale,
  )

  return [
    deleteProperties('always', targetMetadata.elementPath, [
      PP.create('style', 'top'),
      PP.create('style', 'left'),
      PP.create('style', 'right'),
      PP.create('style', 'bottom'),
    ]),
    setCssLengthProperty(
      'always',
      targetMetadata.elementPath,
      PP.create('style', 'top'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(offset.y, null) },
      null,
    ),
    setCssLengthProperty(
      'always',
      targetMetadata.elementPath,
      PP.create('style', 'left'),
      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(offset.x, null) },
      null,
    ),
  ]
}
