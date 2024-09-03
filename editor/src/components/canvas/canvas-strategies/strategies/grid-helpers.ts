import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  gridPositionValue,
  type ElementInstanceMetadata,
  type GridContainerProperties,
  type GridElementProperties,
  type GridPosition,
} from '../../../../core/shared/element-template'
import type { WindowRectangle } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  isInfinityRectangle,
  offsetPoint,
  rectContainsPoint,
  windowRectangle,
  type WindowPoint,
} from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import type { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import type { DragInteractionData, InteractionSession } from '../interaction-state'
import type {
  CustomStrategyState,
  InteractionCanvasState,
  InteractionLifecycle,
} from '../canvas-strategy-types'
import type { GridCellCoordinates } from '../../controls/grid-controls'
import { gridCellCoordinates } from '../../controls/grid-controls'
import * as EP from '../../../../core/shared/element-path'
import { deleteProperties } from '../../commands/delete-properties-command'
import { cssNumber, isCSSKeyword } from '../../../inspector/common/css-utils'
import { setCssLengthProperty } from '../../commands/set-css-length-command'

export function getGridCellUnderMouse(mousePoint: WindowPoint) {
  return getGridCellAtPoint(mousePoint, false)
}

function getGridCellUnderMouseRecursive(mousePoint: WindowPoint) {
  return getGridCellAtPoint(mousePoint, true)
}

const gridCellTargetIdPrefix = 'grid-cell-target-'

export function gridCellTargetId(
  gridElementPath: ElementPath,
  row: number,
  column: number,
): string {
  return gridCellTargetIdPrefix + `${EP.toString(gridElementPath)}-${row}-${column}`
}

function isGridCellTargetId(id: string): boolean {
  return id.startsWith(gridCellTargetIdPrefix)
}

function getGridCellAtPoint(
  windowPoint: WindowPoint,
  duplicating: boolean,
): { id: string; coordinates: GridCellCoordinates; cellWindowRectangle: WindowRectangle } | null {
  function maybeRecursivelyFindCellAtPoint(
    elements: Element[],
  ): { element: Element; cellWindowRectangle: WindowRectangle } | null {
    // If this used during duplication, the canvas controls will be in the way and we need to traverse the children too.
    for (const element of elements) {
      if (isGridCellTargetId(element.id)) {
        const domRect = element.getBoundingClientRect()
        const windowRect = windowRectangle(domRect)
        if (rectContainsPoint(windowRect, windowPoint)) {
          return { element: element, cellWindowRectangle: windowRect }
        }
      }

      if (duplicating) {
        const child = maybeRecursivelyFindCellAtPoint(Array.from(element.children))
        if (child != null) {
          return child
        }
      }
    }

    return null
  }

  const cellUnderMouse = maybeRecursivelyFindCellAtPoint(
    document.elementsFromPoint(windowPoint.x, windowPoint.y),
  )
  if (cellUnderMouse == null) {
    return null
  }

  const { element, cellWindowRectangle } = cellUnderMouse
  const row = element.getAttribute('data-grid-row')
  const column = element.getAttribute('data-grid-column')
  return {
    id: element.id,
    cellWindowRectangle: cellWindowRectangle,
    coordinates: gridCellCoordinates(
      row == null ? 0 : parseInt(row),
      column == null ? 0 : parseInt(column),
    ),
  }
}

export function runGridRearrangeMove(
  targetElement: ElementPath,
  selectedElement: ElementPath,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  customStrategyState: CustomStrategyState,
  strategyLifecycle: InteractionLifecycle,
  duplicating: boolean,
): {
  commands: CanvasCommand[]
  targetCell: GridCellCoordinates | null
  draggingFromCell: GridCellCoordinates | null
  originalRootCell: GridCellCoordinates | null
  targetRootCell: GridCellCoordinates | null
} {
  const { interactionData } = interactionSession
  if (interactionData.type !== 'DRAG' || interactionData.drag == null) {
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
    canvasState.scale,
    canvasState.canvasOffset,
  )

  const newTargetCellData = getTargetCell(
    customStrategyState.grid.targetCell,
    duplicating,
    mouseWindowPoint,
  )

  const targetCellUnderMouse = newTargetCellData?.gridCellCoordinates ?? null

  // if there's no cell target under the mouse, try using the last known cell
  const newTargetCell = targetCellUnderMouse ?? customStrategyState.grid.targetCell

  if (newTargetCell == null) {
    return {
      commands: [],
      targetCell: null,
      draggingFromCell: null,
      originalRootCell: null,
      targetRootCell: null,
    }
  }

  const absoluteMoveCommands =
    newTargetCellData == null
      ? []
      : gridChildAbsoluteMoveCommands(
          MetadataUtils.findElementByElementPath(canvasState.startingMetadata, targetElement),
          newTargetCellData.cellWindowRectangle,
          interactionData,
          canvasState,
        )

  const originalElementMetadata = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
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
    canvasState.startingMetadata,
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

  const cellGridProperties = getElementGridProperties(originalElementMetadata, newTargetCell)

  // calculate the difference between the cell the mouse started the interaction from, and the "root"
  // cell of the element, meaning the top-left-most cell the element occupies.
  const draggingFromCell = customStrategyState.grid.draggingFromCell ?? newTargetCell
  const rootCell =
    customStrategyState.grid.originalRootCell ??
    gridCellCoordinates(cellGridProperties.row, cellGridProperties.column)
  const coordsDiff = getCellCoordsDelta(draggingFromCell, rootCell)

  // get the new adjusted row
  const row = getCoordBounds(newTargetCell, 'row', cellGridProperties.width, coordsDiff.row)
  // get the new adjusted column
  const column = getCoordBounds(
    newTargetCell,
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
    targetCell: newTargetCell,
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

export function getTargetCell(
  previousTargetCell: GridCellCoordinates | null,
  duplicating: boolean,
  mouseWindowPoint: WindowPoint,
): { gridCellCoordinates: GridCellCoordinates; cellWindowRectangle: WindowRectangle } | null {
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

  const offsetInTarget = canvasPoint({
    x: dragInteractionData.originalDragStart.x - targetMetadata.globalFrame.x,
    y: dragInteractionData.originalDragStart.y - targetMetadata.globalFrame.y,
  })

  const dragCanvasOffset = offsetPoint(
    dragInteractionData.originalDragStart,
    dragInteractionData.drag ?? canvasPoint({ x: 0, y: 0 }),
  )

  const dragWindowOffset = canvasPointToWindowPoint(
    dragCanvasOffset,
    canvasContext.scale,
    canvasContext.canvasOffset,
  )

  const offset = {
    x: dragWindowOffset.x - targetCellWindowRect.x - offsetInTarget.x,
    y: dragWindowOffset.y - targetCellWindowRect.y - offsetInTarget.y,
  }

  return [
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
