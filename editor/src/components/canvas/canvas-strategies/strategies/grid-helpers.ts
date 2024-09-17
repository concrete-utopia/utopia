import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  GridPositionValue,
  GridTemplate,
} from '../../../../core/shared/element-template'
import {
  gridPositionValue,
  type ElementInstanceMetadata,
  type GridContainerProperties,
  type GridElementProperties,
  type GridPosition,
} from '../../../../core/shared/element-template'
import type {
  CanvasRectangle,
  CanvasVector,
  WindowRectangle,
} from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  isInfinityRectangle,
  offsetPoint,
  scaleVector,
  windowPoint,
  windowVector,
} from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { absolute } from '../../../../utils/utils'
import { cssNumber, isCSSKeyword } from '../../../inspector/common/css-utils'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import { reorderElement } from '../../commands/reorder-element-command'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
import { setProperty } from '../../commands/set-property-command'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import type { GridCustomStrategyState, InteractionCanvasState } from '../canvas-strategy-types'
import type { DragInteractionData } from '../interaction-state'
import type { GridCellCoordinates } from './grid-cell-bounds'
import {
  getCellWindowRect,
  getGridCellUnderMouseFromMetadata,
  gridCellCoordinates,
} from './grid-cell-bounds'
import { memoize } from '../../../../core/shared/memoize'

export function runGridRearrangeMove(
  targetElement: ElementPath,
  selectedElement: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  canvasScale: number,
  canvasOffset: CanvasVector,
  customState: GridCustomStrategyState,
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

  const parentGridPath = EP.parentPath(selectedElement)
  const grid = MetadataUtils.findElementByElementPath(jsxMetadata, parentGridPath)

  if (grid == null) {
    return {
      commands: [],
      targetCell: null,
      originalRootCell: null,
      draggingFromCell: null,
      targetRootCell: null,
    }
  }

  const mousePos = offsetPoint(interactionData.dragStart, interactionData.drag)

  const targetCellData =
    getGridCellUnderMouseFromMetadata(grid, mousePos) ?? customState.targetCellData

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

  const gridCellMoveCommands = setGridPropsCommands(targetElement, gridTemplate, {
    gridColumnStart: gridPositionValue(column.start),
    gridColumnEnd: gridPositionValue(column.end),
    gridRowEnd: gridPositionValue(row.end),
    gridRowStart: gridPositionValue(row.start),
  })

  const gridTemplateColumns =
    gridTemplate.gridTemplateColumns?.type === 'DIMENSIONS'
      ? gridTemplate.gridTemplateColumns.dimensions.length
      : 1

  // The "pure" index in the grid children for the cell under mouse
  const possiblyReorderIndex = getGridPositionIndex({
    row: targetCellUnderMouse.row,
    column: targetCellUnderMouse.column,
    gridTemplateColumns: gridTemplateColumns,
  })

  // The siblings of the grid element being moved
  const siblings = MetadataUtils.getChildrenUnordered(jsxMetadata, EP.parentPath(selectedElement))
    .filter((s) => !EP.pathsEqual(s.elementPath, selectedElement))
    .map(
      (s, index): SortableGridElementProperties => ({
        ...s.specialSizeMeasurements.elementGridProperties,
        index: index,
        path: s.elementPath,
      }),
    )

  // Sort the siblings and the cell under mouse ascending based on their grid coordinates, so that
  // the indexes grow left-right, top-bottom.
  const cellsSortedByPosition = siblings
    .concat({
      ...{
        gridColumnStart: gridPositionValue(targetCellUnderMouse.column),
        gridColumnEnd: gridPositionValue(targetCellUnderMouse.column),
        gridRowStart: gridPositionValue(targetCellUnderMouse.row),
        gridRowEnd: gridPositionValue(targetCellUnderMouse.row),
      },
      path: selectedElement,
      index: siblings.length + 1,
    })
    .sort(sortElementsByGridPosition(gridTemplateColumns))

  // If rearranging, reorder to the index based on the sorted cells arrays.
  const indexInSortedCellsForRearrange = cellsSortedByPosition.findIndex((s) =>
    EP.pathsEqual(selectedElement, s.path),
  )

  const moveType = getGridMoveType({
    originalElementMetadata: originalElementMetadata,
    possiblyReorderIndex: possiblyReorderIndex,
    cellsSortedByPosition: cellsSortedByPosition,
  })

  switch (moveType) {
    case 'rearrange': {
      const targetRootCell = gridCellCoordinates(row.start, column.start)
      const windowRect = getCellWindowRect(targetRootCell)
      const absoluteMoveCommands =
        windowRect == null
          ? []
          : gridChildAbsoluteMoveCommands(
              MetadataUtils.findElementByElementPath(jsxMetadata, targetElement),
              windowRect,
              interactionData,
              { scale: canvasScale, canvasOffset: canvasOffset },
            )
      return {
        commands: [
          ...gridCellMoveCommands,
          ...absoluteMoveCommands,
          reorderElement(
            'always',
            selectedElement,
            absolute(Math.max(indexInSortedCellsForRearrange, 0)),
          ),
        ],
        targetCell: targetCellData ?? customState.targetCellData,
        originalRootCell: rootCell,
        draggingFromCell: draggingFromCell,
        targetRootCell: gridCellCoordinates(row.start, column.start),
      }
    }
    case 'reorder': {
      return {
        commands: [
          reorderElement('always', selectedElement, absolute(possiblyReorderIndex)),
          deleteProperties('always', selectedElement, [
            PP.create('style', 'gridColumn'),
            PP.create('style', 'gridRow'),
            PP.create('style', 'gridColumnStart'),
            PP.create('style', 'gridColumnEnd'),
            PP.create('style', 'gridRowStart'),
            PP.create('style', 'gridRowEnd'),
          ]),
        ],
        targetCell: targetCellData ?? customState.targetCellData,
        originalRootCell: rootCell,
        draggingFromCell: draggingFromCell,
        targetRootCell: targetCellUnderMouse,
      }
    }
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
  cellCanvasRectangle: CanvasRectangle
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

type SortableGridElementProperties = GridElementProperties & { path: ElementPath; index: number }

function sortElementsByGridPosition(gridTemplateColumns: number) {
  return function (a: SortableGridElementProperties, b: SortableGridElementProperties): number {
    function getPosition(index: number, e: GridElementProperties) {
      if (
        e.gridColumnStart == null ||
        isCSSKeyword(e.gridColumnStart) ||
        e.gridRowStart == null ||
        isCSSKeyword(e.gridRowStart)
      ) {
        return index
      }

      const row = e.gridRowStart.numericalPosition ?? 1
      const column = e.gridColumnStart.numericalPosition ?? 1

      return (row - 1) * gridTemplateColumns + column - 1
    }

    return getPosition(a.index, a) - getPosition(b.index, b)
  }
}

type GridMoveType =
  | 'reorder' // reorder the element in the code based on the ascending position, and remove explicit positioning props
  | 'rearrange' // set explicit positioning props, and reorder based on the visual location

function getGridMoveType(params: {
  originalElementMetadata: ElementInstanceMetadata
  possiblyReorderIndex: number
  cellsSortedByPosition: SortableGridElementProperties[]
}): GridMoveType {
  // For absolute move, just use rearrange.
  // TODO: maybe worth reconsidering in the future?
  if (MetadataUtils.isPositionAbsolute(params.originalElementMetadata)) {
    return 'rearrange'
  }
  if (params.possiblyReorderIndex >= params.cellsSortedByPosition.length) {
    return 'rearrange'
  }

  const elementGridProperties =
    params.originalElementMetadata.specialSizeMeasurements.elementGridProperties

  // The first element is intrinsically in order, so try to adjust for that
  if (params.possiblyReorderIndex === 0) {
    const isTheOnlyChild = params.cellsSortedByPosition.length === 1
    const isAlreadyTheFirstChild = EP.pathsEqual(
      params.cellsSortedByPosition[0].path,
      params.originalElementMetadata.elementPath,
    )
    const isAlreadyAtOrigin =
      gridPositionNumberValue(elementGridProperties.gridRowStart) === 1 &&
      gridPositionNumberValue(elementGridProperties.gridColumnStart) === 1
    if (isTheOnlyChild || isAlreadyTheFirstChild || isAlreadyAtOrigin) {
      return 'reorder'
    }
  }

  const previousElement = params.cellsSortedByPosition.at(params.possiblyReorderIndex - 1)
  if (previousElement == null) {
    return 'rearrange'
  }
  const previousElementColumn = previousElement.gridColumnStart ?? null
  const previousElementRow = previousElement.gridRowStart ?? null
  return isGridPositionNumericValue(previousElementColumn) &&
    isGridPositionNumericValue(previousElementRow)
    ? 'rearrange'
    : 'reorder'
}

function isGridPositionNumericValue(p: GridPosition | null): p is GridPositionValue {
  return p != null && !(isCSSKeyword(p) && p.value === 'auto')
}

function gridPositionNumberValue(p: GridPosition | null): number | null {
  return isGridPositionNumericValue(p) ? p.numericalPosition : null
}

function getGridPositionIndex(props: {
  row: number
  column: number
  gridTemplateColumns: number
}): number {
  return (props.row - 1) * props.gridTemplateColumns + props.column - 1
}

export type GridCellGlobalFrames = Array<Array<CanvasRectangle>>

function getGlobalFramesOfGridCellsInner(
  metadata: ElementInstanceMetadata,
): GridCellGlobalFrames | null {
  const { globalFrame } = metadata
  if (globalFrame == null || isInfinityRectangle(globalFrame)) {
    return null
  }

  const { containerGridProperties, padding, rowGap, columnGap } = metadata.specialSizeMeasurements

  const columnWidths = gridTemplateToNumbers(containerGridProperties.gridTemplateColumns)

  const rowHeights = gridTemplateToNumbers(containerGridProperties.gridTemplateRows)

  if (columnWidths == null || rowHeights == null) {
    return null
  }

  const cellRects: Array<Array<CanvasRectangle>> = []
  let yOffset = globalFrame.y + (padding.top ?? 0)
  rowHeights.forEach((height) => {
    let xOffset = globalFrame.x + (padding.left ?? 0)
    const rowRects: CanvasRectangle[] = []
    columnWidths.forEach((width) => {
      const rect = canvasRectangle({ x: xOffset, y: yOffset, width: width, height: height })
      rowRects.push(rect)
      xOffset += width + (columnGap ?? 0)
    })
    cellRects.push(rowRects)
    yOffset += height + (rowGap ?? 0)
  })

  return cellRects
}

export const getGlobalFramesOfGridCells = memoize(getGlobalFramesOfGridCellsInner)

function gridTemplateToNumbers(gridTemplate: GridTemplate | null): Array<number> | null {
  if (gridTemplate?.type !== 'DIMENSIONS') {
    return null
  }

  const result: Array<number> = []

  for (const dimension of gridTemplate.dimensions) {
    if (dimension.type !== 'NUMBER') {
      return null
    }
    result.push(dimension.value.value)
  }

  return result
}
