import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  GridPositionValue,
} from '../../../../core/shared/element-template'
import {
  gridPositionValue,
  type ElementInstanceMetadata,
  type GridContainerProperties,
  type GridElementProperties,
  type GridPosition,
} from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasVector,
  isInfinityRectangle,
  offsetPoint,
} from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { absolute } from '../../../../utils/utils'
import type { GridDimension } from '../../../inspector/common/css-utils'
import {
  cssNumber,
  gridCSSRepeat,
  isCSSKeyword,
  isGridCSSRepeat,
  isStaticGridRepeat,
} from '../../../inspector/common/css-utils'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import { reorderElement } from '../../commands/reorder-element-command'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
import { setProperty } from '../../commands/set-property-command'
import type {
  CustomStrategyState,
  CustomStrategyStatePatch,
  GridCustomStrategyState,
} from '../canvas-strategy-types'
import type { DragInteractionData } from '../interaction-state'
import type { GridCellCoordinates } from './grid-cell-bounds'
import { getGridCellUnderMouseFromMetadata, gridCellCoordinates } from './grid-cell-bounds'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { assertNever } from '../../../../core/shared/utils'

export function runGridRearrangeMove(
  targetElement: ElementPath,
  selectedElement: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
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
    case 'absolute': {
      const absoluteMoveCommands = gridChildAbsoluteMoveCommands(
        MetadataUtils.findElementByElementPath(jsxMetadata, targetElement),
        MetadataUtils.getFrameOrZeroRectInCanvasCoords(grid.elementPath, jsxMetadata),
        interactionData,
      )
      return {
        commands: absoluteMoveCommands,
        targetCell: targetCellData ?? customState.targetCellData,
        originalRootCell: rootCell,
        draggingFromCell: draggingFromCell,
        targetRootCell: gridCellCoordinates(row.start, column.start),
      }
    }
    case 'rearrange': {
      const targetRootCell = gridCellCoordinates(row.start, column.start)
      const canvasRect = getGlobalFrameOfGridCell(containerMetadata, targetRootCell)
      const absoluteMoveCommands =
        canvasRect == null
          ? []
          : gridChildAbsoluteMoveCommands(
              MetadataUtils.findElementByElementPath(jsxMetadata, targetElement),
              canvasRect,
              interactionData,
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
    default:
      assertNever(moveType)
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
  containingRect: CanvasRectangle,
  dragInteractionData: DragInteractionData,
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

  const dragOffset = offsetPoint(
    dragInteractionData.originalDragStart,
    dragInteractionData.drag ?? canvasPoint({ x: 0, y: 0 }),
  )

  const offset = canvasVector({
    x: dragOffset.x - containingRect.x - offsetInTarget.x,
    y: dragOffset.y - containingRect.y - offsetInTarget.y,
  })

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
  | 'absolute' // a regular absolute move, relative to the grid

function getGridMoveType(params: {
  originalElementMetadata: ElementInstanceMetadata
  possiblyReorderIndex: number
  cellsSortedByPosition: SortableGridElementProperties[]
}): GridMoveType {
  const specialSizeMeasurements = params.originalElementMetadata.specialSizeMeasurements
  if (MetadataUtils.isPositionAbsolute(params.originalElementMetadata)) {
    return MetadataUtils.hasNoGridCellPositioning(specialSizeMeasurements)
      ? 'absolute'
      : 'rearrange'
  }
  if (params.possiblyReorderIndex >= params.cellsSortedByPosition.length) {
    return 'rearrange'
  }

  const elementGridProperties = specialSizeMeasurements.elementGridProperties

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

export function getGlobalFrameOfGridCell(
  grid: ElementInstanceMetadata,
  coords: GridCellCoordinates,
): CanvasRectangle | null {
  const gridCellGlobalFrames = grid.specialSizeMeasurements.gridCellGlobalFrames
  if (gridCellGlobalFrames == null) {
    return null
  }

  return gridCellGlobalFrames[coords.row - 1]?.[coords.column - 1] ?? null
}

type DimensionIndexes = {
  originalIndex: number // the index of this element in the original values
  repeatedIndex: number // the index of this element, if it's generated via a repeat, inside the repeated values array definition
}

export type ExpandedGridDimension = GridDimension & {
  indexes: DimensionIndexes
}

function expandedGridDimension(
  dim: GridDimension,
  originalIndex: number,
  repeatedIndex: number = 0,
): ExpandedGridDimension {
  return {
    ...dim,
    indexes: {
      originalIndex: originalIndex,
      repeatedIndex: repeatedIndex,
    },
  }
}

export function expandGridDimensions(template: GridDimension[]): ExpandedGridDimension[] {
  // Expanded representation of the original values, where repeated elements are serialized.
  // Each element also contains the indexes information to be used later on to build the resized
  // template string.
  return template.reduce((acc, cur, index) => {
    if (isStaticGridRepeat(cur)) {
      const repeatGroup = cur.value.map((dim, repeatedIndex) =>
        expandedGridDimension(dim, index, repeatedIndex),
      )
      let expanded: ExpandedGridDimension[] = []
      for (let i = 0; i < cur.times; i++) {
        expanded.push(...repeatGroup)
      }
      return [...acc, ...expanded]
    } else {
      return [...acc, expandedGridDimension(cur, index)]
    }
  }, [] as ExpandedGridDimension[])
}

function alterGridTemplateDimensions(params: {
  originalValues: GridDimension[]
  target: ExpandedGridDimension
  patch: AlterGridTemplateDimensionPatch
}): GridDimension[] {
  return mapDropNulls((dim, index) => {
    if (index !== params.target.indexes.originalIndex) {
      return dim
    } else if (isGridCSSRepeat(dim)) {
      const repeatedIndex = params.target.indexes.repeatedIndex ?? 0
      const before = dim.value.slice(0, repeatedIndex)
      const after = dim.value.slice(repeatedIndex + 1)
      switch (params.patch.type) {
        case 'REMOVE':
          if (before.length + after.length === 0) {
            return null
          }
          return gridCSSRepeat(dim.times, [...before, ...after])
        case 'REPLACE':
          return gridCSSRepeat(dim.times, [...before, params.patch.newValue, ...after])
        default:
          assertNever(params.patch)
      }
    } else {
      switch (params.patch.type) {
        case 'REPLACE':
          return params.patch.newValue
        case 'REMOVE':
          return null
        default:
          assertNever(params.patch)
      }
    }
  }, params.originalValues)
}

export type ReplaceGridDimensionPatch = {
  type: 'REPLACE'
  newValue: GridDimension
}

export type RemoveGridDimensionPatch = {
  type: 'REMOVE'
}

export type AlterGridTemplateDimensionPatch = ReplaceGridDimensionPatch | RemoveGridDimensionPatch

export function replaceGridTemplateDimensionAtIndex(
  template: GridDimension[],
  expanded: ExpandedGridDimension[],
  index: number,
  newValue: GridDimension,
): GridDimension[] {
  return alterGridTemplateDimensions({
    originalValues: template,
    target: expanded[index],
    patch: {
      type: 'REPLACE',
      newValue: newValue,
    },
  })
}

export function removeGridTemplateDimensionAtIndex(
  template: GridDimension[],
  expanded: ExpandedGridDimension[],
  index: number,
): GridDimension[] {
  return alterGridTemplateDimensions({
    originalValues: template,
    target: expanded[index],
    patch: {
      type: 'REMOVE',
    },
  })
}

// Return an array of related indexes to a given index inside a grid's template dimensions.
export function getGridRelatedIndexes(params: {
  template: GridDimension[]
  index: number
}): number[] {
  let relatedIndexes: number[][][] = [] // This looks scary but it's not! It's just a list of indexes, containing a list of the indexes *per group element*.
  // For example, 1fr repeat(3, 10px 20px) 1fr, will be represented as:
  /**
   * [
   *  [ [0] ]
   *  [ [1, 3] [2, 4]  ]
   *  [ [5] ]
   * ]
   */
  let elementCount = 0 // basically the expanded index
  for (const dim of params.template) {
    if (isStaticGridRepeat(dim)) {
      let groupIndexes: number[][] = []
      // for each value push the related indexes as many times as the repeats counter
      for (let valueIndex = 0; valueIndex < dim.value.length; valueIndex++) {
        let repeatedValueIndexes: number[] = []
        for (let repeatIndex = 0; repeatIndex < dim.times; repeatIndex++) {
          repeatedValueIndexes.push(elementCount + valueIndex + repeatIndex * dim.value.length)
        }
        groupIndexes.push(repeatedValueIndexes)
      }
      relatedIndexes.push(groupIndexes)
      elementCount += dim.value.length * dim.times // advance the counter as many times as the repeated values *combined*
    } else {
      relatedIndexes.push([[elementCount]])
      elementCount++
    }
  }

  // Now, expand the indexes calculated above so they "flatten out" to match the generated values
  let expandedRelatedIndexes: number[][] = []
  params.template.forEach((dim, dimIndex) => {
    if (isStaticGridRepeat(dim)) {
      for (let repeatIndex = 0; repeatIndex < dim.times * dim.value.length; repeatIndex++) {
        const indexes = relatedIndexes[dimIndex][repeatIndex % dim.value.length]
        expandedRelatedIndexes.push(indexes)
      }
    } else {
      expandedRelatedIndexes.push(relatedIndexes[dimIndex][0])
    }
  })

  return expandedRelatedIndexes[params.index] ?? []
}

export function getMetadataWithGridCellBounds(
  path: ElementPath | null | undefined,
  startingMetadata: ElementInstanceMetadataMap,
  latestMetadata: ElementInstanceMetadataMap,
  customStrategyState: CustomStrategyState,
): {
  metadata: ElementInstanceMetadata | null
  customStrategyState: CustomStrategyState | null
} {
  if (path == null) {
    return {
      metadata: null,
      customStrategyState: null,
    }
  }

  const fromStartingMetadata = MetadataUtils.findElementByElementPath(startingMetadata, path)

  if (fromStartingMetadata?.specialSizeMeasurements.gridCellGlobalFrames != null) {
    return {
      metadata: fromStartingMetadata,
      customStrategyState: null,
    }
  }

  const fromStrategyState = customStrategyState.grid.metadataCacheForGrids[EP.toString(path)]
  if (fromStrategyState != null) {
    return {
      metadata: fromStrategyState,
      customStrategyState: null,
    }
  }

  const fromLatestMetadata = MetadataUtils.findElementByElementPath(latestMetadata, path)
  if (fromLatestMetadata?.specialSizeMeasurements.gridCellGlobalFrames != null) {
    return {
      metadata: fromLatestMetadata,
      customStrategyState: {
        ...customStrategyState,
        grid: {
          ...customStrategyState.grid,
          metadataCacheForGrids: {
            ...customStrategyState.grid.metadataCacheForGrids,
            [EP.toString(path)]: fromLatestMetadata,
          },
        },
      },
    }
  }

  return {
    metadata: fromStartingMetadata,
    customStrategyState: null,
  }
}
