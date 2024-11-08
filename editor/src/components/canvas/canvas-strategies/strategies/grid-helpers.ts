import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  GridAutoOrTemplateBase,
  GridPositionValue,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import {
  type ElementInstanceMetadata,
  type GridContainerProperties,
  type GridElementProperties,
  type GridPosition,
} from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { assertNever } from '../../../../core/shared/utils'
import type { GridDimension } from '../../../inspector/common/css-utils'
import {
  gridCSSRepeat,
  isCSSKeyword,
  isGridCSSRepeat,
  isStaticGridRepeat,
  printGridAutoOrTemplateBase,
} from '../../../inspector/common/css-utils'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import type { PropertyToUpdate } from '../../commands/set-property-command'
import {
  propertyToDelete,
  propertyToSet,
  setProperty,
  updateBulkProperties,
} from '../../commands/set-property-command'
import type { DragInteractionData } from '../interaction-state'
import type { GridCellCoordinates } from './grid-cell-bounds'
import {
  getClosestGridCellToPoint,
  getGridChildCellCoordBoundsFromCanvas,
  gridCellCoordinates,
} from './grid-cell-bounds'

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

  const lineColumnStart = asMaybeNamedLineOrValue(gridTemplate, 'column', columnStart)
  const lineColumnEnd = asMaybeNamedLineOrValue(gridTemplate, 'column', columnEnd)
  const lineRowStart = asMaybeNamedLineOrValue(gridTemplate, 'row', rowStart)
  const lineRowEnd = asMaybeNamedLineOrValue(gridTemplate, 'row', rowEnd)

  if (columnStart != null && columnStart === columnEnd) {
    commands.push(
      setProperty('always', elementPath, PP.create('style', 'gridColumn'), lineColumnStart),
    )
  } else if (
    columnStart != null &&
    typeof columnStart === 'number' &&
    columnEnd != null &&
    typeof columnEnd === 'number' &&
    columnStart === columnEnd - 1
  ) {
    commands.push(
      setProperty('always', elementPath, PP.create('style', 'gridColumn'), lineColumnStart),
    )
  } else {
    if (columnStart != null) {
      commands.push(
        setProperty('always', elementPath, PP.create('style', 'gridColumnStart'), lineColumnStart),
      )
    }
    if (columnEnd != null) {
      commands.push(
        setProperty('always', elementPath, PP.create('style', 'gridColumnEnd'), lineColumnEnd),
      )
    }
  }

  if (rowStart != null && rowStart === rowEnd) {
    commands.push(setProperty('always', elementPath, PP.create('style', 'gridRow'), lineRowStart))
  } else if (
    rowStart != null &&
    typeof rowStart === 'number' &&
    rowEnd != null &&
    typeof rowEnd === 'number' &&
    rowStart === rowEnd - 1
  ) {
    commands.push(setProperty('always', elementPath, PP.create('style', 'gridRow'), lineRowStart))
  } else {
    if (rowStart != null) {
      commands.push(
        setProperty('always', elementPath, PP.create('style', 'gridRowStart'), lineRowStart),
      )
    }
    if (rowEnd != null) {
      commands.push(
        setProperty('always', elementPath, PP.create('style', 'gridRowEnd'), lineRowEnd),
      )
    }
  }

  return commands
}

export interface TargetGridCellData {
  gridCellCoordinates: GridCellCoordinates
  cellCanvasRectangle: CanvasRectangle
}

function getGridChildCellCoordBoundsFromProps(
  element: ElementInstanceMetadata,
  fallback: { row: number; column: number; width?: number; height?: number },
): {
  row: number
  width: number
  column: number
  height: number
} {
  // get the grid fixtures (start and end for column and row) from the element metadata
  function getGridProperty(field: keyof GridElementProperties, innerFallback: number) {
    const propValue = element.specialSizeMeasurements.elementGridProperties[field]
    if (propValue == null || isCSSKeyword(propValue)) {
      return innerFallback
    }
    return propValue.numericalPosition ?? innerFallback
  }
  const column = getGridProperty('gridColumnStart', fallback.column)
  const height = getGridProperty('gridColumnEnd', fallback.column + (fallback.width ?? 1)) - column
  const row = getGridProperty('gridRowStart', fallback.row)
  const width = getGridProperty('gridRowEnd', fallback.row + (fallback.height ?? 1)) - row

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

function asMaybeNamedLineOrValue(
  grid: GridContainerProperties,
  axis: 'row' | 'column',
  value: number | string | null,
): string | number {
  if (value == null) {
    return 1
  } else if (typeof value === 'number') {
    const template = axis === 'row' ? grid.gridTemplateRows : grid.gridTemplateColumns
    if (template?.type === 'DIMENSIONS') {
      const maybeLineStart = template.dimensions.at(value - 1)
      if (maybeLineStart != null && maybeLineStart.lineName != null) {
        return maybeLineStart.lineName
      }
    }
    return value === 0 ? 1 : value
  }
  return value
}

export type SortableGridElementProperties = GridElementProperties & {
  path: ElementPath
  index: number
}

export function sortElementsByGridPosition(gridTemplateColumns: number) {
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

function isGridPositionNumericValue(p: GridPosition | null): p is GridPositionValue {
  return p != null && !(isCSSKeyword(p) && p.value === 'auto')
}

export function getGridPositionIndex(props: {
  row: number
  column: number
  gridTemplateColumns: number
}): number {
  return (props.row - 1) * props.gridTemplateColumns + props.column - 1
}

export type GridCellGlobalFrames = Array<Array<CanvasRectangle>>

export function getGlobalFrameOfGridCellFromMetadata(
  grid: ElementInstanceMetadata,
  coords: GridCellCoordinates,
): CanvasRectangle | null {
  const gridCellGlobalFrames = grid.specialSizeMeasurements.gridCellGlobalFrames
  if (gridCellGlobalFrames == null) {
    return null
  }

  return getGlobalFrameOfGridCell(gridCellGlobalFrames, coords)
}

export function getGlobalFrameOfGridCell(
  gridCellGlobalFrames: GridCellGlobalFrames,
  coords: GridCellCoordinates,
): CanvasRectangle | null {
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
          return gridCSSRepeat(dim.times, [...before, ...after], dim.lineName)
        case 'REPLACE':
          return gridCSSRepeat(
            dim.times,
            [...before, params.patch.newValue, ...after],
            dim.lineName,
          )
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

export function getOriginalElementGridConfiguration(
  gridCellGlobalFrames: GridCellGlobalFrames,
  interactionData: DragInteractionData,
  originalElement: ElementInstanceMetadata,
) {
  const draggingFromCellCoords = getClosestGridCellToPoint(
    gridCellGlobalFrames,
    interactionData.dragStart,
  )?.gridCellCoordinates
  if (draggingFromCellCoords == null) {
    return null
  }

  // measured cell coord bounds on the canvas, this is the default when the cell position is not explicitly set
  const originalElementCellCoordsOnCanvas = getGridChildCellCoordBoundsFromCanvas(
    originalElement,
    gridCellGlobalFrames,
  )

  // get the bounds from the props, or the canvas, or just default to the cell of the starting mouse position
  const originalCellBounds = getGridChildCellCoordBoundsFromProps(
    originalElement,
    originalElementCellCoordsOnCanvas ?? draggingFromCellCoords,
  )

  // the cell position of the mouse relative to the original element (we have to keep this offset while dragging)
  const mouseCellPosInOriginalElement = getCellCoordsDelta(
    draggingFromCellCoords,
    originalCellBounds,
  )

  return {
    originalCellBounds,
    mouseCellPosInOriginalElement,
  }
}

export function findOriginalGrid(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): ElementPath | null {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
  if (elementMetadata == null) {
    return null
  }

  if (!MetadataUtils.isGridLayoutedContainer(elementMetadata)) {
    return null
  }

  if (!elementMetadata.specialSizeMeasurements.layoutSystemForChildrenInherited) {
    return path
  }

  const parentPath = EP.parentPath(path)
  if (parentPath == null) {
    return null
  }

  return findOriginalGrid(metadata, parentPath)
}

// Returns whether the given dimensions are made of just one item, being a CSS keyword with value "auto".
export function isJustAutoGridDimension(dimensions: GridDimension[]): boolean {
  return (
    dimensions.length === 1 &&
    dimensions[0].type === 'KEYWORD' &&
    dimensions[0].value.value === 'auto'
  )
}

type GridElementPinState = 'not-pinned' | 'auto-pinned' | 'pinned'

export function getGridElementPinState(
  elementGridPropertiesFromProps: GridElementProperties | null,
): GridElementPinState {
  if (
    elementGridPropertiesFromProps?.gridColumnEnd == null ||
    elementGridPropertiesFromProps?.gridColumnStart == null ||
    elementGridPropertiesFromProps?.gridRowEnd == null ||
    elementGridPropertiesFromProps?.gridRowStart == null
  ) {
    return 'not-pinned'
  }
  if (
    isGridPositionNumericValue(elementGridPropertiesFromProps?.gridColumnEnd ?? null) ||
    isGridPositionNumericValue(elementGridPropertiesFromProps?.gridColumnStart ?? null) ||
    isGridPositionNumericValue(elementGridPropertiesFromProps?.gridRowEnd ?? null) ||
    isGridPositionNumericValue(elementGridPropertiesFromProps?.gridRowStart ?? null)
  ) {
    return 'pinned'
  }
  return 'auto-pinned'
}

export function isFlowGridChild(child: ElementInstanceMetadata) {
  return (
    getGridElementPinState(child.specialSizeMeasurements.elementGridPropertiesFromProps) !==
    'pinned'
  )
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
