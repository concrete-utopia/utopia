/** @jsxRuntime classic */
/** @jsx jsx */
import type { Sides } from 'utopia-api/core'
import type { ElementPath } from 'utopia-shared/src/types'
import { isStaticGridRepeat, printGridAutoOrTemplateBase } from '../../inspector/common/css-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { type GridAutoOrTemplateBase } from '../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import { isFiniteRectangle } from '../../../core/shared/math-utils'
import { assertNever } from '../../../core/shared/utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type {
  ControlWithProps,
  WhenToShowControl,
} from '../canvas-strategies/canvas-strategy-types'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import type { GridResizeEdge } from '../canvas-strategies/interaction-state'
import type { EdgePosition } from '../canvas-types'
import {
  EdgePositionBottom,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
} from '../canvas-types'
import {
  GridControlsComponent,
  type GridMeasurementHelperProps,
  GridResizeControlsComponent,
  GridRowColumnResizingControlsComponent,
} from './grid-controls'
import { isEdgePositionOnSide } from '../canvas-utils'

export const GridCellTestId = (elementPath: ElementPath) => `grid-cell-${EP.toString(elementPath)}`

function getCellsCount(template: GridAutoOrTemplateBase | null): number {
  if (template == null) {
    return 0
  }

  switch (template.type) {
    case 'DIMENSIONS':
      return template.dimensions.reduce((acc, cur) => {
        return acc + (isStaticGridRepeat(cur) ? cur.times : 1)
      }, 0)
    case 'FALLBACK':
      return 0
    default:
      assertNever(template)
  }
}

export function getNullableAutoOrTemplateBaseString(
  template: GridAutoOrTemplateBase | null,
): string | undefined {
  if (template == null) {
    return undefined
  } else {
    return printGridAutoOrTemplateBase(template)
  }
}

export type GridData = {
  elementPath: ElementPath
  frame: CanvasRectangle
  gridTemplateColumns: GridAutoOrTemplateBase | null
  gridTemplateRows: GridAutoOrTemplateBase | null
  gridTemplateColumnsFromProps: GridAutoOrTemplateBase | null
  gridTemplateRowsFromProps: GridAutoOrTemplateBase | null
  gap: number | null
  justifyContent: string | null
  alignContent: string | null
  rowGap: number | null
  columnGap: number | null
  padding: Sides
  rows: number
  columns: number
  cells: number
  metadata: ElementInstanceMetadata
}

export function useGridMeasurentHelperData(
  elementPath: ElementPath,
): GridMeasurementHelperProps | null {
  const grid = useEditorState(
    Substores.metadata,
    (store) => {
      const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath)

      const targetGridContainer = MetadataUtils.isGridLayoutedContainer(element) ? element : null

      if (
        targetGridContainer == null ||
        targetGridContainer.globalFrame == null ||
        !isFiniteRectangle(targetGridContainer.globalFrame)
      ) {
        return null
      }

      const columns = getCellsCount(
        targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns,
      )
      const rows = getCellsCount(
        targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows,
      )

      return {
        elementPath: targetGridContainer.elementPath,
        frame: targetGridContainer.globalFrame,
        gridTemplateColumns:
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns,
        gridTemplateRows:
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows,
        gap: targetGridContainer.specialSizeMeasurements.gap,
        rowGap: targetGridContainer.specialSizeMeasurements.rowGap,
        columnGap: targetGridContainer.specialSizeMeasurements.columnGap,
        justifyContent: targetGridContainer.specialSizeMeasurements.justifyContent,
        alignContent: targetGridContainer.specialSizeMeasurements.alignContent,
        padding: targetGridContainer.specialSizeMeasurements.padding,
        columns: columns,
        cells: rows * columns,
      }
    },
    'useGridMeasurentHelperData',
  )

  return grid
}

export function useGridData(elementPaths: ElementPath[]): GridData[] {
  const grids = useEditorState(
    Substores.metadata,
    (store) => {
      return mapDropNulls((view) => {
        const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, view)

        const targetGridContainer = MetadataUtils.isGridLayoutedContainer(element) ? element : null

        if (
          targetGridContainer == null ||
          targetGridContainer.globalFrame == null ||
          !isFiniteRectangle(targetGridContainer.globalFrame)
        ) {
          return null
        }

        const gap = targetGridContainer.specialSizeMeasurements.gap
        const rowGap = targetGridContainer.specialSizeMeasurements.rowGap
        const columnGap = targetGridContainer.specialSizeMeasurements.columnGap
        const padding = targetGridContainer.specialSizeMeasurements.padding

        const gridTemplateColumns =
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns
        const gridTemplateRows =
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows
        const gridTemplateColumnsFromProps =
          targetGridContainer.specialSizeMeasurements.containerGridPropertiesFromProps
            .gridTemplateColumns
        const gridTemplateRowsFromProps =
          targetGridContainer.specialSizeMeasurements.containerGridPropertiesFromProps
            .gridTemplateRows

        const columns = getCellsCount(
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns,
        )
        const rows = getCellsCount(
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows,
        )

        return {
          elementPath: targetGridContainer.elementPath,
          metadata: targetGridContainer,
          frame: targetGridContainer.globalFrame,
          gridTemplateColumns: gridTemplateColumns,
          gridTemplateRows: gridTemplateRows,
          gridTemplateColumnsFromProps: gridTemplateColumnsFromProps,
          gridTemplateRowsFromProps: gridTemplateRowsFromProps,
          gap: gap,
          rowGap: rowGap,
          columnGap: columnGap,
          justifyContent: targetGridContainer.specialSizeMeasurements.justifyContent,
          alignContent: targetGridContainer.specialSizeMeasurements.alignContent,
          padding: padding,
          rows: rows,
          columns: columns,
          cells: rows * columns,
        }
      }, elementPaths)
    },
    'useGridData',
  )

  return grids
}

interface GridRowColumnResizingControlsProps {
  target: ElementPath
}

export const GridRowColumnResizingControls =
  controlForStrategyMemoized<GridRowColumnResizingControlsProps>(
    GridRowColumnResizingControlsComponent,
  )

export const GridControlsKey = (gridPath: ElementPath) => `grid-controls-${EP.toString(gridPath)}`
export const GridControlKey = (gridPath: ElementPath) => `grid-control-${EP.toString(gridPath)}`

export interface GridControlProps {
  grid: GridData
}

export interface GridControlsProps {
  targets: ElementPath[]
}

export const GridControls = controlForStrategyMemoized<GridControlsProps>(GridControlsComponent)

interface GridResizeControlProps {
  target: ElementPath
}

export const GridResizeControls = controlForStrategyMemoized<GridResizeControlProps>(
  GridResizeControlsComponent,
)

export function gridEdgeToEdgePosition(edge: GridResizeEdge): EdgePosition {
  switch (edge) {
    case 'column-end':
      return EdgePositionRight
    case 'column-start':
      return EdgePositionLeft
    case 'row-end':
      return EdgePositionBottom
    case 'row-start':
      return EdgePositionTop
    default:
      assertNever(edge)
  }
}

export function edgePositionToGridResizeEdge(position: EdgePosition): GridResizeEdge | null {
  if (!isEdgePositionOnSide(position)) {
    return null
  } else if (position.x === 0) {
    return 'column-start'
  } else if (position.x === 1) {
    return 'column-end'
  } else if (position.y === 0) {
    return 'row-start'
  } else if (position.y === 1) {
    return 'row-end'
  } else {
    return null
  }
}

export function controlsForGridPlaceholders(
  gridPath: ElementPath,
  whenToShow: WhenToShowControl = 'always-visible',
): ControlWithProps<any> {
  return {
    control: GridControls,
    props: { targets: [gridPath] },
    key: GridControlsKey(gridPath),
    show: whenToShow,
    priority: 'bottom',
  }
}
