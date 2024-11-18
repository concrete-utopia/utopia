/** @jsxRuntime classic */
/** @jsx jsx */
import { sides, type Sides } from 'utopia-api/core'
import type { ElementPath } from 'utopia-shared/src/types'
import {
  isStaticGridRepeat,
  parseCSSLength,
  printGridAutoOrTemplateBase,
} from '../../inspector/common/css-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import type { BorderWidths } from '../../../core/shared/element-template'
import { type GridAutoOrTemplateBase } from '../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
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
  CanvasContainerID,
  EdgePositionBottom,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
} from '../canvas-types'
import {
  GridControlsComponent,
  GridResizeControlsComponent,
  GridRowColumnResizingControlsComponent,
} from './grid-controls'
import { isEdgePositionOnSide } from '../canvas-utils'
import { getFromElement } from '../direct-dom-lookups'
import { applicativeSidesPxTransform, getGridContainerProperties } from '../dom-walker'
import { applicative4Either, defaultEither, isRight, mapEither } from '../../../core/shared/either'
import { domRectToScaledCanvasRectangle, getRoundingFn } from '../../../core/shared/dom-utils'
import Utils from '../../../utils/utils'
import { useMonitorChangesToElements } from '../../../components/editor/store/store-monitor'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import type { CSSProperties } from 'react'
import {
  gridContainerIdentifier,
  pathOfGridFromGridIdentifier,
  type GridIdentifier,
} from '../../editor/store/editor-state'
import { findOriginalGrid } from '../canvas-strategies/strategies/grid-helpers'
import * as React from 'react'
import { addChangeCallback, removeChangeCallback } from '../observers'

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

export type GridMeasurementHelperData = {
  frame: CanvasRectangle
  rows: number
  columns: number
  cells: number
  computedStyling: CSSProperties
  gridTemplateColumns: GridAutoOrTemplateBase | null
  gridTemplateRows: GridAutoOrTemplateBase | null
  gridTemplateColumnsFromProps: GridAutoOrTemplateBase | null
  gridTemplateRowsFromProps: GridAutoOrTemplateBase | null
  border: BorderWidths
  gap: number | null
  rowGap: number | null
  columnGap: number | null
  padding: Sides
}

export function getGridMeasurementHelperData(
  elementPath: ElementPath,
  scale: number,
): GridMeasurementHelperData | undefined {
  return getFromElement(elementPath, gridMeasurementHelperDataFromElement(scale))
}

function getStylingSubset(styling: CSSStyleDeclaration): CSSProperties {
  // Fields chosen to not overlap with any others, so as to not make React complain.
  return {
    gridAutoFlow: styling.gridAutoFlow,
    gridAutoColumns: styling.gridAutoColumns,
    gridAutoRows: styling.gridAutoRows,
    gridTemplateColumns: styling.gridTemplateColumns,
    gridTemplateRows: styling.gridTemplateRows,
    gridColumn: styling.gridColumn,
    gridRow: styling.gridRow,
    gap: styling.gap,
    rowGap: styling.rowGap,
    columnGap: styling.columnGap,
    justifyContent: styling.justifyContent,
    alignContent: styling.alignContent,
    padding: styling.padding,
    paddingTop: styling.paddingTop,
    paddingLeft: styling.paddingLeft,
    paddingBottom: styling.paddingBottom,
    paddingRight: styling.paddingRight,
    borderTop: styling.borderTopWidth,
    borderLeft: styling.borderLeftWidth,
    borderBottom: styling.borderBottomWidth,
    borderRight: styling.borderRightWidth,
  }
}

export function gridMeasurementHelperDataFromElement(
  scale: number,
): (element: HTMLElement) => GridMeasurementHelperData | undefined {
  return (element) => {
    const canvasRootContainer = document.getElementById(CanvasContainerID)
    if (canvasRootContainer == null) {
      return undefined
    }

    const computedStyle = getComputedStyle(element)
    const boundingRectangle = element.getBoundingClientRect()

    const computedStyling: CSSProperties = getStylingSubset(computedStyle)

    const containerGridProperties = getGridContainerProperties(computedStyle)
    const containerGridPropertiesFromProps = getGridContainerProperties(element.style)

    const columns = getCellsCount(containerGridProperties.gridTemplateColumns)
    const rows = getCellsCount(containerGridProperties.gridTemplateRows)
    const borderTopWidth = parseCSSLength(computedStyle.borderTopWidth)
    const borderRightWidth = parseCSSLength(computedStyle.borderRightWidth)
    const borderBottomWidth = parseCSSLength(computedStyle.borderBottomWidth)
    const borderLeftWidth = parseCSSLength(computedStyle.borderLeftWidth)
    const border: BorderWidths = {
      top: isRight(borderTopWidth) ? borderTopWidth.value.value : 0,
      right: isRight(borderRightWidth) ? borderRightWidth.value.value : 0,
      bottom: isRight(borderBottomWidth) ? borderBottomWidth.value.value : 0,
      left: isRight(borderLeftWidth) ? borderLeftWidth.value.value : 0,
    }
    const padding = defaultEither(
      sides(undefined, undefined, undefined, undefined),
      applicative4Either(
        applicativeSidesPxTransform,
        parseCSSLength(computedStyle.paddingTop),
        parseCSSLength(computedStyle.paddingRight),
        parseCSSLength(computedStyle.paddingBottom),
        parseCSSLength(computedStyle.paddingLeft),
      ),
    )
    const gap = defaultEither(
      null,
      mapEither((n) => n.value, parseCSSLength(computedStyle.gap)),
    )

    const rowGap = defaultEither(
      null,
      mapEither((n) => n.value, parseCSSLength(computedStyle.rowGap)),
    )

    const columnGap = defaultEither(
      null,
      mapEither((n) => n.value, parseCSSLength(computedStyle.columnGap)),
    )

    const elementRect = domRectToScaledCanvasRectangle(
      boundingRectangle,
      1 / scale,
      getRoundingFn('nearest-half'),
    )
    const parentRect = domRectToScaledCanvasRectangle(
      canvasRootContainer.getBoundingClientRect(),
      1 / scale,
      getRoundingFn('nearest-half'),
    )
    const frame = Utils.offsetRect(elementRect, Utils.negate(parentRect))

    return {
      frame: frame,
      gridTemplateColumns: containerGridProperties.gridTemplateColumns,
      gridTemplateRows: containerGridProperties.gridTemplateRows,
      gridTemplateColumnsFromProps: containerGridPropertiesFromProps.gridTemplateColumns,
      gridTemplateRowsFromProps: containerGridPropertiesFromProps.gridTemplateRows,
      border: border,
      padding: padding,
      gap: gap,
      rowGap: rowGap,
      columnGap: columnGap,
      rows: rows,
      columns: columns,
      cells: columns * rows,
      computedStyling: computedStyling,
    }
  }
}

function useObserversToWatch(elementPathOrPaths: Array<ElementPath> | ElementPath): number {
  // Used to trigger extra renders.
  const [counter, setCounter] = React.useState(0)
  const bumpCounter = React.useCallback(() => {
    setCounter((value) => value + 1)
  }, [])

  // Need to use the mount count for the callback trigger.
  const mountCount = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.mountCount,
    'useObserversToWatch mountCount',
  )

  React.useEffect(() => {
    // Add the change callback(s) for the element path or paths.
    if (Array.isArray(elementPathOrPaths)) {
      for (const elementPath of elementPathOrPaths) {
        addChangeCallback(mountCount, elementPath, bumpCounter)
      }
    } else {
      addChangeCallback(mountCount, elementPathOrPaths, bumpCounter)
    }

    return function cleanup {
      if (Array.isArray(elementPathOrPaths)) {
        for (const elementPath of elementPathOrPaths) {
          removeChangeCallback(elementPath, bumpCounter)
        }
      } else {
        removeChangeCallback(elementPathOrPaths, bumpCounter)
      }
    }
  }, [mountCount, elementPathOrPaths, bumpCounter])

  return counter
}

export function useGridMeasurementHelperData(
  elementPath: ElementPath,
): GridMeasurementHelperData | undefined {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'useGridMeasurementHelperData scale',
  )

  useMonitorChangesToElements([elementPath])

  useObserversToWatch(elementPath)

  return useKeepReferenceEqualityIfPossible(getGridMeasurementHelperData(elementPath, scale))
}

export type GridData = GridMeasurementHelperData & {
  identifier: GridIdentifier
}

export function useGridData(gridIdentifiers: GridIdentifier[]): GridData[] {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'useGridData scale',
  )

  const elementPaths = React.useMemo(() => {
    return gridIdentifiers.map(pathOfGridFromGridIdentifier)
  }, [gridIdentifiers])

  useMonitorChangesToElements(elementPaths)

  useObserversToWatch(elementPaths)

  const grids = useEditorState(
    Substores.metadata,
    (store) => {
      return mapDropNulls((view) => {
        const originalGridPath = findOriginalGrid(
          store.editor.jsxMetadata,
          pathOfGridFromGridIdentifier(view), // TODO: this is temporary, we will need to handle showing a grid control on the parent dom element of a grid item
        )
        if (originalGridPath == null) {
          return null
        }
        const element = MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          originalGridPath,
        )

        const targetGridContainer = MetadataUtils.isGridLayoutedContainer(element) ? element : null
        if (targetGridContainer == null) {
          return null
        }

        const helperData = getGridMeasurementHelperData(originalGridPath, scale)
        if (helperData == null) {
          return null
        }

        const gridData: GridData = {
          ...helperData,
          identifier: gridContainerIdentifier(originalGridPath),
        }
        return gridData
      }, gridIdentifiers)
    },
    'useGridData',
  )

  return useKeepReferenceEqualityIfPossible(grids)
}

interface GridRowColumnResizingControlsProps {
  target: GridIdentifier
}

export const GridRowColumnResizingControls =
  controlForStrategyMemoized<GridRowColumnResizingControlsProps>(
    GridRowColumnResizingControlsComponent,
  )

export const GridControlsKey = (gridPath: ElementPath) => `grid-controls-${EP.toString(gridPath)}`
export const GridControlKey = (gridPath: ElementPath) => `grid-control-${EP.toString(gridPath)}`

export const GridMeasurementHelpersKey = (gridPath: ElementPath) =>
  `grid-measurement-helpers-${EP.toString(gridPath)}`
export const GridMeasurementHelperKey = (gridPath: ElementPath) =>
  `grid-measurement-helper-${EP.toString(gridPath)}`
export const GridElementContainingBlockKey = (gridPath: ElementPath) =>
  `grid-measurement-containing-block-${EP.toString(gridPath)}`

export interface GridControlProps {
  grid: GridData
}

export interface GridControlsProps {
  type: 'GRID_CONTROLS_PROPS'
  targets: GridIdentifier[]
}

export function isGridControlsProps(props: unknown): props is GridControlsProps {
  return (props as GridControlsProps).type === 'GRID_CONTROLS_PROPS'
}

export const GridControls = controlForStrategyMemoized<GridControlsProps>(GridControlsComponent)

interface GridResizeControlProps {
  target: GridIdentifier
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
  gridPath: GridIdentifier | Array<GridIdentifier>,
  whenToShow: WhenToShowControl = 'always-visible',
  suffix: string | null = null,
): ControlWithProps<any> {
  return {
    control: GridControls,
    props: {
      type: 'GRID_CONTROLS_PROPS',
      targets: Array.isArray(gridPath) ? gridPath : [gridPath],
    },
    key: `GridControls${suffix == null ? '' : suffix}`,
    show: whenToShow,
    priority: 'bottom',
  }
}
