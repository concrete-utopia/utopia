/** @jsxRuntime classic */
/** @jsx jsx */
import type { ElementPath } from 'utopia-shared/src/types'
import { printGridAutoOrTemplateBase } from '../../inspector/common/css-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { type GridAutoOrTemplateBase } from '../../../core/shared/element-template'
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
  GridResizeControlsComponent,
  GridRowColumnResizingControlsComponent,
} from './grid-controls'
import { isEdgePositionOnSide } from '../canvas-utils'
import { useMonitorChangesToElements } from '../../../components/editor/store/store-monitor'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import {
  gridContainerIdentifier,
  gridItemIdentifier,
  pathOfGridFromGridIdentifier,
  type GridIdentifier,
} from '../../editor/store/editor-state'
import { findOriginalGrid } from '../canvas-strategies/strategies/grid-helpers'
import * as React from 'react'
import type { GridData } from './grid-measurements'
import { getGridMeasurementHelperData, useObserversToWatch } from './grid-measurements'

export const GridMeasurementHelperMap = { current: new WeakMap<HTMLElement, string>() }

export const GridCellTestId = (elementPath: ElementPath) => `grid-cell-${EP.toString(elementPath)}`

export function getNullableAutoOrTemplateBaseString(
  template: GridAutoOrTemplateBase | null,
): string | undefined {
  if (template == null) {
    return undefined
  } else {
    return printGridAutoOrTemplateBase(template)
  }
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
        switch (view.type) {
          case 'GRID_CONTAINER': {
            const originalGridPath = findOriginalGrid(store.editor.jsxMetadata, view.container)
            if (originalGridPath == null) {
              return null
            }
            const element = MetadataUtils.findElementByElementPath(
              store.editor.jsxMetadata,
              originalGridPath,
            )

            const targetGridContainer = MetadataUtils.isGridLayoutedContainer(element)
              ? element
              : null
            if (targetGridContainer == null) {
              return null
            }

            const helperData = getGridMeasurementHelperData(originalGridPath, scale, 'element')
            if (helperData == null) {
              return null
            }

            const gridData: GridData = {
              ...helperData,
              identifier: gridContainerIdentifier(originalGridPath),
            }
            return gridData
          }
          case 'GRID_ITEM':
            const item = MetadataUtils.isGridItem(store.editor.jsxMetadata, view.item)
              ? MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, view.item)
              : null
            if (item == null) {
              return null
            }

            const helperData = getGridMeasurementHelperData(view.item, scale, 'parent')
            if (helperData == null) {
              return null
            }

            const gridData: GridData = {
              ...helperData,
              identifier: gridItemIdentifier(view.item),
            }
            return gridData
          default:
            assertNever(view)
        }
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
  }
}
