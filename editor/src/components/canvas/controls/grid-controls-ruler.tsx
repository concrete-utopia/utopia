/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import type { AnimationControls } from 'framer-motion'
import { motion, useAnimationControls } from 'framer-motion'
import type { CSSProperties } from 'react'
import React from 'react'
import type { Sides } from 'utopia-api/core'
import type { ElementPath } from 'utopia-shared/src/types'
import type {
  GridDimension,
  GridDiscreteDimension,
} from '../../../components/inspector/common/css-utils'
import {
  isCSSKeyword,
  isDynamicGridRepeat,
  isGridCSSRepeat,
  isStaticGridRepeat,
  printGridAutoOrTemplateBase,
  printGridCSSNumber,
} from '../../../components/inspector/common/css-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import { defaultEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  GridAutoOrTemplateDimensions,
} from '../../../core/shared/element-template'
import {
  isGridAutoOrTemplateDimensions,
  type GridAutoOrTemplateBase,
} from '../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import {
  canvasPoint,
  isFiniteRectangle,
  isInfinityRectangle,
  nullIfInfinity,
  pointsEqual,
  scaleRect,
  windowPoint,
  zeroRectangle,
  zeroRectIfNullOrInfinity,
} from '../../../core/shared/math-utils'
import {
  fromArrayIndex,
  fromField,
  fromTypeGuard,
  notNull,
} from '../../../core/shared/optics/optic-creators'
import { toFirst } from '../../../core/shared/optics/optic-utilities'
import type { Optic } from '../../../core/shared/optics/optics'
import { optionalMap } from '../../../core/shared/optional-utils'
import { assertNever } from '../../../core/shared/utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { useColorTheme, UtopiaStyles } from '../../../uuiui'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import CanvasActions from '../canvas-actions'
import type {
  ControlWithProps,
  WhenToShowControl,
} from '../canvas-strategies/canvas-strategy-types'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import type {
  GridResizeEdge,
  GridResizeEdgeProperties,
} from '../canvas-strategies/interaction-state'
import {
  createInteractionViaMouse,
  gridAxisHandle,
  gridCellHandle,
  gridResizeEdgeProperties,
  GridResizeEdges,
  gridResizeHandle,
} from '../canvas-strategies/interaction-state'
import { resizeBoundingBoxFromSide } from '../canvas-strategies/strategies/resize-helpers'
import type { EdgePosition } from '../canvas-types'
import {
  CSSCursor,
  EdgePositionBottom,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
} from '../canvas-types'
import { windowToCanvasCoordinates } from '../dom-lookup'
import type { Axis } from '../gap-utils'
import { useCanvasAnimation } from '../ui-jsx-canvas-renderer/animation-context'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { CanvasLabel } from './select-mode/controls-common'
import { useMaybeHighlightElement } from './select-mode/select-mode-hooks'
import type { GridCellCoordinates } from '../canvas-strategies/strategies/grid-cell-bounds'
import { gridCellTargetId } from '../canvas-strategies/strategies/grid-cell-bounds'
import {
  getGlobalFrameOfGridCell,
  getGridRelatedIndexes,
} from '../canvas-strategies/strategies/grid-helpers'
import { canResizeGridTemplate } from '../canvas-strategies/strategies/resize-grid-strategy'
import {
  GRID_RESIZE_HANDLE_CONTAINER_SIZE,
  GRID_RESIZE_HANDLE_SIZE,
  gridEdgeToCSSCursor,
  gridEdgeToEdgePosition,
  gridEdgeToWidthHeight,
  GridResizeEdgeTestId,
  GridResizingControl,
  useGridData,
} from './grid-controls'

export const GridControlsRuler = React.memo(() => {
  const selectedElement = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews?.[0],
    `GridControlsRuler selectedElement`,
  )
  if (selectedElement == null) {
    return null
  }
  return (
    <div>
      <div
        style={{
          position: 'absolute',
          top: 0,
          left: 0,
          width: '100%',
          height: 20,
          backgroundColor: 'white',
        }}
      ></div>
      <div
        style={{
          position: 'absolute',
          top: 0,
          left: 0,
          height: '100%',
          width: 20,
          backgroundColor: 'white',
        }}
      ></div>
      <GridRowColumnResizingControls.control target={selectedElement} />
    </div>
  )
})

interface GridRowColumnResizingControlsProps {
  target: ElementPath
}

export const GridRowColumnResizingControls =
  controlForStrategyMemoized<GridRowColumnResizingControlsProps>(({ target }) => {
    const grids = useGridData([target])

    function getStripedAreaLength(template: GridAutoOrTemplateBase | null, gap: number) {
      if (template?.type !== 'DIMENSIONS') {
        return null
      }
      return template.dimensions.reduce((acc, curr, index) => {
        if (curr.type === 'NUMBER') {
          return acc + curr.value.value + (index > 0 ? gap : 0)
        }
        return acc
      }, 0)
    }

    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'GridRowColumnResizingControls scale',
    )

    const gridsWithVisibleResizeControls = React.useMemo(() => {
      return grids.filter((grid) => {
        if (
          grid.gridTemplateColumns?.type !== 'DIMENSIONS' ||
          grid.gridTemplateRows?.type !== 'DIMENSIONS'
        ) {
          return false
        }

        // returns whether the rendered dimensions are too crowded, as in there are two cols/rows that are closer than the handle sizes
        function tooCrowded(dimensions: GridDimension[]): boolean {
          const visualSizes = dimensions.map(
            (dim) => (dim.type === 'NUMBER' ? dim.value.value : 0) * scale,
          )
          return visualSizes.some((dim, index) => {
            if (index < visualSizes.length - 1) {
              const next = visualSizes[index + 1]
              if (dim + next < GRID_RESIZE_HANDLE_SIZE * 2) {
                return true
              }
            }
            return false
          })
        }

        return (
          !tooCrowded(grid.gridTemplateColumns.dimensions) &&
          !tooCrowded(grid.gridTemplateRows.dimensions)
        )
      })
    }, [scale, grids])

    return (
      <React.Fragment>
        <CanvasOffsetWrapper limitAxis='x'>
          {gridsWithVisibleResizeControls.flatMap((grid) => {
            return (
              <GridResizing
                key={`grid-resizing-column-${EP.toString(grid.elementPath)}`}
                axisValues={grid.gridTemplateColumns}
                fromPropsAxisValues={grid.gridTemplateColumnsFromProps}
                containingFrame={grid.frame}
                axis={'column'}
                gap={grid.columnGap ?? grid.gap}
                padding={grid.padding}
                stripedAreaLength={getStripedAreaLength(grid.gridTemplateRows, grid.gap ?? 0)}
              />
            )
          })}
        </CanvasOffsetWrapper>
        <CanvasOffsetWrapper limitAxis='y'>
          {gridsWithVisibleResizeControls.flatMap((grid) => {
            return (
              <GridResizing
                key={`grid-resizing-row-${EP.toString(grid.elementPath)}`}
                axisValues={grid.gridTemplateRows}
                fromPropsAxisValues={grid.gridTemplateRowsFromProps}
                containingFrame={grid.frame}
                axis={'row'}
                gap={grid.rowGap ?? grid.gap}
                padding={grid.padding}
                stripedAreaLength={getStripedAreaLength(grid.gridTemplateColumns, grid.gap ?? 0)}
              />
            )
          })}
        </CanvasOffsetWrapper>
      </React.Fragment>
    )
  })

export interface GridResizingProps {
  axisValues: GridAutoOrTemplateBase | null
  fromPropsAxisValues: GridAutoOrTemplateBase | null
  stripedAreaLength: number | null
  containingFrame: CanvasRectangle
  axis: Axis
  gap: number | null
  padding: Sides | null
}

export const GridResizing = React.memo((props: GridResizingProps) => {
  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'GridResizing canvasScale',
  )

  const fromProps = React.useMemo((): GridAutoOrTemplateDimensions | null => {
    if (props.fromPropsAxisValues?.type !== 'DIMENSIONS') {
      return null
    }
    if (!canResizeGridTemplate(props.fromPropsAxisValues)) {
      return null
    }
    return {
      type: 'DIMENSIONS',
      dimensions: props.fromPropsAxisValues.dimensions.reduce(
        (acc, cur): GridDiscreteDimension[] => {
          if (isGridCSSRepeat(cur)) {
            if (isDynamicGridRepeat(cur)) {
              return acc
            }
            let expanded: GridDiscreteDimension[] = []
            for (let i = 0; i < cur.times; i++) {
              expanded.push(...cur.value.filter((v) => v.type !== 'REPEAT'))
            }
            return acc.concat(...expanded)
          } else {
            return acc.concat(cur)
          }
        },
        [] as GridDiscreteDimension[],
      ),
    }
  }, [props.fromPropsAxisValues])

  const resizeLocked = React.useMemo(() => {
    return fromProps == null || !canResizeGridTemplate(fromProps)
  }, [fromProps])

  const [resizingIndex, setResizingIndex] = React.useState<number | null>(null)

  // These are the indexes of the elements that will resize too alongside the one at the index of
  // `resizingIndex`.
  const coresizingIndexes: number[] = React.useMemo(() => {
    if (props.fromPropsAxisValues?.type !== 'DIMENSIONS' || resizingIndex == null) {
      return []
    }
    return getGridRelatedIndexes({
      template: props.fromPropsAxisValues.dimensions,
      index: resizingIndex,
    })
  }, [props.fromPropsAxisValues, resizingIndex])

  if (props.axisValues == null) {
    return null
  }
  switch (props.axisValues.type) {
    case 'DIMENSIONS':
      const size = GRID_RESIZE_HANDLE_CONTAINER_SIZE / canvasScale
      const dimensions = props.axisValues.dimensions

      return (
        <div
          id='outer-wrapper'
          style={{
            position: 'absolute',
            top: props.axis === 'column' ? 0 : props.containingFrame.y,
            left: props.axis === 'row' ? 0 : props.containingFrame.x,
            width: props.axis === 'column' ? props.containingFrame.width : size,
            height: props.axis === 'row' ? props.containingFrame.height : size,
            display: 'grid',
            gridTemplateColumns:
              props.axis === 'column'
                ? dimensions.map((dim) => printGridCSSNumber(dim)).join(' ')
                : undefined,
            gridTemplateRows:
              props.axis === 'row'
                ? dimensions.map((dim) => printGridCSSNumber(dim)).join(' ')
                : undefined,
            gap: props.gap ?? 0,
            paddingLeft:
              props.axis === 'column' && props.padding != null
                ? `${props.padding.left}px`
                : undefined,
            paddingTop:
              props.axis === 'row' && props.padding != null ? `${props.padding.top}px` : undefined,
          }}
        >
          {dimensions.flatMap((dimension, dimensionIndex) => {
            return (
              <GridResizingControl
                key={`grid-resizing-control-${dimensionIndex}`}
                dimensionIndex={dimensionIndex}
                dimension={dimension}
                fromPropsAxisValues={fromProps}
                stripedAreaLength={props.stripedAreaLength}
                axis={props.axis}
                containingFrame={props.containingFrame}
                resizing={
                  resizingIndex === dimensionIndex
                    ? 'resize-target'
                    : coresizingIndexes.includes(dimensionIndex)
                    ? 'resize-generated'
                    : 'not-resizing'
                }
                resizeLocked={resizeLocked}
                setResizingIndex={setResizingIndex}
                padding={
                  props.padding == null
                    ? 0
                    : props.axis === 'column'
                    ? props.padding.top ?? 0
                    : props.padding.left ?? 0
                }
              />
            )
          })}
        </div>
      )
    case 'FALLBACK':
      return null
    default:
      assertNever(props.axisValues)
  }
})
GridResizing.displayName = 'GridResizing'
