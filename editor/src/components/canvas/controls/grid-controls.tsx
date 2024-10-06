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
import { mapDropNulls, stripNulls } from '../../../core/shared/array-utils'
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
import { useRollYourOwnFeatures } from '../../navigator/left-pane/roll-your-own-pane'
import CanvasActions from '../canvas-actions'
import type { ControlWithProps } from '../canvas-strategies/canvas-strategy-types'
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

const CELL_ANIMATION_DURATION = 0.15 // seconds

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

function getFromPropsOptic(index: number): Optic<GridAutoOrTemplateBase | null, GridDimension> {
  return notNull<GridAutoOrTemplateBase>()
    .compose(fromTypeGuard(isGridAutoOrTemplateDimensions))
    .compose(fromField('dimensions'))
    .compose(fromArrayIndex(index))
}

function gridCSSNumberToLabel(gridCSSNumber: GridDimension): string {
  return printGridCSSNumber(gridCSSNumber)
}

function getLabelForAxis(
  fromDOM: GridDimension,
  index: number,
  fromProps: GridAutoOrTemplateDimensions | null,
): string {
  const fromPropsAtIndex = toFirst(getFromPropsOptic(index), fromProps)
  return gridCSSNumberToLabel(defaultEither(fromDOM, fromPropsAtIndex))
}

const GRID_RESIZE_HANDLE_CONTAINER_SIZE = 30 // px
const GRID_RESIZE_HANDLE_SIZE = 15 // px

export interface GridResizingControlProps {
  dimension: GridDimension
  dimensionIndex: number
  axis: Axis
  containingFrame: CanvasRectangle
  fromPropsAxisValues: GridAutoOrTemplateDimensions | null
  padding: number
  resizing: 'resize-target' | 'resize-generated' | 'not-resizing'
  setResizingIndex: (v: number | null) => void
  resizeLocked: boolean
  stripedAreaLength: number | null
}

export const GridResizingControl = React.memo((props: GridResizingControlProps) => {
  const { setResizingIndex } = props

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'GridResizingControl canvasOffset',
  )
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridResizingControl scale',
  )
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const mouseDownHandler = React.useCallback(
    (event: React.MouseEvent): void => {
      function mouseUpHandler() {
        setResizingIndex(null)
        window.removeEventListener('mouseup', mouseUpHandler)
      }
      window.addEventListener('mouseup', mouseUpHandler)

      const start = windowToCanvasCoordinates(
        scale,
        canvasOffset,
        windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
      )
      setResizingIndex(props.dimensionIndex)

      dispatch([
        CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            start.canvasPositionRounded,
            Modifier.modifiersForEvent(event),
            gridAxisHandle(props.axis, props.dimensionIndex),
            'zero-drag-not-permitted',
          ),
        ),
      ])
      event.stopPropagation()
      event.preventDefault()
    },
    [canvasOffset, dispatch, props.axis, props.dimensionIndex, scale, setResizingIndex],
  )

  const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

  const onMouseMove = React.useCallback(
    (e: React.MouseEvent) => {
      maybeClearHighlightsOnHoverEnd()
      e.stopPropagation()
    },
    [maybeClearHighlightsOnHoverEnd],
  )

  const labelId = `grid-${props.axis}-handle-${props.dimensionIndex}`
  const containerId = `${labelId}-container`

  const shadowSize = React.useMemo(() => {
    return props.axis === 'column'
      ? props.containingFrame.height + GRID_RESIZE_HANDLE_CONTAINER_SIZE
      : props.containingFrame.width + GRID_RESIZE_HANDLE_CONTAINER_SIZE
  }, [props.containingFrame, props.axis])

  const stripedAreaSkew = React.useMemo(
    () => GRID_RESIZE_HANDLE_CONTAINER_SIZE / scale + props.padding,
    [scale, props.padding],
  )

  return (
    <div
      key={containerId}
      data-testid={containerId}
      style={{
        display: 'flex',
        alignItems: props.axis === 'column' ? 'flex-start' : 'center',
        justifyContent: props.axis === 'column' ? 'center' : 'flex-start',
        height: props.axis === 'column' && props.resizing !== 'not-resizing' ? shadowSize : '100%',
        width: props.axis === 'row' && props.resizing !== 'not-resizing' ? shadowSize : '100%',
        position: 'relative',
      }}
    >
      <div
        data-testid={labelId}
        style={{
          zoom: 1 / scale,
          width: GRID_RESIZE_HANDLE_SIZE,
          height: GRID_RESIZE_HANDLE_SIZE,
          borderRadius: '100%',
          border: `1px solid ${colorTheme.border0.value}`,
          boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor50.value} 0px 0px
              1px, ${colorTheme.canvasControlsSizeBoxShadowColor20.value} 0px 1px 2px 2px`,
          background: colorTheme.white.value,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          cursor: gridEdgeToCSSCursor(props.axis === 'column' ? 'column-start' : 'row-start'),
          fontSize: 8,
          pointerEvents: 'initial',
        }}
        css={{
          opacity: props.resizing !== 'not-resizing' ? 1 : 0.5,
          ':hover': {
            opacity: 1,
          },
        }}
        onMouseDown={mouseDownHandler}
        onMouseMove={onMouseMove}
      >
        {props.axis === 'row' ? '↕' : '↔'}
        {when(
          props.dimension.areaName != null,
          <span style={{ position: 'absolute', top: 12 }}>{props.dimension.areaName}</span>,
        )}
      </div>
      {when(
        props.resizing !== 'not-resizing',
        <div
          style={{
            position: 'absolute',
            top: props.axis === 'column' ? stripedAreaSkew : 0,
            left: props.axis === 'row' ? stripedAreaSkew : 0,
            right: props.axis === 'row' || props.stripedAreaLength == null ? undefined : 0,
            width:
              props.axis === 'row' && props.stripedAreaLength != null
                ? props.stripedAreaLength
                : undefined,
            bottom: props.axis === 'column' || props.stripedAreaLength == null ? undefined : 0,
            height:
              props.axis === 'column' && props.stripedAreaLength != null
                ? props.stripedAreaLength
                : undefined,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            border: `1px solid ${
              props.resizeLocked
                ? colorTheme.brandNeonPink10.value
                : props.resizing === 'resize-target'
                ? colorTheme.brandNeonPink.value
                : colorTheme.brandNeonPink60.value
            }`,
            ...(props.resizeLocked
              ? UtopiaStyles.backgrounds.stripedBackground(colorTheme.brandNeonPink10.value, scale)
              : props.resizing === 'resize-target'
              ? UtopiaStyles.backgrounds.stripedBackground(colorTheme.brandNeonPink60.value, scale)
              : UtopiaStyles.backgrounds.stripedBackground(
                  colorTheme.brandNeonPink10.value,
                  scale,
                )),
          }}
        >
          <CanvasLabel
            value={getLabelForAxis(
              props.dimension,
              props.dimensionIndex,
              props.fromPropsAxisValues,
            )}
            scale={scale}
            color={
              props.resizeLocked
                ? colorTheme.brandNeonPink10.value
                : props.resizing === 'resize-target'
                ? colorTheme.brandNeonPink.value
                : colorTheme.brandNeonPink60.value
            }
            textColor={colorTheme.white.value}
          />
        </div>,
      )}
    </div>
  )
})
GridResizingControl.displayName = 'GridResizingControl'

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
          style={{
            position: 'absolute',
            top: props.containingFrame.y - (props.axis === 'column' ? size : 0),
            left: props.containingFrame.x - (props.axis === 'row' ? size : 0),
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
      <CanvasOffsetWrapper>
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
    )
  })

export const GridControlsKey = (gridPath: ElementPath) => `grid-controls-${EP.toString(gridPath)}`

export interface GridControlProps {
  grid: GridData
}

export const GridControl = React.memo<GridControlProps>(({ grid }) => {
  const dispatch = useDispatch()
  const controls = useAnimationControls()
  const colorTheme = useColorTheme()
  const features = useRollYourOwnFeatures()

  const editorMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'GridControl editorMetadata',
  )

  const interactionLatestMetadata = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG'
        ? store.editor.canvas.interactionSession.latestMetadata
        : null,
    'GridControl interactionLatestMetadata',
  )

  const jsxMetadata = React.useMemo(
    () => interactionLatestMetadata ?? editorMetadata,
    [interactionLatestMetadata, editorMetadata],
  )

  const activelyDraggingOrResizingCell = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE' &&
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG' &&
      store.editor.canvas.interactionSession?.interactionData.modifiers.cmd !== true &&
      store.editor.canvas.interactionSession?.interactionData.drag != null
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    'GridControl activelyDraggingOrResizingCell',
  )

  const currentHoveredCell = useEditorState(
    Substores.restOfStore,
    (store) =>
      store.strategyState.customStrategyState.grid.targetCellData?.gridCellCoordinates ?? null,
    'GridControl currentHoveredCell',
  )

  const targetsAreCellsWithPositioning = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.selectedViews.every((elementPath) =>
        MetadataUtils.isGridCellWithPositioning(store.editor.jsxMetadata, elementPath),
      ),
    'GridControl targetsAreCellsWithPositioning',
  )

  const anyTargetAbsolute = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.selectedViews.some((elementPath) =>
        MetadataUtils.isPositionAbsolute(
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath),
        ),
      ),
    'GridControl anyTargetAbsolute',
  )

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridControl scale',
  )

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

  const startInteractionWithUid = React.useCallback(
    (params: { uid: string; row: number; column: number; frame: CanvasRectangle }) =>
      (event: React.MouseEvent) => {
        setInitialShadowFrame(params.frame)

        const start = windowToCanvasCoordinates(
          scale,
          canvasOffsetRef.current,
          windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
        )

        dispatch([
          CanvasActions.createInteractionSession(
            createInteractionViaMouse(
              start.canvasPositionRounded,
              Modifier.modifiersForEvent(event),
              gridCellHandle({ id: params.uid }),
              'zero-drag-not-permitted',
            ),
          ),
        ])
      },
    [canvasOffsetRef, dispatch, scale],
  )

  const cells = React.useMemo(() => {
    const children = MetadataUtils.getChildrenUnordered(jsxMetadata, grid.elementPath)
    return mapDropNulls((cell, index) => {
      if (cell == null || cell.globalFrame == null || !isFiniteRectangle(cell.globalFrame)) {
        return null
      }
      const countedRow = Math.floor(index / grid.columns) + 1
      const countedColumn = Math.floor(index % grid.columns) + 1

      const columnFromProps = cell.specialSizeMeasurements.elementGridProperties.gridColumnStart
      const rowFromProps = cell.specialSizeMeasurements.elementGridProperties.gridRowStart
      return {
        elementPath: cell.elementPath,
        globalFrame: cell.globalFrame,
        borderRadius: cell.specialSizeMeasurements.borderRadius,
        column:
          columnFromProps == null
            ? countedColumn
            : isCSSKeyword(columnFromProps)
            ? countedColumn
            : columnFromProps.numericalPosition ?? countedColumn,
        row:
          rowFromProps == null
            ? countedRow
            : isCSSKeyword(rowFromProps)
            ? countedRow
            : rowFromProps.numericalPosition ?? countedRow,
        index: index,
      }
    }, children)
  }, [grid, jsxMetadata])

  const dragging = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE'
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    'GridControl dragging',
  )

  const shadow = React.useMemo(() => {
    return cells.find((cell) => EP.toUid(cell.elementPath) === dragging) ?? null
  }, [cells, dragging])

  const [initialShadowFrame, setInitialShadowFrame] = React.useState<CanvasRectangle | null>(
    shadow?.globalFrame ?? null,
  )

  const interactionData = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG'
        ? store.editor.canvas.interactionSession.interactionData
        : null,
    'GridControl interactionData',
  )

  const { hoveringStart, mouseCanvasPosition } = useMouseMove(activelyDraggingOrResizingCell)

  // NOTE: this stuff is meant to be temporary, until we settle on the set of interaction pieces we like.
  // After that, we should get rid of this.
  const shadowPosition = React.useMemo(() => {
    const drag = interactionData?.drag
    const dragStart = interactionData?.dragStart
    if (
      initialShadowFrame == null ||
      interactionData == null ||
      drag == null ||
      dragStart == null ||
      hoveringStart == null ||
      shadow == null
    ) {
      return null
    }

    const getCoord = (axis: 'x' | 'y', dimension: 'width' | 'height') => {
      if (features.Grid.dragVerbatim) {
        return initialShadowFrame[axis] + drag[axis]
      } else if (features.Grid.dragMagnetic) {
        return shadow.globalFrame[axis] + (mouseCanvasPosition[axis] - hoveringStart.point[axis])
      } else if (features.Grid.dragRatio) {
        return (
          shadow.globalFrame[axis] +
          drag[axis] -
          (shadow.globalFrame[axis] - dragStart[axis]) -
          shadow.globalFrame[dimension] *
            ((dragStart[axis] - initialShadowFrame[axis]) / initialShadowFrame[dimension])
        )
      } else {
        return undefined
      }
    }

    // make sure the shadow is displayed only inside the grid container bounds
    function wrapCoord(c: number, min: number, max: number, shadowSize: number) {
      return Math.min(Math.max(c, min), max - shadowSize)
    }

    return {
      x: wrapCoord(
        getCoord('x', 'width') ?? 0,
        grid.frame.x,
        grid.frame.x + grid.frame.width,
        shadow.globalFrame.width,
      ),
      y: wrapCoord(
        getCoord('y', 'height') ?? 0,
        grid.frame.y,
        grid.frame.y + grid.frame.height,
        shadow.globalFrame.height,
      ),
    }
  }, [
    interactionData,
    initialShadowFrame,
    hoveringStart,
    shadow,
    grid.frame.x,
    grid.frame.width,
    grid.frame.y,
    grid.frame.height,
    features.Grid.dragVerbatim,
    features.Grid.dragMagnetic,
    features.Grid.dragRatio,
    mouseCanvasPosition,
  ])

  const gridPath = optionalMap(EP.parentPath, shadow?.elementPath)

  const targetRootCell = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState.customStrategyState.grid.currentRootCell,
    'GridControl targetRootCell',
  )

  useCellAnimation({
    disabled: anyTargetAbsolute,
    targetRootCell: targetRootCell,
    controls: controls,
    shadowFrame: initialShadowFrame,
    gridPath: gridPath,
  })

  const placeholders = Array.from(Array(grid.cells).keys())
  let style: CSSProperties = {
    position: 'absolute',
    top: grid.frame.y - 1, // account for border!
    left: grid.frame.x - 1, // account for border!
    width: grid.frame.width,
    height: grid.frame.height,
    display: 'grid',
    gridTemplateColumns: getNullableAutoOrTemplateBaseString(grid.gridTemplateColumns),
    gridTemplateRows: getNullableAutoOrTemplateBaseString(grid.gridTemplateRows),
    backgroundColor:
      activelyDraggingOrResizingCell != null ? features.Grid.activeGridBackground : 'transparent',
    border: `1px solid ${
      activelyDraggingOrResizingCell != null ? colorTheme.primary.value : 'transparent'
    }`,
    justifyContent: grid.justifyContent ?? 'initial',
    alignContent: grid.alignContent ?? 'initial',
    pointerEvents: 'none',
    padding:
      grid.padding == null
        ? 0
        : `${grid.padding.top}px ${grid.padding.right}px ${grid.padding.bottom}px ${grid.padding.left}px`,
  }

  // Gap needs to be set only if the other two are not present or we'll have rendering issues
  // due to how measurements are calculated.
  if (grid.rowGap != null && grid.columnGap != null) {
    style.rowGap = grid.rowGap
    style.columnGap = grid.columnGap
  } else {
    if (grid.gap != null) {
      style.gap = grid.gap
    }
    if (grid.rowGap != null) {
      style.rowGap = grid.rowGap
    }
    if (grid.columnGap != null) {
      style.columnGap = grid.columnGap
    }
  }

  return (
    <React.Fragment>
      {/* grid lines */}
      <div
        key={gridKeyFromPath(grid.elementPath)}
        id={gridKeyFromPath(grid.elementPath)}
        data-grid-path={EP.toString(grid.elementPath)}
        style={style}
      >
        {placeholders.map((cell) => {
          const countedRow = Math.floor(cell / grid.columns) + 1
          const countedColumn = Math.floor(cell % grid.columns) + 1
          const id = gridCellTargetId(grid.elementPath, countedRow, countedColumn)
          const borderID = `${id}-border`
          const dotgridColor =
            activelyDraggingOrResizingCell != null ? features.Grid.dotgridColor : 'transparent'

          const isActiveCell =
            countedColumn === currentHoveredCell?.column && countedRow === currentHoveredCell?.row

          const borderColor =
            isActiveCell && targetsAreCellsWithPositioning
              ? colorTheme.brandNeonPink.value
              : features.Grid.inactiveGridColor
          return (
            <div
              key={id}
              id={id}
              data-testid={id}
              data-wtf={`data-wtf`}
              style={{
                position: 'relative',
                pointerEvents: 'initial',
              }}
              data-grid-row={countedRow}
              data-grid-column={countedColumn}
            >
              <React.Fragment>
                <div
                  key={borderID}
                  id={borderID}
                  data-testid={borderID}
                  style={{
                    position: 'relative',
                    left: gridPlaceholderTopOrLeftPosition(scale),
                    top: gridPlaceholderTopOrLeftPosition(scale),
                    width: gridPlaceholderWidthOrHeight(scale),
                    height: gridPlaceholderWidthOrHeight(scale),
                    borderTop: gridPlaceholderBorder(borderColor, scale),
                    borderLeft: gridPlaceholderBorder(borderColor, scale),
                    borderBottom:
                      isActiveCell ||
                      countedRow >= grid.rows ||
                      (grid.rowGap != null && grid.rowGap > 0)
                        ? gridPlaceholderBorder(borderColor, scale)
                        : undefined,
                    borderRight:
                      isActiveCell ||
                      countedColumn >= grid.columns ||
                      (grid.columnGap != null && grid.columnGap > 0)
                        ? gridPlaceholderBorder(borderColor, scale)
                        : undefined,
                  }}
                />
                {when(
                  features.Grid.dotgrid,
                  <React.Fragment>
                    <div
                      style={{
                        position: 'absolute',
                        top: -1,
                        bottom: -1,
                        left: -1,
                        right: -1,
                        display: 'flex',
                        alignItems: 'center',
                        justifyContent: 'center',
                      }}
                    >
                      <div style={{ width: 2, height: 2, backgroundColor: dotgridColor }} />
                    </div>
                    <div
                      style={{
                        width: 2,
                        height: 2,
                        backgroundColor: dotgridColor,
                        position: 'absolute',
                        top: -1,
                        left: -1,
                      }}
                    />
                    <div
                      style={{
                        width: 2,
                        height: 2,
                        backgroundColor: dotgridColor,
                        position: 'absolute',
                        bottom: -1,
                        left: -1,
                      }}
                    />
                    <div
                      style={{
                        width: 2,
                        height: 2,
                        backgroundColor: dotgridColor,
                        position: 'absolute',
                        top: -1,
                        right: -1,
                      }}
                    />
                    <div
                      style={{
                        width: 2,
                        height: 2,
                        backgroundColor: dotgridColor,
                        position: 'absolute',
                        bottom: -1,
                        right: -1,
                      }}
                    />
                  </React.Fragment>,
                )}
              </React.Fragment>
            </div>
          )
        })}
      </div>
      {/* cell targets */}
      {cells.map((cell) => {
        return (
          <div
            onMouseDown={startInteractionWithUid({
              uid: EP.toUid(cell.elementPath),
              frame: cell.globalFrame,
              row: cell.row,
              column: cell.column,
            })}
            key={GridCellTestId(cell.elementPath)}
            data-testid={GridCellTestId(cell.elementPath)}
            style={{
              position: 'absolute',
              top: cell.globalFrame.y,
              left: cell.globalFrame.x,
              width: cell.globalFrame.width,
              height: cell.globalFrame.height,
              display: 'flex',
              justifyContent: 'flex-end',
              alignItems: 'flex-end',
              backgroundColor:
                activelyDraggingOrResizingCell != null &&
                EP.toUid(cell.elementPath) !== activelyDraggingOrResizingCell
                  ? '#ffffff66'
                  : 'transparent',
              borderRadius:
                cell.borderRadius != null
                  ? `${cell.borderRadius.top}px ${cell.borderRadius.right}px ${cell.borderRadius.bottom}px ${cell.borderRadius.left}px`
                  : 0,
            }}
          />
        )
      })}
      {/* shadow */}
      {features.Grid.shadow &&
      !anyTargetAbsolute &&
      shadow != null &&
      initialShadowFrame != null &&
      interactionData?.dragStart != null &&
      interactionData?.drag != null &&
      hoveringStart != null ? (
        <motion.div
          style={{
            pointerEvents: 'none',
            position: 'absolute',
            width: shadow.globalFrame.width,
            height: shadow.globalFrame.height,
            borderRadius:
              shadow.borderRadius != null
                ? `${shadow.borderRadius.top}px ${shadow.borderRadius.right}px ${shadow.borderRadius.bottom}px ${shadow.borderRadius.left}px`
                : 0,
            backgroundColor: 'black',
            opacity: features.Grid.shadowOpacity,
            border: '1px solid white',
            top: shadowPosition?.y,
            left: shadowPosition?.x,
          }}
        />
      ) : null}
    </React.Fragment>
  )
})
GridControl.displayName = 'GridControl'

export interface GridControlsProps {
  targets: ElementPath[]
}

export const GridControls = controlForStrategyMemoized<GridControlsProps>(({ targets }) => {
  const targetRootCell = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState.customStrategyState.grid.currentRootCell,
    'GridControls targetRootCell',
  )

  const hoveredGrids = useEditorState(
    Substores.canvas,
    (store) => stripNulls([store.editor.canvas.controls.gridControls]),
    'GridControls hoveredGrids',
  )

  const grids = useGridData([...targets, ...hoveredGrids])

  if (grids.length === 0) {
    return null
  }

  return (
    <div id={'grid-controls'}>
      <CanvasOffsetWrapper>
        {grids.map((grid) => {
          return <GridControl key={`grid-control-${EP.toString(grid.elementPath)}`} grid={grid} />
        })}
        <AbsoluteDistanceIndicators targetRootCell={targetRootCell} />
      </CanvasOffsetWrapper>
    </div>
  )
})

const MIN_INDICATORS_DISTANCE = 32 // px

const AbsoluteDistanceIndicators = React.memo(
  (props: { targetRootCell: GridCellCoordinates | null }) => {
    const colorTheme = useColorTheme()

    const gridMetadata = useEditorState(
      Substores.metadata,
      (store) => {
        if (store.editor.selectedViews.length !== 1) {
          return null
        }

        return MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          store.editor.selectedViews[0],
        )
      },
      'AbsoluteDistanceIndicators cellFrame',
    )

    const cellFrame = !MetadataUtils.isPositionAbsolute(gridMetadata)
      ? null
      : nullIfInfinity(gridMetadata?.globalFrame)

    const targetCellBoundingBox = React.useMemo(() => {
      if (gridMetadata == null || props.targetRootCell == null) {
        return null
      }
      return getGlobalFrameOfGridCell(gridMetadata, props.targetRootCell)
    }, [props.targetRootCell, gridMetadata])

    const distanceTop =
      targetCellBoundingBox == null || cellFrame == null ? 0 : cellFrame.y - targetCellBoundingBox.y

    const distanceLeft =
      targetCellBoundingBox == null || cellFrame == null ? 0 : cellFrame.x - targetCellBoundingBox.x

    const positioning = React.useMemo(() => {
      if (cellFrame == null || targetCellBoundingBox == null) {
        return null
      }

      function position(
        wantedCoord: 'x' | 'y',
        cell: CanvasRectangle,
        root: CanvasRectangle,
        dominantDistance: number,
        otherDistance: number,
      ): { left: number; top: number } {
        const otherCoord = wantedCoord === 'x' ? 'y' : 'x'
        const dimension = wantedCoord === 'x' ? 'width' : 'height'
        const dominant =
          cell[wantedCoord] < root[wantedCoord] ||
          dominantDistance < MIN_INDICATORS_DISTANCE ||
          otherDistance < 0
            ? root[wantedCoord] + root[dimension] / 2
            : Math.max(root[wantedCoord], cell[wantedCoord])
        const other = otherDistance < 0 ? cell[otherCoord] : root[otherCoord]
        if (wantedCoord === 'x') {
          return {
            left: dominant,
            top: other,
          }
        } else {
          return {
            left: other,
            top: dominant,
          }
        }
      }

      function compensationNegative(
        wantedCoord: 'x' | 'y',
        cell: CanvasRectangle,
        root: CanvasRectangle,
        dist: number,
      ): { width: number; height: number; left: number; top: number } | null {
        const otherCoord = wantedCoord === 'x' ? 'y' : 'x'
        const dimension = wantedCoord === 'x' ? 'width' : 'height'

        const shouldCompensate =
          dist < 0 && cell[wantedCoord] > root[wantedCoord] + root[dimension] / 2
        if (!shouldCompensate) {
          return null
        }

        const size = Math.abs(root[wantedCoord] + root[dimension] / 2 - cell[wantedCoord])
        const dominant = root[wantedCoord] + root[dimension] / 2
        const other = cell[otherCoord]

        return wantedCoord === 'x'
          ? {
              width: size,
              height: 1,
              top: other,
              left: dominant,
            }
          : {
              width: 1,
              height: size,
              top: dominant,
              left: other,
            }
      }

      function compensationPositive(
        wantedCoord: 'x' | 'y',
        cell: CanvasRectangle,
        root: CanvasRectangle,
        dist: number,
      ): { width: number; height: number; left: number; top: number } | null {
        const otherCoord = wantedCoord === 'x' ? 'y' : 'x'
        const dimension = wantedCoord === 'x' ? 'width' : 'height'

        const shouldCompensate = dist > 0 && cell[wantedCoord] > root[wantedCoord] + root[dimension]
        if (!shouldCompensate) {
          return null
        }

        const size = Math.abs(root[wantedCoord] + root[dimension] / 2 - cell[wantedCoord])
        const other = root[otherCoord]
        const dominant = root[wantedCoord] + root[dimension] / 2

        return wantedCoord === 'x'
          ? {
              width: size,
              height: 1,
              top: other,
              left: dominant,
            }
          : {
              height: size,
              width: 1,
              left: other,
              top: dominant,
            }
      }

      const topIndicator = {
        ...position('x', cellFrame, targetCellBoundingBox, distanceLeft, distanceTop),
        compensateNegative: compensationNegative(
          'x',
          cellFrame,
          targetCellBoundingBox,
          distanceTop,
        ),
        compensatePositive: compensationPositive(
          'x',
          cellFrame,
          targetCellBoundingBox,
          distanceTop,
        ),
      }

      const leftIndicator = {
        ...position('y', cellFrame, targetCellBoundingBox, distanceLeft, distanceLeft),
        compensateNegative: compensationNegative(
          'y',
          cellFrame,
          targetCellBoundingBox,
          distanceLeft,
        ),
        compensatePositive: compensationPositive(
          'y',
          cellFrame,
          targetCellBoundingBox,
          distanceLeft,
        ),
      }

      return { topIndicator, leftIndicator }
    }, [cellFrame, targetCellBoundingBox, distanceLeft, distanceTop])

    if (targetCellBoundingBox == null || cellFrame == null || positioning == null) {
      return null
    }

    const backgroundColor = colorTheme.brandNeonPink.value
    const dashedBorder = `1px dashed ${backgroundColor}`

    return (
      <React.Fragment>
        {/* top distance */}
        <React.Fragment>
          <div
            style={{
              position: 'absolute',
              borderLeft: dashedBorder,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',

              left: positioning.topIndicator.left,
              top: positioning.topIndicator.top,
              width: 1,
              height: Math.abs(distanceTop),
            }}
          >
            <span
              style={{
                backgroundColor: backgroundColor,
                padding: '0px 2px',
                borderRadius: 2,
                fontSize: 9,
                color: '#fff',
              }}
            >
              {distanceTop}
            </span>
          </div>
          {/* compensate */}
          {positioning.topIndicator.compensateNegative != null ? (
            <div
              style={{
                position: 'absolute',
                borderTop: dashedBorder,

                left: positioning.topIndicator.compensateNegative.left,
                top: positioning.topIndicator.compensateNegative.top,
                width: positioning.topIndicator.compensateNegative.width,
                height: positioning.topIndicator.compensateNegative.height,
              }}
            />
          ) : null}
          {positioning.topIndicator.compensatePositive != null ? (
            <div
              style={{
                position: 'absolute',
                borderTop: dashedBorder,

                left: positioning.topIndicator.compensatePositive.left,
                top: positioning.topIndicator.compensatePositive.top,
                width: positioning.topIndicator.compensatePositive.width,
                height: positioning.topIndicator.compensatePositive.height,
              }}
            />
          ) : null}
        </React.Fragment>

        {/* left distance */}
        <React.Fragment>
          <div
            style={{
              position: 'absolute',
              borderTop: dashedBorder,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',

              left: positioning.leftIndicator.left,
              top: positioning.leftIndicator.top,
              width: Math.abs(distanceLeft),
              height: 1,
            }}
          >
            <span
              style={{
                backgroundColor: backgroundColor,
                padding: '0px 2px',
                borderRadius: 2,
                fontSize: 9,
                color: '#fff',
              }}
            >
              {distanceLeft}
            </span>
          </div>
          {/* compensate */}
          {positioning.leftIndicator.compensateNegative != null ? (
            <div
              style={{
                position: 'absolute',
                borderLeft: dashedBorder,

                left: positioning.leftIndicator.compensateNegative.left,
                top: positioning.leftIndicator.compensateNegative.top,
                width: positioning.leftIndicator.compensateNegative.width,
                height: positioning.leftIndicator.compensateNegative.height,
              }}
            />
          ) : null}
          {positioning.leftIndicator.compensatePositive != null ? (
            <div
              style={{
                position: 'absolute',
                borderLeft: dashedBorder,

                left: positioning.leftIndicator.compensatePositive.left,
                top: positioning.leftIndicator.compensatePositive.top,
                width: positioning.leftIndicator.compensatePositive.width,
                height: positioning.leftIndicator.compensatePositive.height,
              }}
            />
          ) : null}
        </React.Fragment>
      </React.Fragment>
    )
  },
)

function useCellAnimation(params: {
  disabled: boolean
  gridPath: ElementPath | null
  shadowFrame: CanvasRectangle | null
  targetRootCell: GridCellCoordinates | null
  controls: AnimationControls
}) {
  const { gridPath, targetRootCell, controls, shadowFrame, disabled } = params

  const [lastTargetRootCellId, setLastTargetRootCellId] = React.useState(targetRootCell)
  const [lastSnapPoint, setLastSnapPoint] = React.useState<CanvasPoint | null>(shadowFrame)

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'useSnapAnimation selectedViews',
  )

  const animate = useCanvasAnimation(selectedViews)

  const gridMetadata = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, gridPath),
    'useCellAnimation gridMetadata',
  )

  const moveFromPoint = React.useMemo(() => {
    return lastSnapPoint ?? shadowFrame
  }, [lastSnapPoint, shadowFrame])

  const snapPoint = React.useMemo(() => {
    if (gridMetadata == null || targetRootCell == null) {
      return null
    }

    return getGlobalFrameOfGridCell(gridMetadata, targetRootCell)
  }, [gridMetadata, targetRootCell])

  React.useEffect(() => {
    if (disabled) {
      return
    }

    if (targetRootCell != null && snapPoint != null && moveFromPoint != null) {
      const snapPointsDiffer = lastSnapPoint == null || !pointsEqual(snapPoint, lastSnapPoint)
      const hasMovedToANewCell = lastTargetRootCellId != null
      const shouldAnimate = snapPointsDiffer && hasMovedToANewCell
      if (shouldAnimate) {
        void animate(
          {
            scale: [0.97, 1.02, 1], // a very subtle boop
            x: [moveFromPoint.x - snapPoint.x, 0],
            y: [moveFromPoint.y - snapPoint.y, 0],
          },
          {
            duration: CELL_ANIMATION_DURATION,
            type: 'tween',
            ease: 'easeInOut',
          },
        )
      }
    }
    setLastSnapPoint(snapPoint)
    setLastTargetRootCellId(targetRootCell)
  }, [
    targetRootCell,
    controls,
    lastSnapPoint,
    snapPoint,
    animate,
    moveFromPoint,
    lastTargetRootCellId,
    disabled,
  ])
}

function useMouseMove(activelyDraggingOrResizingCell: string | null) {
  const [hoveringStart, setHoveringStart] = React.useState<{
    point: CanvasPoint
  } | null>(null)
  const [mouseCanvasPosition, setMouseCanvasPosition] = React.useState<CanvasPoint>(
    canvasPoint({ x: 0, y: 0 }),
  )

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'useHoveringCell canvasScale',
  )

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'useHoveringCell canvasOffset',
  )

  React.useEffect(() => {
    function handleMouseMove(e: MouseEvent) {
      if (activelyDraggingOrResizingCell == null) {
        setHoveringStart(null)
        return
      }

      const newMouseCanvasPosition = windowToCanvasCoordinates(
        canvasScale,
        canvasOffset,
        windowPoint({ x: e.clientX, y: e.clientY }),
      ).canvasPositionRaw
      setMouseCanvasPosition(newMouseCanvasPosition)

      setHoveringStart((start) => {
        if (start == null) {
          return {
            point: canvasPoint(newMouseCanvasPosition),
          }
        }
        return start
      })
    }
    window.addEventListener('mousemove', handleMouseMove)
    return function () {
      window.removeEventListener('mousemove', handleMouseMove)
    }
  }, [activelyDraggingOrResizingCell, canvasOffset, canvasScale])

  return { hoveringStart, mouseCanvasPosition }
}

export const GridResizeEdgeTestId = (edge: GridResizeEdge) => `grid-resize-edge-${edge}`

interface GridResizeControlProps {
  target: ElementPath
}

export const GridResizeControls = controlForStrategyMemoized<GridResizeControlProps>(
  ({ target }) => {
    const colorTheme = useColorTheme()

    const element = useEditorState(
      Substores.metadata,
      (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, target),
      'GridResizeControls element',
    )

    const dispatch = useDispatch()
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'GridResizeControls scale',
    )

    const resizeControlRef = useRefEditorState((store) =>
      store.editor.canvas.interactionSession?.activeControl.type !== 'GRID_RESIZE_HANDLE'
        ? null
        : store.editor.canvas.interactionSession.activeControl,
    )

    const dragRef = useRefEditorState((store) =>
      store.editor.canvas.interactionSession?.interactionData.type !== 'DRAG'
        ? null
        : store.editor.canvas.interactionSession?.interactionData.drag,
    )

    const [startingBounds, setStartingBounds] = React.useState<CanvasRectangle | null>(null)
    const [bounds, setBounds] = React.useState<CanvasRectangle | null>(null)
    const onMouseMove = React.useCallback(() => {
      if (resizeControlRef.current == null || dragRef.current == null) {
        return
      }

      if (startingBounds == null) {
        return
      }
      setBounds(
        resizeBoundingBoxFromSide(
          startingBounds,
          dragRef.current,
          gridEdgeToEdgePosition(resizeControlRef.current.edge),
          'non-center-based',
          null,
        ),
      )
    }, [dragRef, resizeControlRef, startingBounds])

    const isResizing = bounds != null

    const [resizingEdge, setResizingEdge] = React.useState<GridResizeEdge | null>(null)

    const onMouseUp = React.useCallback(() => {
      setBounds(null)
      setStartingBounds(null)
      setResizingEdge(null)
    }, [])

    React.useEffect(() => {
      window.addEventListener('mousemove', onMouseMove)
      window.addEventListener('mouseup', onMouseUp)
      return () => {
        window.removeEventListener('mousemove', onMouseMove)
        window.removeEventListener('mouseup', onMouseUp)
      }
    }, [onMouseMove, onMouseUp])

    const startResizeInteraction = React.useCallback(
      (uid: string, edge: GridResizeEdge) => (event: React.MouseEvent) => {
        event.stopPropagation()
        const frame = zeroRectIfNullOrInfinity(element?.globalFrame ?? null)
        setResizingEdge(edge)
        setBounds(frame)
        setStartingBounds(frame)
        const start = windowToCanvasCoordinates(
          scale,
          canvasOffsetRef.current,
          windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
        )
        dispatch([
          CanvasActions.createInteractionSession(
            createInteractionViaMouse(
              start.canvasPositionRounded,
              Modifier.modifiersForEvent(event),
              gridResizeHandle(uid, edge),
              'zero-drag-not-permitted',
            ),
          ),
        ])
      },
      [canvasOffsetRef, dispatch, element?.globalFrame, scale],
    )

    const canShowHandles = React.useMemo(() => {
      if (isResizing) {
        return true
      }
      if (element?.globalFrame == null || isInfinityRectangle(element.globalFrame)) {
        return false
      }
      const scaledFrame = scaleRect(element.globalFrame, scale)
      return scaledFrame.width * scale > 30 && scaledFrame.height > 30
    }, [element, scale, isResizing])

    if (
      element == null ||
      element.globalFrame == null ||
      isInfinityRectangle(element.globalFrame) ||
      !canShowHandles
    ) {
      return null
    }

    return (
      <CanvasOffsetWrapper>
        <div
          data-testid={`grid-resize-container-${EP.toString(element.elementPath)}`}
          key={`grid-resize-container-${EP.toString(element.elementPath)}`}
          style={{
            pointerEvents: 'none',
            position: 'absolute',
            top: bounds?.y ?? element.globalFrame.y,
            left: bounds?.x ?? element.globalFrame.x,
            width: bounds?.width ?? element.globalFrame.width,
            height: bounds?.height ?? element.globalFrame.height,
            backgroundColor: isResizing ? colorTheme.primary25.value : 'transparent',
          }}
        >
          <div
            style={{
              position: 'relative',
              width: '100%',
              height: '100%',
              pointerEvents: 'none',
            }}
          >
            {GridResizeEdges.map((edge) => {
              const properties = gridResizeEdgeProperties(edge)
              const visible = !isResizing || resizingEdge === edge
              return (
                <div
                  key={edge}
                  style={{
                    visibility: visible ? 'visible' : 'hidden',
                    position: 'absolute',
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'center',
                    pointerEvents: 'none',
                    ...gridEdgeToWidthHeight(properties, scale),
                  }}
                >
                  <div
                    data-testid={GridResizeEdgeTestId(edge)}
                    onMouseDown={startResizeInteraction(EP.toUid(element.elementPath), edge)}
                    style={{
                      width: properties.isRow
                        ? GRID_RESIZE_HANDLE_SIZES.long
                        : GRID_RESIZE_HANDLE_SIZES.short,
                      height: properties.isColumn
                        ? GRID_RESIZE_HANDLE_SIZES.long
                        : GRID_RESIZE_HANDLE_SIZES.short,
                      borderRadius: 4,
                      cursor: gridEdgeToCSSCursor(edge),
                      pointerEvents: 'initial',
                      backgroundColor: colorTheme.white.value,
                      boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor50.value} 0px 0px
                        ${1 / scale}px, ${
                        colorTheme.canvasControlsSizeBoxShadowColor20.value
                      } 0px ${1 / scale}px ${2 / scale}px ${1 / scale}px`,
                      zoom: 1 / scale,
                    }}
                  />
                </div>
              )
            })}
          </div>
        </div>
      </CanvasOffsetWrapper>
    )
  },
)

const GRID_RESIZE_HANDLE_SIZES = {
  long: 24,
  short: 4,
}

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

function gridEdgeToCSSCursor(edge: GridResizeEdge): CSSCursor {
  switch (edge) {
    case 'column-end':
    case 'column-start':
      return CSSCursor.ColResize
    case 'row-end':
    case 'row-start':
      return CSSCursor.RowResize
    default:
      assertNever(edge)
  }
}

function gridEdgeToWidthHeight(props: GridResizeEdgeProperties, scale: number): CSSProperties {
  return {
    width: props.isColumn ? (GRID_RESIZE_HANDLE_SIZES.short * 4) / scale : '100%',
    height: props.isRow ? (GRID_RESIZE_HANDLE_SIZES.short * 4) / scale : '100%',
    top: props.isStart ? 0 : undefined,
    left: props.isStart ? 0 : undefined,
    right: props.isEnd ? 0 : undefined,
    bottom: props.isEnd ? 0 : undefined,
  }
}

function gridKeyFromPath(path: ElementPath): string {
  return `grid-${EP.toString(path)}`
}

const placeholderBorderBaseWidth = 2

function gridPlaceholderBorder(color: string, scale: number): string {
  return `${placeholderBorderBaseWidth / scale}px solid ${color}`
}

function gridPlaceholderTopOrLeftPosition(scale: number): string {
  return `${-placeholderBorderBaseWidth / scale}px`
}

function gridPlaceholderWidthOrHeight(scale: number): string {
  return `calc(100% + ${(placeholderBorderBaseWidth * 2) / scale}px)`
}

export function controlsForGridPlaceholders(gridPath: ElementPath): ControlWithProps<any> {
  return {
    control: GridControls,
    props: { targets: [gridPath] },
    key: GridControlsKey(gridPath),
    show: 'always-visible',
    priority: 'bottom',
  }
}
