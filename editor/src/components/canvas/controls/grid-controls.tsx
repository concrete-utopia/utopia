/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import type { AnimationControls } from 'framer-motion'
import { motion, useAnimationControls } from 'framer-motion'
import type { CSSProperties } from 'react'
import React from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import type { GridCSSNumber } from '../../../components/inspector/common/css-utils'
import { printGridAutoOrTemplateBase } from '../../../components/inspector/common/css-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { defaultEither } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  isGridAutoOrTemplateDimensions,
  type GridAutoOrTemplateBase,
} from '../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import {
  canvasPoint,
  distance,
  getRectCenter,
  isFiniteRectangle,
  isInfinityRectangle,
  offsetPoint,
  pointDifference,
  pointsEqual,
  scaleRect,
  windowPoint,
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
import { assertNever } from '../../../core/shared/utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { useRollYourOwnFeatures } from '../../navigator/left-pane/roll-your-own-pane'
import CanvasActions from '../canvas-actions'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import type {
  GridResizeEdge,
  GridResizeEdgeProperties,
} from '../canvas-strategies/interaction-state'
import {
  GridResizeEdges,
  createInteractionViaMouse,
  gridAxisHandle,
  gridCellHandle,
  gridResizeEdgeProperties,
  gridResizeHandle,
} from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { useColorTheme, UtopiaStyles } from '../../../uuiui'
import { gridCellTargetId } from '../canvas-strategies/strategies/grid-helpers'
import { resizeBoundingBoxFromSide } from '../canvas-strategies/strategies/resize-helpers'
import type { EdgePosition } from '../canvas-types'
import {
  CSSCursor,
  EdgePositionBottom,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
} from '../canvas-types'
import { useCanvasAnimation } from '../ui-jsx-canvas-renderer/animation-context'
import { CanvasLabel } from './select-mode/controls-common'
import { optionalMap } from '../../../core/shared/optional-utils'
import type { Sides } from 'utopia-api/core'

const CELL_ANIMATION_DURATION = 0.15 // seconds

export const GridCellTestId = (elementPath: ElementPath) => `grid-cell-${EP.toString(elementPath)}`

export type GridCellCoordinates = { row: number; column: number }

export function gridCellCoordinates(row: number, column: number): GridCellCoordinates {
  return { row: row, column: column }
}

function getCellsCount(template: GridAutoOrTemplateBase | null): number {
  if (template == null) {
    return 0
  }

  switch (template.type) {
    case 'DIMENSIONS':
      return template.dimensions.length
    case 'FALLBACK':
      return 0
    default:
      assertNever(template)
  }
}

function getNullableAutoOrTemplateBaseString(
  template: GridAutoOrTemplateBase | null,
): string | undefined {
  if (template == null) {
    return undefined
  } else {
    return printGridAutoOrTemplateBase(template)
  }
}

function getFromPropsOptic(index: number): Optic<GridAutoOrTemplateBase | null, GridCSSNumber> {
  return notNull<GridAutoOrTemplateBase>()
    .compose(fromTypeGuard(isGridAutoOrTemplateDimensions))
    .compose(fromField('dimensions'))
    .compose(fromArrayIndex(index))
}

function gridCSSNumberToLabel(gridCSSNumber: GridCSSNumber): string {
  return `${gridCSSNumber.value}${gridCSSNumber.unit ?? ''}`
}

function getLabelForAxis(
  fromDOM: GridCSSNumber,
  index: number,
  fromProps: GridAutoOrTemplateBase | null,
): string {
  const fromPropsAtIndex = toFirst(getFromPropsOptic(index), fromProps)
  return gridCSSNumberToLabel(defaultEither(fromDOM, fromPropsAtIndex))
}

const SHADOW_SNAP_ANIMATION = 'shadow-snap'
const GRID_RESIZE_HANDLE_CONTAINER_SIZE = 30 // px
const GRID_RESIZE_HANDLE_SIZE = 15 // px

export interface GridResizingControlProps {
  dimension: GridCSSNumber
  dimensionIndex: number
  axis: 'row' | 'column'
  containingFrame: CanvasRectangle
  fromPropsAxisValues: GridAutoOrTemplateBase | null
  padding: number | null
}

export const GridResizingControl = React.memo((props: GridResizingControlProps) => {
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

  const [resizing, setResizing] = React.useState(false)

  const mouseDownHandler = React.useCallback(
    (event: React.MouseEvent): void => {
      function mouseUpHandler() {
        setResizing(false)
        window.removeEventListener('mouseup', mouseUpHandler)
      }
      window.addEventListener('mouseup', mouseUpHandler)

      const start = windowToCanvasCoordinates(
        scale,
        canvasOffset,
        windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
      )
      setResizing(true)

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
    [canvasOffset, dispatch, props.axis, props.dimensionIndex, scale],
  )

  const labelId = `grid-${props.axis}-handle-${props.dimensionIndex}`
  const containerId = `${labelId}-container`

  const shadowSize = React.useMemo(() => {
    return props.axis === 'column'
      ? props.containingFrame.height + GRID_RESIZE_HANDLE_CONTAINER_SIZE
      : props.containingFrame.width + GRID_RESIZE_HANDLE_CONTAINER_SIZE
  }, [props.containingFrame, props.axis])

  return (
    <div
      key={containerId}
      data-testid={containerId}
      style={{
        display: 'flex',
        alignItems: props.axis === 'column' ? 'flex-start' : 'center',
        justifyContent: props.axis === 'column' ? 'center' : 'flex-start',
        border: `1px solid ${resizing ? colorTheme.brandNeonPink.value : 'transparent'}`,
        height: props.axis === 'column' && resizing ? shadowSize : '100%',
        width: props.axis === 'row' && resizing ? shadowSize : '100%',
        position: 'relative',
        ...(resizing
          ? UtopiaStyles.backgrounds.stripedBackground(colorTheme.brandNeonPink60.value, scale)
          : {}),
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
              ${1 / scale}px, ${colorTheme.canvasControlsSizeBoxShadowColor20.value} 0px ${
            1 / scale
          }px ${2 / scale}px ${1 / scale}px`,
          background: colorTheme.white.value,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          cursor: gridEdgeToCSSCursor(props.axis === 'column' ? 'column-start' : 'row-start'),
          fontSize: 8,
          position: 'relative',
        }}
        css={{
          opacity: resizing ? 1 : 0.5,
          ':hover': {
            opacity: 1,
          },
        }}
        onMouseDown={mouseDownHandler}
      >
        {props.axis === 'row' ? '↕' : '↔'}
        {when(
          props.dimension.areaName != null,
          <span style={{ position: 'absolute', top: 12 }}>{props.dimension.areaName}</span>,
        )}
      </div>
      {when(
        resizing,
        <div
          style={{
            position: 'absolute',
            top: props.axis === 'column' ? GRID_RESIZE_HANDLE_CONTAINER_SIZE : 0,
            left: props.axis === 'row' ? GRID_RESIZE_HANDLE_CONTAINER_SIZE : 0,
            right: 0,
            bottom: 0,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          <CanvasLabel
            value={getLabelForAxis(
              props.dimension,
              props.dimensionIndex,
              props.fromPropsAxisValues,
            )}
            scale={scale}
            color={colorTheme.brandNeonPink.value}
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
  containingFrame: CanvasRectangle
  axis: 'row' | 'column'
  gap: number | null
  padding: Sides | null
}

export const GridResizing = React.memo((props: GridResizingProps) => {
  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'GridResizing canvasScale',
  )
  if (props.axisValues == null) {
    return null
  }
  switch (props.axisValues.type) {
    case 'DIMENSIONS':
      const size = GRID_RESIZE_HANDLE_CONTAINER_SIZE / canvasScale
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
                ? props.axisValues.dimensions.map((dim) => `${dim.value}${dim.unit}`).join(' ')
                : undefined,
            gridTemplateRows:
              props.axis === 'row'
                ? props.axisValues.dimensions.map((dim) => `${dim.value}${dim.unit}`).join(' ')
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
          {props.axisValues.dimensions.flatMap((dimension, dimensionIndex) => {
            return (
              <GridResizingControl
                key={`grid-resizing-control-${dimensionIndex}`}
                dimensionIndex={dimensionIndex}
                dimension={dimension}
                fromPropsAxisValues={props.fromPropsAxisValues}
                axis={props.axis}
                containingFrame={props.containingFrame}
                padding={
                  props.padding == null
                    ? 0
                    : props.axis === 'column'
                    ? props.padding.left ?? 0
                    : props.padding.top ?? 0
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

export const GridControls = controlForStrategyMemoized(() => {
  const dispatch = useDispatch()
  const controls = useAnimationControls()
  const colorTheme = useColorTheme()
  const features = useRollYourOwnFeatures()

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  const activelyDraggingOrResizingCell = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE'
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    'GridControls activelyDraggingOrResizingCell',
  )

  const { hoveringStart, mouseCanvasPosition } = useMouseMove(activelyDraggingOrResizingCell)

  const targetRootCell = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState.customStrategyState.grid.currentRootCell,
    'GridControls targetRootCell',
  )

  const dragging = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE'
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    'GridControls dragging',
  )

  const interactionData = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG'
        ? store.editor.canvas.interactionSession.interactionData
        : null,
    'GridControls interactionData',
  )

  const interactionLatestMetadata = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG'
        ? store.editor.canvas.interactionSession.latestMetadata
        : null,
    'GridControls interactionLatestMetadata',
  )

  const editorMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'GridControls editorMetadata',
  )

  const jsxMetadata = React.useMemo(
    () => interactionLatestMetadata ?? editorMetadata,
    [interactionLatestMetadata, editorMetadata],
  )

  const grids = useEditorState(
    Substores.metadataAndPropertyControlsInfo,
    (store) => {
      return mapDropNulls((view) => {
        const element = MetadataUtils.findElementByElementPath(jsxMetadata, view)
        const parent = MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(view))

        const targetGridContainer = MetadataUtils.isGridLayoutedContainer(element)
          ? element
          : MetadataUtils.isGridLayoutedContainer(parent)
          ? parent
          : null

        if (
          targetGridContainer == null ||
          targetGridContainer.globalFrame == null ||
          !isFiniteRectangle(targetGridContainer.globalFrame)
        ) {
          return null
        }

        const gap = targetGridContainer.specialSizeMeasurements.gap
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
          frame: targetGridContainer.globalFrame,
          gridTemplateColumns: gridTemplateColumns,
          gridTemplateRows: gridTemplateRows,
          gridTemplateColumnsFromProps: gridTemplateColumnsFromProps,
          gridTemplateRowsFromProps: gridTemplateRowsFromProps,
          gap: gap,
          padding: padding,
          rows: rows,
          columns: columns,
          cells: rows * columns,
        }
      }, store.editor.selectedViews)
    },
    'GridControls grids',
  )

  const cells = React.useMemo(() => {
    return grids.flatMap((grid) => {
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
              : columnFromProps === 'auto'
              ? countedColumn
              : columnFromProps.numericalPosition ?? countedColumn,
          row:
            rowFromProps == null
              ? countedRow
              : rowFromProps === 'auto'
              ? countedRow
              : rowFromProps.numericalPosition ?? countedRow,
          index: index,
        }
      }, children)
    })
  }, [grids, jsxMetadata])

  const shadow = React.useMemo(() => {
    return cells.find((cell) => EP.toUid(cell.elementPath) === dragging) ?? null
  }, [cells, dragging])

  const [initialShadowFrame, setInitialShadowFrame] = React.useState<CanvasRectangle | null>(
    shadow?.globalFrame ?? null,
  )

  const gridPath = optionalMap(EP.parentPath, shadow?.elementPath)

  useSnapAnimation({
    targetRootCell: targetRootCell,
    controls: controls,
    shadowFrame: initialShadowFrame,
    gridPath: gridPath,
  })

  const startInteractionWithUid = React.useCallback(
    (params: { uid: string; row: number; column: number; frame: CanvasRectangle }) =>
      (event: React.MouseEvent) => {
        setInitialShadowFrame(params.frame)

        const start = windowToCanvasCoordinates(
          scaleRef.current,
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
    [canvasOffsetRef, dispatch, scaleRef],
  )

  // NOTE: this stuff is meant to be temporary, until we settle on the set of interaction pieces we like.
  // After that, we should get rid of this.
  const shadowOpacity = React.useMemo(() => {
    if (shadow == null || initialShadowFrame == null || interactionData == null) {
      return 0
    } else if (!features.Grid.adaptiveOpacity) {
      return features.Grid.opacityBaseline
    } else if (features.Grid.dragLockedToCenter) {
      return Math.min(
        (0.2 * distance(getRectCenter(shadow.globalFrame), mouseCanvasPosition)) /
          Math.min(shadow.globalFrame.height, shadow.globalFrame.width) +
          0.05,
        features.Grid.opacityBaseline,
      )
    } else {
      return Math.min(
        (0.2 *
          distance(
            offsetPoint(
              interactionData.dragStart,
              pointDifference(initialShadowFrame, shadow.globalFrame),
            ),
            mouseCanvasPosition,
          )) /
          Math.min(shadow.globalFrame.height, shadow.globalFrame.width) +
          0.05,
        features.Grid.opacityBaseline,
      )
    }
  }, [features, shadow, initialShadowFrame, interactionData, mouseCanvasPosition])

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
      } else if (features.Grid.dragLockedToCenter) {
        return (
          shadow.globalFrame[axis] +
          drag[axis] -
          (shadow.globalFrame[axis] - dragStart[axis]) -
          shadow.globalFrame[dimension] / 2
        )
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

    return { x: getCoord('x', 'width'), y: getCoord('y', 'height') }
  }, [features, initialShadowFrame, interactionData, shadow, hoveringStart, mouseCanvasPosition])

  if (grids.length === 0) {
    return null
  }

  return (
    <React.Fragment>
      <CanvasOffsetWrapper>
        {/* grid lines */}
        {grids.map((grid) => {
          const placeholders = Array.from(Array(grid.cells).keys())

          return (
            <div
              key={`grid-${EP.toString(grid.elementPath)}`}
              style={{
                position: 'absolute',
                top: grid.frame.y,
                left: grid.frame.x,
                width: grid.frame.width,
                height: grid.frame.height,
                display: 'grid',
                gridTemplateColumns: getNullableAutoOrTemplateBaseString(grid.gridTemplateColumns),
                gridTemplateRows: getNullableAutoOrTemplateBaseString(grid.gridTemplateRows),
                backgroundColor:
                  activelyDraggingOrResizingCell != null
                    ? colorTheme.primary10.value
                    : 'transparent',
                border: `1px solid ${
                  activelyDraggingOrResizingCell != null ? colorTheme.primary.value : 'transparent'
                }`,
                gap: grid.gap ?? 0,
                padding:
                  grid.padding == null
                    ? 0
                    : `${grid.padding.top}px ${grid.padding.right}px ${grid.padding.bottom}px ${grid.padding.left}px`,
              }}
            >
              {placeholders.map((cell) => {
                const countedRow = Math.floor(cell / grid.columns) + 1
                const countedColumn = Math.floor(cell % grid.columns) + 1
                const id = gridCellTargetId(grid.elementPath, countedRow, countedColumn)
                const dotgridColor =
                  activelyDraggingOrResizingCell != null
                    ? features.Grid.dotgridColor
                    : 'transparent'
                const borderColor =
                  activelyDraggingOrResizingCell != null
                    ? features.Grid.activeGridColor
                    : features.Grid.inactiveGridColor

                return (
                  <div
                    key={id}
                    id={id}
                    data-testid={id}
                    style={{
                      border: `1px solid ${borderColor}`,
                      position: 'relative',
                    }}
                    data-grid-row={countedRow}
                    data-grid-column={countedColumn}
                  >
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
                  </div>
                )
              })}
            </div>
          )
        })}
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
        shadow != null &&
        initialShadowFrame != null &&
        interactionData?.dragStart != null &&
        interactionData?.drag != null &&
        hoveringStart != null ? (
          <motion.div
            initial={'normal'}
            variants={{
              normal: { scale: 1 },
              [SHADOW_SNAP_ANIMATION]: {
                scale: [1, 1.4, 1],
                transition: { duration: 0.15, type: 'tween' },
              },
            }}
            animate={controls}
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
              opacity: shadowOpacity,
              border: '1px solid white',
              top: shadowPosition?.y,
              left: shadowPosition?.x,
            }}
          />
        ) : null}
        {grids.flatMap((grid) => {
          return (
            <GridResizing
              key={`grid-resizing-column-${EP.toString(grid.elementPath)}`}
              axisValues={grid.gridTemplateColumns}
              fromPropsAxisValues={grid.gridTemplateColumnsFromProps}
              containingFrame={grid.frame}
              axis={'column'}
              gap={grid.gap}
              padding={grid.padding}
            />
          )
        })}
        {grids.flatMap((grid) => {
          return (
            <GridResizing
              key={`grid-resizing-row-${EP.toString(grid.elementPath)}`}
              axisValues={grid.gridTemplateRows}
              fromPropsAxisValues={grid.gridTemplateRowsFromProps}
              containingFrame={grid.frame}
              axis={'row'}
              gap={grid.gap}
              padding={grid.padding}
            />
          )
        })}
      </CanvasOffsetWrapper>
    </React.Fragment>
  )
})

function useSnapAnimation(params: {
  gridPath: ElementPath | null
  shadowFrame: CanvasRectangle | null
  targetRootCell: GridCellCoordinates | null
  controls: AnimationControls
}) {
  const { gridPath, targetRootCell, controls, shadowFrame } = params
  const features = useRollYourOwnFeatures()

  const [lastTargetRootCellId, setLastTargetRootCellId] = React.useState(targetRootCell)
  const [lastSnapPoint, setLastSnapPoint] = React.useState<CanvasPoint | null>(shadowFrame)

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'useSnapAnimation selectedViews',
  )

  const animate = useCanvasAnimation(selectedViews)

  const canvasScale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'useSnapAnimation canvasScale',
  )

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    'useSnapAnimation canvasOffset',
  )

  const moveFromPoint = React.useMemo(() => {
    return lastSnapPoint ?? shadowFrame
  }, [lastSnapPoint, shadowFrame])

  const snapPoint = React.useMemo(() => {
    if (gridPath == null || targetRootCell == null) {
      return null
    }

    const element = document.getElementById(
      gridCellTargetId(gridPath, targetRootCell.row, targetRootCell.column),
    )
    if (element == null) {
      return null
    }

    const rect = element.getBoundingClientRect()
    const point = windowPoint({ x: rect.x, y: rect.y })

    return windowToCanvasCoordinates(canvasScale, canvasOffset, point).canvasPositionRounded
  }, [canvasScale, canvasOffset, gridPath, targetRootCell])

  React.useEffect(() => {
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
          { duration: CELL_ANIMATION_DURATION },
        )

        if (features.Grid.animateShadowSnap) {
          void controls.start(SHADOW_SNAP_ANIMATION)
        }
      }
    }
    setLastSnapPoint(snapPoint)
    setLastTargetRootCellId(targetRootCell)
  }, [
    targetRootCell,
    controls,
    features.Grid.animateShadowSnap,
    lastSnapPoint,
    snapPoint,
    animate,
    moveFromPoint,
    lastTargetRootCellId,
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
      'GridResizeShadow element',
    )

    const dispatch = useDispatch()
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'GridResizingControl scale',
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
            backgroundColor: isResizing ? colorTheme.whiteOpacity30.value : 'transparent',
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

function gridEdgeToEdgePosition(edge: GridResizeEdge): EdgePosition {
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
