import type { AnimationControls } from 'framer-motion'
import { motion, useAnimationControls } from 'framer-motion'
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
  offsetPoint,
  pointDifference,
  pointsEqual,
  windowPoint,
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
import {
  createInteractionViaMouse,
  gridAxisHandle,
  gridCellHandle,
} from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { useColorTheme } from '../../../uuiui'
import { gridCellTargetId } from '../canvas-strategies/strategies/grid-helpers'
import { useCanvasAnimation } from '../ui-jsx-canvas-renderer/animation-context'
import { CanvasLabel } from './select-mode/controls-common'
import { optionalMap } from '../../../core/shared/optional-utils'

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

const GridResizingContainerSize = 100

export interface GridResizingControlProps {
  dimension: GridCSSNumber
  dimensionIndex: number
  axis: 'row' | 'column'
  containingFrame: CanvasRectangle
  workingPrefix: number
  fromPropsAxisValues: GridAutoOrTemplateBase | null
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

  const mouseDownHandler = React.useCallback(
    (event: React.MouseEvent): void => {
      const start = windowToCanvasCoordinates(
        scale,
        canvasOffset,
        windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
      )

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

  return (
    <div
      key={containerId}
      data-testid={containerId}
      style={{
        position: 'absolute',
        left:
          props.axis === 'column' ? props.workingPrefix - GridResizingContainerSize / 2 : undefined,
        top:
          props.axis === 'row'
            ? props.workingPrefix - GridResizingContainerSize / 2
            : props.containingFrame.y - 30 / scale,
        right: props.axis === 'row' ? 10 / scale - props.containingFrame.x : undefined,
        width: props.axis === 'column' ? GridResizingContainerSize : `max-content`,
        height: props.axis === 'row' ? GridResizingContainerSize : `max-content`,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <CanvasLabel
        testId={labelId}
        value={getLabelForAxis(props.dimension, props.dimensionIndex, props.fromPropsAxisValues)}
        scale={scale}
        color={colorTheme.brandNeonPink.value}
        textColor={colorTheme.white.value}
        onMouseDown={mouseDownHandler}
      />
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
}

export const GridResizing = React.memo((props: GridResizingProps) => {
  if (props.axisValues == null) {
    return null
  } else {
    switch (props.axisValues.type) {
      case 'DIMENSIONS':
        let workingPrefix: number =
          props.axis === 'column' ? props.containingFrame.x : props.containingFrame.y
        return (
          <>
            {props.axisValues.dimensions.flatMap((dimension, dimensionIndex) => {
              // Assumes pixels currently.
              workingPrefix += dimension.value
              if (dimensionIndex === 0) {
                // Shift by half the gap initially...
                workingPrefix += (props.gap ?? 0) / 2
              } else {
                // ...Then by the full gap, as it would be half from the prior entry
                // and half from the current one.
                workingPrefix += props.gap ?? 0
              }

              return (
                <GridResizingControl
                  dimensionIndex={dimensionIndex}
                  dimension={dimension}
                  fromPropsAxisValues={props.fromPropsAxisValues}
                  axis={props.axis}
                  containingFrame={props.containingFrame}
                  workingPrefix={workingPrefix}
                />
              )
            })}
          </>
        )
      case 'FALLBACK':
        return null
      default:
        assertNever(props.axisValues)
        return null
    }
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
                      <>
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
                      </>,
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
              axisValues={grid.gridTemplateColumns}
              fromPropsAxisValues={grid.gridTemplateColumnsFromProps}
              containingFrame={grid.frame}
              axis={'column'}
              gap={grid.gap}
            />
          )
        })}
        {grids.flatMap((grid) => {
          return (
            <GridResizing
              axisValues={grid.gridTemplateRows}
              fromPropsAxisValues={grid.gridTemplateRowsFromProps}
              containingFrame={grid.frame}
              axis={'row'}
              gap={grid.gap}
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
