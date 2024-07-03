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
  canvasRectangle,
  distance,
  getRectCenter,
  isFiniteRectangle,
  isInfinityRectangle,
  offsetPoint,
  pointDifference,
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
import type { GridResizeEdge } from '../canvas-strategies/interaction-state'
import {
  GridResizeEdges,
  createInteractionViaMouse,
  gridAxisHandle,
  gridCellHandle,
  gridResizeHandle,
} from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { useColorTheme } from '../../../uuiui'
import { getGridCellUnderMouse } from '../canvas-strategies/strategies/grid-helpers'

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

  const { hoveringCell, hoveringStart, mouseCanvasPosition } = useMouseMove(
    activelyDraggingOrResizingCell,
  )
  useSnapAnimation(hoveringCell, controls)

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
    return cells.find((cell) => EP.toUid(cell.elementPath) === dragging)
  }, [cells, dragging])

  const [initialShadowFrame, setInitialShadowFrame] = React.useState<CanvasRectangle | null>(
    shadow?.globalFrame ?? null,
  )

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
        {grids.map((grid, index) => {
          const placeholders = Array.from(Array(grid.cells).keys())

          return (
            <div
              key={`grid-${index}`}
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
              {placeholders.map((cell, cellIndex) => {
                const countedRow = Math.floor(cellIndex / grid.columns) + 1
                const countedColumn = Math.floor(cellIndex % grid.columns) + 1
                const id = `gridcell-${index}-${cell}`
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
              }}
            />
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
              key={`grid-cell-${EP.toString(cell.elementPath)}`}
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
          if (grid.gridTemplateColumns == null) {
            return []
          } else {
            switch (grid.gridTemplateColumns.type) {
              case 'DIMENSIONS':
                let workingPrefix: number = grid.frame.x
                return grid.gridTemplateColumns.dimensions.flatMap((dimension, dimensionIndex) => {
                  // Assumes pixels currently.
                  workingPrefix += dimension.value
                  if (dimensionIndex !== 0) {
                    workingPrefix += grid.gap ?? 0
                  }
                  function mouseDownHandler(event: React.MouseEvent): void {
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
                          gridAxisHandle('column', dimensionIndex),
                          'zero-drag-not-permitted',
                        ),
                      ),
                    ])
                    event.stopPropagation()
                    event.preventDefault()
                  }

                  return (
                    <div
                      data-testid={`grid-column-handle-${dimensionIndex}`}
                      style={{
                        position: 'absolute',
                        left: workingPrefix - 15,
                        top: grid.frame.y - 30,
                        width: 40,
                        height: '20px',
                        borderRadius: 5,
                        backgroundColor: colorTheme.brandNeonPink.value,
                        color: colorTheme.white.value,
                        display: 'flex',
                        justifyContent: 'center',
                        alignItems: 'center',
                      }}
                      onMouseDown={mouseDownHandler}
                    >
                      {getLabelForAxis(
                        dimension,
                        dimensionIndex,
                        grid.gridTemplateColumnsFromProps,
                      )}
                    </div>
                  )
                })
              case 'FALLBACK':
                return []
              default:
                assertNever(grid.gridTemplateColumns)
                return []
            }
          }
        })}
        {grids.flatMap((grid) => {
          if (grid.gridTemplateRows == null) {
            return []
          } else {
            switch (grid.gridTemplateRows.type) {
              case 'DIMENSIONS':
                let workingPrefix: number = grid.frame.y
                return grid.gridTemplateRows.dimensions.flatMap((dimension, dimensionIndex) => {
                  // Assumes pixels currently.
                  workingPrefix += dimension.value
                  if (dimensionIndex !== 0) {
                    workingPrefix += grid.gap ?? 0
                  }
                  function mouseDownHandler(event: React.MouseEvent): void {
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
                          gridAxisHandle('row', dimensionIndex),
                          'zero-drag-not-permitted',
                        ),
                      ),
                    ])
                    event.stopPropagation()
                    event.preventDefault()
                  }
                  return (
                    <div
                      data-testid={`grid-row-handle-${dimensionIndex}`}
                      style={{
                        position: 'absolute',
                        left: grid.frame.x - 50,
                        top: workingPrefix - 5,
                        width: 40,
                        height: '20px',
                        borderRadius: 5,
                        backgroundColor: colorTheme.brandNeonPink.value,
                        color: colorTheme.white.value,
                        display: 'flex',
                        justifyContent: 'center',
                        alignItems: 'center',
                      }}
                      onMouseDown={mouseDownHandler}
                    >
                      {getLabelForAxis(dimension, dimensionIndex, grid.gridTemplateRowsFromProps)}
                    </div>
                  )
                })
              case 'FALLBACK':
                return []
              default:
                assertNever(grid.gridTemplateRows)
                return []
            }
          }
        })}
      </CanvasOffsetWrapper>
    </React.Fragment>
  )
})

function useSnapAnimation(hoveringCell: string | null, controls: AnimationControls) {
  const features = useRollYourOwnFeatures()
  React.useEffect(() => {
    if (!features.Grid.animateSnap || hoveringCell == null) {
      return
    }
    void controls.start(SHADOW_SNAP_ANIMATION)
  }, [hoveringCell, controls, features.Grid.animateSnap])
}

function useMouseMove(activelyDraggingOrResizingCell: string | null) {
  const [hoveringCell, setHoveringCell] = React.useState<string | null>(null)
  const [hoveringStart, setHoveringStart] = React.useState<{
    id: string
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

      // TODO most of this logic can be simplified consistently
      // by moving the hovering cell info to the editor state, dispatched
      // from the grid-rearrange-move-strategy logic.
      const cellUnderMouse = getGridCellUnderMouse(windowPoint({ x: e.clientX, y: e.clientY }))
      if (cellUnderMouse != null) {
        setHoveringCell(cellUnderMouse.id)

        const newMouseCanvasPosition = windowToCanvasCoordinates(
          canvasScale,
          canvasOffset,
          windowPoint({ x: e.clientX, y: e.clientY }),
        ).canvasPositionRaw
        setMouseCanvasPosition(newMouseCanvasPosition)

        setHoveringStart((start) => {
          if (start == null || start.id !== cellUnderMouse.id) {
            return { id: cellUnderMouse.id, point: canvasPoint(newMouseCanvasPosition) }
          }
          return start
        })
      }
    }
    window.addEventListener('mousemove', handleMouseMove)
    return function () {
      window.removeEventListener('mousemove', handleMouseMove)
    }
  }, [activelyDraggingOrResizingCell, canvasOffset, canvasScale])

  return { hoveringCell, hoveringStart, mouseCanvasPosition }
}

interface GridResizeControlProps {
  target: ElementPath
}

export const GridResizeControls = controlForStrategyMemoized<GridResizeControlProps>(
  ({ target }) => {
    const element = useEditorState(
      Substores.metadata,
      (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, target),
      'GridResizeShadow element',
    )

    const dispatch = useDispatch()
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

    const resizeControlRef = useRefEditorState((store) =>
      store.editor.canvas.interactionSession?.activeControl.type !== 'GRID_RESIZE_HANDLE'
        ? null
        : store.editor.canvas.interactionSession.activeControl,
    )

    const [bounds, setBounds] = React.useState<CanvasRectangle | null>(null)
    const onMouseMove = React.useCallback(
      (e: MouseEvent) => {
        if (resizeControlRef.current == null) {
          return
        }

        let delta: CanvasRectangle = canvasRectangle({ x: 0, y: 0, width: 0, height: 0 })
        switch (resizeControlRef.current.edge) {
          case 'column-end':
            delta.width = e.movementX
            break
          case 'column-start':
            delta.x = e.movementX
            delta.width = -e.movementX
            break
          case 'row-start':
            delta.y = e.movementY
            delta.height = -e.movementY
            break
          case 'row-end':
            delta.height = e.movementY
            break
          default:
            assertNever(resizeControlRef.current.edge)
        }

        setBounds((o) =>
          o == null
            ? null
            : canvasRectangle({
                x: o.x + delta.x,
                y: o.y + delta.y,
                width: o.width + delta.width,
                height: o.height + delta.height,
              }),
        )
      },
      [resizeControlRef],
    )

    const onMouseUp = React.useCallback(() => setBounds(null), [])

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
        setBounds(frame)
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
              gridResizeHandle(uid, edge),
              'zero-drag-not-permitted',
            ),
          ),
        ])
      },
      [canvasOffsetRef, dispatch, element?.globalFrame, scaleRef],
    )

    const colorTheme = useColorTheme()

    if (
      element == null ||
      element.globalFrame == null ||
      isInfinityRectangle(element.globalFrame)
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
            display: 'grid',
            gridTemplateRows: '10px 1fr 10px',
            gridTemplateColumns: '10px 1fr 10px',
            gridTemplateAreas: "'empty1 rs empty2' 'cs empty3 ce' 'empty4 re empty5'",
            backgroundColor: '#ffffff66',
          }}
        >
          {GridResizeEdges.map((edge) => (
            <div
              key={edge}
              style={{
                pointerEvents: 'none',
                width: '100%',
                height: '100%',
                display: 'flex',
                justifyContent: 'center',
                alignItems: 'center',
                gridArea: gridEdgeToGridArea(edge),
              }}
            >
              <div
                data-testid={`grid-resize-edge-${edge}`}
                onMouseDown={startResizeInteraction(EP.toUid(element.elementPath), edge)}
                style={{
                  pointerEvents: 'initial',
                  ...gridEdgeToWidthHeight(edge),
                  backgroundColor: 'black',
                  opacity: 0.5,
                }}
              />
            </div>
          ))}
        </div>
      </CanvasOffsetWrapper>
    )
  },
)

function gridEdgeToGridArea(edge: GridResizeEdge): string {
  switch (edge) {
    case 'column-end':
      return 'ce'
    case 'column-start':
      return 'cs'
    case 'row-end':
      return 're'
    case 'row-start':
      return 'rs'
    default:
      assertNever(edge)
  }
}

function gridEdgeToWidthHeight(edge: GridResizeEdge): {
  width: number
  height: number
  borderRadius: number
} {
  const LONG_EDGE = 24
  const SHORT_EDGE = 4
  switch (edge) {
    case 'column-end':
    case 'column-start':
      return { width: SHORT_EDGE, height: LONG_EDGE, borderRadius: SHORT_EDGE / 2 }
    case 'row-end':
    case 'row-start':
      return { width: LONG_EDGE, height: SHORT_EDGE, borderRadius: SHORT_EDGE / 2 }
    default:
      assertNever(edge)
  }
}
