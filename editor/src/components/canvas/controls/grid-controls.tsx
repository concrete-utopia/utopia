import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { mapDropNulls } from '../../../core/shared/array-utils'
import type { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import {
  canvasPoint,
  distance,
  getRectCenter,
  isFiniteRectangle,
  offsetPoint,
  pointDifference,
  windowPoint,
} from '../../../core/shared/math-utils'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { createInteractionViaMouse, gridCellHandle } from '../canvas-strategies/interaction-state'
import CanvasActions from '../canvas-actions'
import { Modifier } from '../../../utils/modifiers'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { type GridAutoOrTemplateBase } from '../../../core/shared/element-template'
import { assertNever } from '../../../core/shared/utils'
import { printGridAutoOrTemplateBase } from '../../../components/inspector/common/css-utils'
import { when } from '../../../utils/react-conditionals'
import { CanvasMousePositionRaw } from '../../../utils/global-positions'
import { motion, useAnimationControls } from 'framer-motion'
import { useRollYourOwnFeatures } from '../../navigator/left-pane/roll-your-own-pane'
import type { ElementPath } from 'utopia-shared/src/types'

export const GridCellTestId = (elementPath: ElementPath) => `grid-cell-${EP.toString(elementPath)}`

type GridCellCoordinates = { row: number; column: number }

function emptyGridCellCoordinates(): GridCellCoordinates {
  return { row: 0, column: 0 }
}

export let TargetGridCell = { current: emptyGridCellCoordinates() }

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

const SHADOW_SNAP_ANIMATION = 'shadow-snap'

export const GridControls = controlForStrategyMemoized(() => {
  const features = useRollYourOwnFeatures()

  const activelyDraggingOrResizingCell = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE'
        ? store.editor.canvas.interactionSession.activeControl.id
        : null,
    'GridControls activelyDraggingOrResizingCell',
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

  const interactionSession = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG'
        ? store.editor.canvas.interactionSession.interactionData
        : null,
    'GridControls interactionSession',
  )

  const jsxMetadata = useEditorState(
    Substores.fullStore,
    (store) => store.editor.canvas.interactionSession?.latestMetadata ?? store.editor.jsxMetadata,
    'GridControls jsxMetadata',
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

  const dispatch = useDispatch()

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  const shadow = React.useMemo(() => {
    return cells.find((cell) => EP.toUid(cell.elementPath) === dragging)
  }, [cells, dragging])

  const [initialShadowFrame, setInitialShadowFrame] = React.useState<CanvasRectangle | null>(
    shadow?.globalFrame ?? null,
  )

  const startInteractionWithUid = React.useCallback(
    (params: { uid: string; row: number; column: number; frame: CanvasRectangle }) =>
      (event: React.MouseEvent) => {
        TargetGridCell.current = emptyGridCellCoordinates()

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

  const [hoveringCell, setHoveringCell] = React.useState<string | null>(null)
  const [hoveringStart, setHoveringStart] = React.useState<{
    id: string
    point: CanvasPoint
  } | null>(null)
  const controls = useAnimationControls()

  React.useEffect(() => {
    if (!features.Grid.animateSnap) {
      return
    }
    if (hoveringCell == null) {
      return
    }
    void controls.start(SHADOW_SNAP_ANIMATION)
  }, [hoveringCell, controls, features.Grid.animateSnap])

  React.useEffect(() => {
    function handleMouseMove(e: MouseEvent) {
      if (activelyDraggingOrResizingCell == null) {
        setHoveringStart(null)
        return
      }
      const cellsUnderMouse = document
        .elementsFromPoint(e.clientX, e.clientY)
        .filter((el) => el.id.startsWith(`gridcell-`))

      if (cellsUnderMouse.length > 0) {
        const cellUnderMouse = cellsUnderMouse[0]
        const row = cellUnderMouse.getAttribute('data-grid-row')
        const column = cellUnderMouse.getAttribute('data-grid-column')
        TargetGridCell.current.row = row == null ? 0 : parseInt(row)
        TargetGridCell.current.column = column == null ? 0 : parseInt(column)
        setHoveringCell(cellUnderMouse.id)

        setHoveringStart((start) => {
          if (start == null || start.id !== cellUnderMouse.id) {
            return { id: cellUnderMouse.id, point: canvasPoint(CanvasMousePositionRaw!) }
          }
          return start
        })
      }
    }
    window.addEventListener('mousemove', handleMouseMove)
    return function () {
      window.removeEventListener('mousemove', handleMouseMove)
    }
  }, [activelyDraggingOrResizingCell])

  const shadowOpacity = React.useMemo(() => {
    if (shadow == null || initialShadowFrame == null || interactionSession == null) {
      return 0
    } else if (!features.Grid.adaptiveOpacity) {
      return features.Grid.opacityBaseline
    } else if (features.Grid.dragLockedToCenter) {
      return Math.min(
        (0.2 * distance(getRectCenter(shadow.globalFrame), CanvasMousePositionRaw!)) /
          Math.min(shadow.globalFrame.height, shadow.globalFrame.width) +
          0.05,
        features.Grid.opacityBaseline,
      )
    } else {
      return Math.min(
        (0.2 *
          distance(
            offsetPoint(
              interactionSession.dragStart,
              pointDifference(initialShadowFrame, shadow.globalFrame),
            ),
            CanvasMousePositionRaw!,
          )) /
          Math.min(shadow.globalFrame.height, shadow.globalFrame.width) +
          0.05,
        features.Grid.opacityBaseline,
      )
    }
  }, [features, shadow, initialShadowFrame, interactionSession])

  const shadowPosition = React.useMemo(() => {
    if (
      initialShadowFrame == null ||
      interactionSession == null ||
      interactionSession.drag == null ||
      hoveringStart == null ||
      shadow == null
    ) {
      return null
    }

    function getCoord(axis: 'x' | 'y', dimension: 'width' | 'height') {
      if (
        initialShadowFrame == null ||
        interactionSession == null ||
        interactionSession.drag == null ||
        hoveringStart == null ||
        shadow == null
      ) {
        return undefined
      } else if (features.Grid.dragVerbatim) {
        return initialShadowFrame[axis] + interactionSession.drag[axis]
      } else if (features.Grid.dragLockedToCenter) {
        return (
          shadow.globalFrame[axis] +
          interactionSession.drag[axis] -
          (shadow.globalFrame[axis] - interactionSession.dragStart[axis]) -
          shadow.globalFrame[dimension] / 2
        )
      } else if (features.Grid.dragMagnetic) {
        return (
          shadow.globalFrame[axis] + (CanvasMousePositionRaw![axis] - hoveringStart.point[axis])
        )
      } else if (features.Grid.dragRatio) {
        return (
          shadow.globalFrame[axis] +
          interactionSession.drag[axis] -
          (shadow.globalFrame[axis] - interactionSession.dragStart[axis]) -
          shadow.globalFrame[dimension] *
            ((interactionSession.dragStart[axis] - initialShadowFrame[axis]) /
              initialShadowFrame[dimension])
        )
      } else {
        return undefined
      }
    }

    return { x: getCoord('x', 'width'), y: getCoord('y', 'height') }
  }, [features, initialShadowFrame, interactionSession, shadow, hoveringStart])

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
                  activelyDraggingOrResizingCell != null ? '#0099ff11' : 'transparent',
                border: `1px solid ${
                  activelyDraggingOrResizingCell != null ? '#09f' : 'transparent'
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
          interactionSession?.dragStart != null &&
          interactionSession?.drag != null &&
          hoveringStart != null && (
            <motion.div
              // key={`shadow-for-${activelyDraggingCell}`}
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
          )}
      </CanvasOffsetWrapper>
    </React.Fragment>
  )
})
