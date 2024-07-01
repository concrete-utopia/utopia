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
import { CanvasMousePositionRaw } from '../../../utils/global-positions'
import { motion, useAnimationControls } from 'framer-motion'
import type { ElementPath } from '../../../core/shared/project-file-types'

export const GridCellTestId = (elementPath: ElementPath) => `grid-cell-${EP.toString(elementPath)}`

const OPACITY_BASELINE = 0.25

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

export const GridControls = controlForStrategyMemoized(() => {
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

  const [shadowFrame, setShadowFrame] = React.useState<CanvasRectangle | null>(
    shadow?.globalFrame ?? null,
  )

  const startInteractionWithUid = React.useCallback(
    (params: { uid: string; row: number; column: number; frame: CanvasRectangle }) =>
      (event: React.MouseEvent) => {
        TargetGridCell.current = emptyGridCellCoordinates()

        setShadowFrame(params.frame)

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
    if (hoveringCell == null) {
      return
    }

    void controls.start('boop')
  }, [hoveringCell, controls])

  React.useEffect(() => {
    function hover(e: MouseEvent) {
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
    window.addEventListener('mousemove', hover)
    return function () {
      window.removeEventListener('mousemove', hover)
    }
  }, [activelyDraggingOrResizingCell])

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
                    ? '#0099ffaa' // TODO: colortheme
                    : 'transparent'
                const borderColor =
                  activelyDraggingOrResizingCell != null ? '#0099ff77' : '#0000000a'

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
        {shadow != null &&
          shadowFrame != null &&
          interactionSession?.dragStart != null &&
          interactionSession?.drag != null &&
          hoveringStart != null && (
            <motion.div
              // key={`shadow-for-${activelyDraggingCell}`}
              initial={'normal'}
              variants={{
                normal: { scale: 1 },
                boop: {
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
                opacity: Math.min(
                  (0.2 *
                    distance(
                      offsetPoint(
                        interactionSession.dragStart,
                        pointDifference(shadowFrame, shadow.globalFrame),
                      ),
                      CanvasMousePositionRaw!,
                    )) /
                    Math.min(shadow.globalFrame.height, shadow.globalFrame.width) +
                    0.05,
                  OPACITY_BASELINE,
                ),
                border: '1px solid white',
                top:
                  shadow.globalFrame.y +
                  interactionSession.drag.y -
                  (shadow.globalFrame.y - interactionSession.dragStart.y) -
                  shadow.globalFrame.height *
                    ((interactionSession.dragStart.y - shadowFrame.y) / shadowFrame.height),
                left:
                  shadow.globalFrame.x +
                  interactionSession.drag.x -
                  (shadow.globalFrame.x - interactionSession.dragStart.x) -
                  shadow.globalFrame.width *
                    ((interactionSession.dragStart.x - shadowFrame.x) / shadowFrame.width),
              }}
            />
          )}
      </CanvasOffsetWrapper>
    </React.Fragment>
  )
})
