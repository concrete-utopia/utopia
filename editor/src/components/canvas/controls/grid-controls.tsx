import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isFiniteRectangle, windowPoint } from '../../../core/shared/math-utils'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import {
  createInteractionViaMouse,
  gridCellHandle,
  gridResizeHandle,
} from '../canvas-strategies/interaction-state'
import CanvasActions from '../canvas-actions'
import { Modifier } from '../../../utils/modifiers'
import { windowToCanvasCoordinates } from '../dom-lookup'
import type { GridAutoOrTemplateBase } from '../../../core/shared/element-template'
import { assertNever } from '../../../core/shared/utils'
import { printGridAutoOrTemplateBase } from '../../../components/inspector/common/css-utils'

type GridCellCoordinates = { row: number; column: number }

function emptyGridCellCoordinates(): GridCellCoordinates {
  return { row: 0, column: 0 }
}

// TODO please forgive me (hackathon code)
export let TargetGridCell = { current: emptyGridCellCoordinates() }

function getSillyCellsCount(template: GridAutoOrTemplateBase | null): number {
  if (template == null) {
    return 0
  } else {
    switch (template.type) {
      case 'DIMENSIONS':
        return template.dimensions.length
      case 'FALLBACK':
        return 0
      default:
        assertNever(template)
    }
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
  const isActivelyDraggingCell = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE',
    '',
  )

  const jsxMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'GridControls jsxMetadata',
  )

  const grids = useEditorState(
    Substores.metadataAndPropertyControlsInfo,
    (store) => {
      return mapDropNulls((view) => {
        const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, view)
        const parent = MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          EP.parentPath(view),
        )

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

        const columns = getSillyCellsCount(
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns,
        )
        const rows = getSillyCellsCount(
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows,
        )

        return {
          elementPath: targetGridContainer.elementPath,
          frame: targetGridContainer.globalFrame,
          gridTemplateColumns: gridTemplateColumns,
          gridTemplateRows: gridTemplateRows,
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

  const startInteractionWithUid = React.useCallback(
    (params: { uid: string; row: number; column: number }) => (event: React.MouseEvent) => {
      TargetGridCell.current = emptyGridCellCoordinates()

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

  React.useEffect(() => {
    function h(e: MouseEvent) {
      if (!isActivelyDraggingCell) {
        return
      }
      const cellsUnderMouse = document
        .elementsFromPoint(e.clientX, e.clientY)
        .filter((el) => el.id.startsWith(`gridcell-`))

      // TODO this sucks!
      if (cellsUnderMouse.length > 0) {
        const cellUnderMouse = cellsUnderMouse[0]
        const row = cellUnderMouse.getAttribute('data-grid-row')
        const column = cellUnderMouse.getAttribute('data-grid-column')
        TargetGridCell.current.row = row == null ? 0 : parseInt(row)
        TargetGridCell.current.column = column == null ? 0 : parseInt(column)
      }
    }
    window.addEventListener('mousemove', h)
    return function () {
      window.removeEventListener('mousemove', h)
    }
  }, [isActivelyDraggingCell])

  const startResizeInteractionWithUid = React.useCallback(
    (uid: string) => (event: React.MouseEvent) => {
      event.stopPropagation()
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
            gridResizeHandle(uid),
            'zero-drag-not-permitted',
          ),
        ),
      ])
    },
    [canvasOffsetRef, dispatch, scaleRef],
  )

  if (grids.length === 0) {
    return null
  }

  return (
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
              //   backgroundColor: '#ff00ff09',
              display: 'grid',
              gridTemplateColumns: getNullableAutoOrTemplateBaseString(grid.gridTemplateColumns),
              gridTemplateRows: getNullableAutoOrTemplateBaseString(grid.gridTemplateRows),
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
              const edgeColor = isActivelyDraggingCell ? '#00000033' : 'transparent'
              const borderColor = isActivelyDraggingCell ? '#00000022' : '#0000000a'
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
                    <div style={{ width: 2, height: 2, backgroundColor: edgeColor }} />
                  </div>
                  <div
                    style={{
                      width: 2,
                      height: 2,
                      backgroundColor: edgeColor,
                      position: 'absolute',
                      top: -1,
                      left: -1,
                    }}
                  />
                  <div
                    style={{
                      width: 2,
                      height: 2,
                      backgroundColor: edgeColor,
                      position: 'absolute',
                      bottom: -1,
                      left: -1,
                    }}
                  />
                  <div
                    style={{
                      width: 2,
                      height: 2,
                      backgroundColor: edgeColor,
                      position: 'absolute',
                      top: -1,
                      right: -1,
                    }}
                  />
                  <div
                    style={{
                      width: 2,
                      height: 2,
                      backgroundColor: edgeColor,
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
            }}
          />
        )
      })}
    </CanvasOffsetWrapper>
  )
})
