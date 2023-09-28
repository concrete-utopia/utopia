import React from 'react'
import { colorTheme } from '../../uuiui'
import { useGridPanelDragInfo, useGridPanelDropArea } from './grid-panels-dnd'
import type { LayoutUpdate, StoredPanel } from './grid-panels-state'
import {
  ExtraHorizontalDropTargetPadding,
  GridPanelHorizontalGapHalf,
  ResizeColumnWidth,
  wrapAroundColIndex,
} from './grid-panels-state'
import { CSSCursor } from './canvas-types'
import { usePropControlledRef_DANGEROUS } from '../inspector/common/inspector-utils'

export const ColumnDragTargets = React.memo(
  (props: {
    columnIndex: number
    columnWidth: number
    canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
    onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
    setColumnWidth: (columnIndex: number, newWidth: number) => void
  }) => {
    const { columnIndex: columnIndexProp, onDrop, canDrop, columnWidth, setColumnWidth } = props

    const columnIndex = wrapAroundColIndex(columnIndexProp)
    const columnWidthRef = usePropControlledRef_DANGEROUS(columnWidth)

    const { isDragActive, draggedPanel } = useGridPanelDragInfo()

    const leftSideOfGrid = columnIndex > -1

    const dropBeforeColumn: LayoutUpdate = React.useMemo(
      () => ({
        type: 'before-column',
        columnIndex: columnIndex,
      }),
      [columnIndex],
    )

    const dropAfterColumn: LayoutUpdate = React.useMemo(
      () => ({
        type: 'after-column',
        columnIndex: columnIndex,
      }),
      [columnIndex],
    )

    const canDropBefore = draggedPanel != null && canDrop(draggedPanel, dropBeforeColumn)
    const canDropAfter = draggedPanel != null && canDrop(draggedPanel, dropAfterColumn)

    const { drop: dropBefore, isOver: isOverBefore } = useGridPanelDropArea(
      React.useCallback(
        (itemToMove: StoredPanel) => onDrop(itemToMove, dropBeforeColumn),
        [onDrop, dropBeforeColumn],
      ),
    )

    const { drop: dropAfter, isOver: isOverAfter } = useGridPanelDropArea(
      React.useCallback(
        (itemToMove: StoredPanel) => {
          onDrop(itemToMove, dropAfterColumn)
        },
        [onDrop, dropAfterColumn],
      ),
    )

    const handleResizeMouseDown = React.useCallback(
      (mouseDownEvent: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
        const startingColumnWidth = columnWidthRef.current

        const onMouseMove = (mouseMoveEvent: MouseEvent) => {
          const mouseDelta = mouseMoveEvent.clientX - mouseDownEvent.clientX
          setColumnWidth(columnIndex, startingColumnWidth + mouseDelta)
        }
        const onMouseUp = () => {
          window.removeEventListener('mousemove', onMouseMove, { capture: true })
          window.removeEventListener('mouseup', onMouseUp, { capture: true })
        }
        window.addEventListener('mousemove', onMouseMove, { capture: true })
        window.addEventListener('mouseup', onMouseUp, { capture: true })
      },
      [columnIndex, columnWidthRef, setColumnWidth],
    )

    return (
      <>
        <div
          onMouseDown={handleResizeMouseDown}
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            gridRowStart: 1,
            gridRowEnd: -1,
            gridColumn: `col ${leftSideOfGrid ? columnIndex + 1 : columnIndex} / span 1`,
            width: ResizeColumnWidth,
            height: '100%',
            ...(leftSideOfGrid
              ? { right: -ResizeColumnWidth / 2 + GridPanelHorizontalGapHalf } // for the left hand side resize columns
              : { left: -ResizeColumnWidth / 2 + GridPanelHorizontalGapHalf }), // right hand side resize columns
            cursor: CSSCursor.ResizeEW,
          }}
        />
        <div
          ref={dropBefore}
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            gridRowStart: 1,
            gridRowEnd: -1,
            gridColumn: `col ${leftSideOfGrid ? columnIndex + 1 : columnIndex} / span 1`,
            display: isDragActive && canDropBefore ? 'block' : 'none',
            width: 2 * ExtraHorizontalDropTargetPadding + 2 * GridPanelHorizontalGapHalf,
            height: '100%',
            left: -(ExtraHorizontalDropTargetPadding + GridPanelHorizontalGapHalf),
          }}
        >
          <div
            style={{
              display: isOverBefore ? 'block' : 'none',
              position: 'absolute',
              left: `calc(50% - 1px)`,
              width: 2,
              height: '100%',
              backgroundColor: colorTheme.primary.value,
            }}
          />
        </div>
        <div
          ref={dropAfter}
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            gridRowStart: 1,
            gridRowEnd: -1,
            gridColumn: `col ${leftSideOfGrid ? columnIndex + 1 : columnIndex} / span 1`,
            display: isDragActive && canDropAfter ? 'block' : 'none',
            width: 2 * ExtraHorizontalDropTargetPadding + 2 * GridPanelHorizontalGapHalf,
            height: '100%',
            right: -(ExtraHorizontalDropTargetPadding + GridPanelHorizontalGapHalf),
          }}
        >
          <div
            style={{
              display: isOverAfter ? 'block' : 'none',
              position: 'absolute',
              left: `calc(50% - 1px)`,
              width: 2,
              height: '100%',
              backgroundColor: colorTheme.primary.value,
            }}
          />
        </div>
      </>
    )
  },
)
