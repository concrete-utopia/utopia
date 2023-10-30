import React from 'react'
import { when } from '../../utils/react-conditionals'
import { colorTheme } from '../../uuiui'
import { usePropControlledRef_DANGEROUS } from '../inspector/common/inspector-utils'
import { CSSCursor } from './canvas-types'
import { useGridPanelDragInfo, useGridPanelDropArea } from './grid-panels-dnd'
import { wrapAroundColIndex } from './grid-panels-state'
import type { StoredPanel, LayoutUpdate } from './stored-layout'
import {
  ExtraHorizontalDropTargetPadding,
  GridPanelHorizontalGapHalf,
  IndexOfCanvas,
  ResizeColumnWidth,
} from './stored-layout'

export const ColumnDragTargets = React.memo(
  (props: {
    columnIndex: number
    canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
    onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  }) => {
    const { columnIndex: columnIndexProp, onDrop, canDrop } = props

    const columnIndex = wrapAroundColIndex(columnIndexProp)

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

    return (
      <>
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

export const CanvasPaneDragTargets = React.memo(
  (props: {
    canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
    onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  }) => {
    const { onDrop, canDrop } = props
    const { isDragActive, draggedPanel } = useGridPanelDragInfo()

    const dropBeforeColumn: LayoutUpdate = React.useMemo(
      () => ({
        type: 'before-column',
        columnIndex: IndexOfCanvas - 1,
      }),
      [],
    )

    const dropAfterColumn: LayoutUpdate = React.useMemo(
      () => ({
        type: 'after-column',
        columnIndex: IndexOfCanvas - 1,
      }),
      [],
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

    return (
      <>
        <div
          ref={dropBefore}
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            gridRowStart: 1,
            gridRowEnd: -1,
            gridColumn: `canvas / span 1`,
            display: isDragActive && canDropBefore ? 'block' : 'none',
            width: '50%',
            height: '100%',
            left: 0,
          }}
        >
          <div
            style={{
              display: isOverBefore ? 'block' : 'none',
              position: 'absolute',
              left: -1,
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
            gridColumn: `canvas / span 1`,
            display: isDragActive && canDropAfter ? 'block' : 'none',
            width: '50%',
            height: '100%',
            right: 0,
          }}
        >
          <div
            style={{
              display: isOverAfter ? 'block' : 'none',
              position: 'absolute',
              right: -1,
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

export const GridColumnResizeHandle = React.memo(
  (props: {
    columnIndex: number
    columnWidth: number
    setColumnWidth: (columnIndex: number, newWidth: number) => void
  }) => {
    const { columnIndex: columnIndexProp, columnWidth, setColumnWidth } = props

    const columnIndex = wrapAroundColIndex(columnIndexProp)
    const columnWidthRef = usePropControlledRef_DANGEROUS(columnWidth)
    const [isResizing, setIsResizing] = React.useState(false)

    const leftSideOfGrid = columnIndex > -1

    const handleResizeMouseDown = React.useCallback(
      (mouseDownEvent: React.MouseEvent<HTMLDivElement, MouseEvent>) => {
        const startingColumnWidth = columnWidthRef.current
        setIsResizing(true)

        const onMouseMove = (mouseMoveEvent: MouseEvent) => {
          const mouseDelta = mouseMoveEvent.clientX - mouseDownEvent.clientX
          const negateDelta = leftSideOfGrid ? 1 : -1
          setColumnWidth(columnIndex, startingColumnWidth + mouseDelta * negateDelta)
        }
        const onMouseUp = () => {
          setIsResizing(false)
          window.removeEventListener('mousemove', onMouseMove, { capture: true })
          window.removeEventListener('mouseup', onMouseUp, { capture: true })
        }
        window.addEventListener('mousemove', onMouseMove, { capture: true })
        window.addEventListener('mouseup', onMouseUp, { capture: true })
      },
      [leftSideOfGrid, columnIndex, columnWidthRef, setColumnWidth],
    )

    return (
      <>
        <div
          onMouseDown={handleResizeMouseDown}
          style={{
            zIndex: 1, // I feel really bad about this, but _something_ is over this draggable div if there are multiple panels in a column
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
            userSelect: isResizing ? 'none' : 'auto',
          }}
        />
        {
          // this is a mouse catcher div only visible during a resize, to prevent VSCode from capturing our mouse, and to prevent the flickering of the horizontal resize cursor
          when(
            isResizing,
            <div
              style={{
                pointerEvents: 'initial',
                height: '100%',
                width: '100%',
                backgroundColor: 'rgba(0,0,0,0)',
                opacity: 0,
                position: 'fixed',
                zIndex: 9999,
                top: '0',
                left: '0',
                bottom: '0',
                right: '0',
                cursor: CSSCursor.ResizeEW,
                userSelect: 'none',
              }}
            />,
          )
        }
      </>
    )
  },
)
