import React from 'react'
import { colorTheme } from '../../uuiui'
import { useGridPanelDragInfo, useGridPanelDropArea } from './grid-panels-dnd'
import type { LayoutUpdate, StoredPanel } from './grid-panels-state'
import { ExtraHorizontalDropTargetPadding, GridPanelHorizontalGapHalf } from './grid-panels-state'

export const ColumnDragTargets = React.memo(
  (props: {
    columnIndex: number
    canDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
    onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void
  }) => {
    const { columnIndex, onDrop, canDrop } = props
    const { isDragActive, draggedPanel } = useGridPanelDragInfo()

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
            gridColumn: `col ${columnIndex > -1 ? columnIndex + 1 : columnIndex} / span 1`,
            display: isDragActive && canDropBefore ? 'block' : 'none',
            width: 2 * ExtraHorizontalDropTargetPadding + 2 * GridPanelHorizontalGapHalf,
            height: '100%',
            left: -(ExtraHorizontalDropTargetPadding + 2 * GridPanelHorizontalGapHalf),
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
            gridColumn: `col ${columnIndex > -1 ? columnIndex + 1 : columnIndex} / span 1`,
            display: isDragActive && canDropAfter ? 'block' : 'none',
            width: 2 * ExtraHorizontalDropTargetPadding + 2 * GridPanelHorizontalGapHalf,
            height: '100%',
            right: -(ExtraHorizontalDropTargetPadding + 2 * GridPanelHorizontalGapHalf),
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
