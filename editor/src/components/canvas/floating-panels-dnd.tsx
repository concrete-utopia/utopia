import type { ConnectDragPreview, ConnectDragSource, ConnectDropTarget } from 'react-dnd'
import { useDrag, useDragLayer, useDrop } from 'react-dnd'
import type { LayoutUpdate, PanelName, StoredPanel } from './floating-panels'
import { magnitude, windowPoint } from '../../core/shared/math-utils'

const FloatingPanelTitleBarType = 'floating-panel-title-bar'

type FloatingPanelDragItem = { draggedPanel: StoredPanel }

export function useFloatingPanelDraggable(draggedPanel: StoredPanel): {
  drag: ConnectDragSource
  dragPreview: ConnectDragPreview
} {
  const [{ isDragging }, drag, dragPreview] = useDrag(
    () => ({
      // "type" is required. It is used by the "accept" specification of drop targets.
      type: FloatingPanelTitleBarType,
      item: { draggedPanel: draggedPanel } as FloatingPanelDragItem,
      // The collect function utilizes a "monitor" instance (see the Overview for what this is)
      // to pull important pieces of state from the DnD system.
      collect: (monitor) => ({
        isDragging: monitor.isDragging(),
      }),
    }),
    [draggedPanel],
  )

  return { drag, dragPreview }
}

export function useFloatingPanelDropArea(
  columnIndex: number,
  indexInColumn: number,
  onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void,
): {
  drop: ConnectDropTarget
  isOver: boolean
} {
  const [{ canDrop, isOver }, drop] = useDrop(
    () => ({
      // The type (or types) to accept - strings or symbols
      accept: FloatingPanelTitleBarType,
      drop: (droppedItem: FloatingPanelDragItem) =>
        onDrop(droppedItem.draggedPanel, {
          type: 'before-index',
          columnIndex: columnIndex,
          indexInColumn: indexInColumn,
        }),
      // Props to collect
      collect: (monitor) => ({
        isOver: monitor.isOver(),
        canDrop: monitor.canDrop(),
      }),
    }),
    [columnIndex, indexInColumn, onDrop],
  )

  return { drop, isOver }
}

const MinDragThreshold = 3

export function useFloatingPanelDragInfo(): {
  isDragActive: boolean
  draggedPanelName: PanelName | undefined
} {
  const { isDragActive, draggedPanelName } = useDragLayer((monitor) => {
    const dragVector = monitor.getDifferenceFromInitialOffset()
    return {
      isDragActive:
        monitor.isDragging() &&
        monitor.getItemType() === FloatingPanelTitleBarType &&
        dragVector != null &&
        magnitude(windowPoint(dragVector)) > MinDragThreshold,
      draggedPanelName: monitor.getItem<FloatingPanelDragItem | null>()?.draggedPanel.name,
    }
  })

  return { isDragActive, draggedPanelName }
}
