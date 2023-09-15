import type { ConnectDragPreview, ConnectDragSource, ConnectDropTarget } from 'react-dnd'
import { useDrag, useDrop } from 'react-dnd'
import type { LayoutUpdate, StoredPanel } from './floating-panels'

const FloatingPanelTitleBarType = 'floating-panel-title-bar'

type FloatingPanelDragItem = { draggedPanel: StoredPanel }

export function useFloatingPanelDraggable(draggedPanel: StoredPanel): {
  drag: ConnectDragSource
  dragPreview: ConnectDragPreview
} {
  const [{ isDragging }, drag, dragPreview] = useDrag(() => ({
    // "type" is required. It is used by the "accept" specification of drop targets.
    type: FloatingPanelTitleBarType,
    item: { draggedPanel: draggedPanel } as FloatingPanelDragItem,
    // The collect function utilizes a "monitor" instance (see the Overview for what this is)
    // to pull important pieces of state from the DnD system.
    collect: (monitor) => ({
      isDragging: monitor.isDragging(),
    }),
  }))

  return { drag, dragPreview }
}

export function useFloatingPanelDropArea(
  columnIndex: number,
  indexInColumn: number,
  onDrop: (itemToMove: StoredPanel, newPosition: LayoutUpdate) => void,
): {
  drop: ConnectDropTarget
} {
  const [{ canDrop, isOver }, drop] = useDrop(() => ({
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
  }))

  return { drop }
}
