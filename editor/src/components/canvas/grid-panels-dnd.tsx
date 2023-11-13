import type { ConnectDragPreview, ConnectDragSource, ConnectDropTarget } from 'react-dnd'
import { useDrag, useDragLayer, useDrop } from 'react-dnd'
import { magnitude, windowPoint } from '../../core/shared/math-utils'
import type { StoredPanel } from './stored-layout'

const FloatingPanelTitleBarType = 'floating-panel-title-bar'

type FloatingPanelDragItem = { draggedPanel: StoredPanel }

export function useGridPanelDraggable(draggedPanel: StoredPanel): {
  drag: ConnectDragSource
  dragPreview: ConnectDragPreview
} {
  const [{ isDragging }, drag, dragPreview] = useDrag(
    () => ({
      // "type" is required. It is used by the "accept" specification of drop targets.
      type: FloatingPanelTitleBarType,
      item: { draggedPanel: draggedPanel } as FloatingPanelDragItem,
      // The collect function utilizes a "monitor" instance (see https://react-dnd.github.io/react-dnd/docs/overview/ for what this is)
      // to pull important pieces of state from the DnD system.
      collect: (monitor) => ({
        isDragging: monitor.isDragging(),
      }),
    }),
    [draggedPanel],
  )

  return { drag, dragPreview }
}

export function useGridPanelDropArea(onDrop: (itemToMove: StoredPanel) => void): {
  drop: ConnectDropTarget
  isOver: boolean
} {
  const [{ isOver }, drop] = useDrop(
    () => ({
      // The type (or types) to accept - strings or symbols
      accept: FloatingPanelTitleBarType,
      drop: (droppedItem: FloatingPanelDragItem) => onDrop(droppedItem.draggedPanel),
      // Props to collect
      collect: (monitor) => ({
        isOver: monitor.isOver(),
      }),
    }),
    [onDrop],
  )

  return { drop, isOver }
}

const MinDragThreshold = 3

export function useGridPanelDragInfo(): {
  isDragActive: boolean
  draggedPanel: StoredPanel | undefined
} {
  const { isDragActive, draggedPanel } = useDragLayer((monitor) => {
    const dragVector = monitor.getDifferenceFromInitialOffset()
    return {
      isDragActive:
        monitor.isDragging() &&
        monitor.getItemType() === FloatingPanelTitleBarType &&
        dragVector != null &&
        magnitude(windowPoint(dragVector)) > MinDragThreshold,
      draggedPanel: monitor.getItem<FloatingPanelDragItem | null>()?.draggedPanel,
    }
  })

  return { isDragActive, draggedPanel }
}
