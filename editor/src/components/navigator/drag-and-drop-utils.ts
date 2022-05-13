import { DragSourceSpec, DropTargetSpec } from 'react-dnd'
import { NavigatorItemDragAndDropWrapperProps } from './navigator-item/navigator-item-dnd-container'

export interface CollectResults {
  connectDropTarget: any
  connectDragSource: any
  connectDragPreview: any
  isDragging: boolean
  isOver: boolean
}

export function connectDropTarget(connect: any, monitor: any) {
  return {
    connectDropTarget: connect.dropTarget(),
    isOver: monitor.isOver(),
  }
}

export function connectDragSource(connect: any, monitor: any) {
  return {
    connectDragSource: connect.dragSource(),
    connectDragPreview: connect.dragPreview(),
    isDragging: monitor.isDragging(),
  }
}

type Spec = DragSourceSpec<
  NavigatorItemDragAndDropWrapperProps,
  NavigatorItemDragAndDropWrapperProps
>

export const cardSource = (onDragStart: Spec['beginDrag'], onDragEnd: Spec['endDrag']): Spec => {
  return {
    beginDrag: onDragStart,
    endDrag: onDragEnd,
  }
}

export const onDragActions = (
  onDrop: DropTargetSpec<NavigatorItemDragAndDropWrapperProps>['drop'],
  onHover: DropTargetSpec<NavigatorItemDragAndDropWrapperProps>['hover'],
): DropTargetSpec<NavigatorItemDragAndDropWrapperProps> => {
  return {
    drop: onDrop,
    hover: onHover,
  }
}

export function isCursorInTopArea(
  rectangle: ClientRect,
  cursorY: number,
  numberOfAreasToCut: number,
): boolean {
  return rectangle.top + rectangle.height / numberOfAreasToCut > cursorY
}

export function isCursorInBottomArea(
  rectangle: ClientRect,
  cursorY: number,
  numberOfAreasToCut: number,
): boolean {
  return rectangle.bottom - rectangle.height / numberOfAreasToCut < cursorY
}
