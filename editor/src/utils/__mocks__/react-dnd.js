/* eslint-disable @typescript-eslint/no-empty-function */

export function useDrop(specArg, deps) {
  return [{ isDragging: false }, () => {}]
}

export function useDrag(specArg, deps) {
  return [{ isOver: false }, () => {}]
}

export function useDragLayer(specArg, deps) {
  return { item: null, initialOffset: { x: 0, y: 0 }, difference: { x: 0, y: 0 } }
}

export function DndProvider(props) {
  return props.children
}
