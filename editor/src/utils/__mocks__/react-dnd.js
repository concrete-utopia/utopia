/* eslint-disable @typescript-eslint/no-empty-function */

export function useDrop(specArg, deps) {
  return [{ isDragging: false }, () => {}]
}

export function useDrag(specArg, deps) {
  return [{ isOver: false }, () => {}]
}

export function DndProvider(props) {
  return props.children
}
