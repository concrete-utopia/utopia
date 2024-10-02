import type { EditorAction } from '../components/editor/action-types'

export type MouseHandler = (event: MouseEvent) => Array<EditorAction>

let mouseUpHandlers: Array<MouseHandler> = []

export function addMouseUpHandler(handler: MouseHandler): void {
  mouseUpHandlers.push(handler)
}

export function removeMouseUpHandler(handler: MouseHandler): void {
  mouseUpHandlers = mouseUpHandlers.filter((mouseUpHandler) => mouseUpHandler !== handler)
}

export function handleGlobalMouseUp(event: MouseEvent): Array<EditorAction> {
  let result: Array<EditorAction> = []
  for (const handler of mouseUpHandlers) {
    result.push(...handler(event))
  }
  return result
}
