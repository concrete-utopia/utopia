import { getDisplayName } from './canvas-react-utils'

type FiberNodeThingy = { type: any; return?: FiberNodeThingy }
type ElementThingy = { type: any; _owner?: FiberNodeThingy }

function getNamedPathInner(element: FiberNodeThingy | undefined, depth: number): string {
  if (depth === 0 || element == null) {
    return ''
  }
  return `${getNamedPathInner(element.return, depth - 1)}/${getDisplayName(element.type)}`
}

export function getNamedPath(element: ElementThingy | undefined): string {
  if (element == null) {
    return ''
  }
  return `${getNamedPathInner(element._owner, 5)}/${getDisplayName(element.type)}`
}
