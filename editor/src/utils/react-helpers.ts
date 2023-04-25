import { getDisplayName } from './canvas-react-utils'

type FiberNodeThingy = { type: any; return?: FiberNodeThingy }
type ElementThingy = { type: any; _owner?: FiberNodeThingy; return?: FiberNodeThingy }

function getNamedPathInner(element: FiberNodeThingy | undefined, depth: number): string {
  if (depth === 0 || element == null) {
    return ''
  }
  return `${getNamedPathInner(element.return, depth - 1)}/${getDisplayName(element.type)}`
}

export function getNamedPath(element: ElementThingy | undefined, depth: number = 3): string {
  if (depth === 0 || element == null) {
    return ''
  }
  if (element._owner == null) {
    return `${getNamedPath(element.return, depth - 1)}/${getDisplayName(element.type)}`
  } else {
    return `${getNamedPath(element._owner, depth)}/${getDisplayName(element.type)}`
  }
}
