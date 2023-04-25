import { getDisplayName } from './canvas-react-utils'

type FiberNodeThingy = { type: any; return?: FiberNodeThingy }
type ElementThingy = { type: any; _owner?: FiberNodeThingy }

function getNamedPathInner(element: FiberNodeThingy | undefined): string {
  if (element == null) {
    return ''
  }
  return `${getNamedPathInner(element.return)}/${getDisplayName(element.type)}`
}

export function getNamedPath(element: ElementThingy | undefined): string {
  if (element == null) {
    return ''
  }
  return `${getNamedPathInner(element._owner)}/${getDisplayName(element.type)}`
}
