import { getDisplayName } from './canvas-react-utils'

type ElementThingy = { type: any; _owner?: ElementThingy; return?: ElementThingy }

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
