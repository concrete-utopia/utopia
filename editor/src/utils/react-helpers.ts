import { getDisplayName } from './canvas-react-utils'

type ElementThingy = { type: any; _owner?: ElementThingy; return?: ElementThingy; props?: any }

export function getNamedPath(element: ElementThingy | undefined, depth: number = 3): string {
  if (depth === 0 || element == null) {
    return ''
  }

  const dataTestId = 'props' in element ? element.props['data-testid'] : null
  const dataTestIdString = dataTestId == null ? '' : `:data-testid='${dataTestId}'`

  if (element._owner == null) {
    return `${getNamedPath(element.return, depth - 1)}/${getDisplayName(
      element.type,
    )}${dataTestIdString}`
  } else {
    return `${getNamedPath(element._owner, depth)}/${getDisplayName(
      element.type,
    )}${dataTestIdString}`
  }
}
