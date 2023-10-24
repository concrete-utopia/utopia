import type { FlexDirection } from '../../components/inspector/common/css-utils'
import type { FlexAlignment, FlexJustifyContent } from '../../components/inspector/inspector-common'
import { assertNever } from '../shared/utils'

export function isReversedFlexDirection(flexDirection: FlexDirection): boolean {
  switch (flexDirection) {
    case 'row':
    case 'column':
      return false
    case 'row-reverse':
    case 'column-reverse':
      return true
    default:
      assertNever(flexDirection)
  }
}

export function flipFlexDirection(flexDirection: FlexDirection): FlexDirection {
  switch (flexDirection) {
    case 'row':
      return 'column'
    case 'row-reverse':
      return 'column-reverse'
    case 'column':
      return 'row'
    case 'column-reverse':
      return 'row-reverse'
    default:
      assertNever(flexDirection)
  }
}

export function reverseFlexDirection(flexDirection: FlexDirection): FlexDirection {
  switch (flexDirection) {
    case 'row':
      return 'row-reverse'
    case 'row-reverse':
      return 'row'
    case 'column':
      return 'column-reverse'
    case 'column-reverse':
      return 'column'
    default:
      assertNever(flexDirection)
  }
}

export function reverseFlexAlignment(flexAlignment: FlexAlignment): FlexAlignment {
  switch (flexAlignment) {
    case 'auto':
    case 'center':
    case 'stretch':
      return flexAlignment
    case 'flex-start':
      return 'flex-end'
    case 'flex-end':
      return 'flex-start'
    default:
      assertNever(flexAlignment)
  }
}

export function reverseJustifyContent(justifyContent: FlexJustifyContent): FlexJustifyContent {
  switch (justifyContent) {
    case 'center':
    case 'space-around':
    case 'space-evenly':
    case 'space-between':
      return justifyContent
    case 'flex-start':
      return 'flex-end'
    case 'flex-end':
      return 'flex-start'
    default:
      assertNever(justifyContent)
  }
}
