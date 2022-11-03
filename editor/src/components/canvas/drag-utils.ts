import { CanvasVector } from '../../core/shared/math-utils'
import { assertNever } from '../../core/shared/utils'

export type SimpleFlexDirection = 'row' | 'column' | 'row-reverse' | 'column-reverse'

export function simpleFlexDirectionFromString(raw: string): SimpleFlexDirection | null {
  switch (raw) {
    case 'row':
      return 'row'
    case 'column':
      return 'column'
    case 'row-reverse':
      return 'row-reverse'
    case 'column-reverse':
      return 'column-reverse'
    default:
      return null
  }
}

export function updateGapValue(
  orientation: SimpleFlexDirection,
  originalValue: number,
  delta: CanvasVector,
): number {
  switch (orientation) {
    case 'row':
      return originalValue + delta.x
    case 'row-reverse':
      return originalValue - delta.x
    case 'column':
      return originalValue + delta.y
    case 'column-reverse':
      return originalValue - delta.y
    default:
      assertNever(orientation)
  }
}
