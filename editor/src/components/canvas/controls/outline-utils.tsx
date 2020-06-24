import { Size } from '../../../core/shared/math-utils'
import { UtopiaTheme } from '../../../uuiui'
import { isFeatureEnabled } from '../../../utils/feature-switches'

function getDimension(size: Size): 'point' | 'horizontal' | 'vertical' | 'n/a' {
  if (size.width === 0 && size.height === 0) {
    return 'point'
  }
  if (size.width === 0) {
    return 'vertical'
  }
  if (size.height === 0) {
    return 'horizontal'
  }
  return 'n/a'
}

export function calculateExtraSizeForZeroSizedElement(
  size: Size,
): {
  extraWidth: number
  extraHeight: number
  showingInvisibleElement: boolean
  borderRadius: number
  dimension: 'point' | 'horizontal' | 'vertical' | 'n/a'
} {
  if (isFeatureEnabled('Invisible Element Controls')) {
    const extraWidth = size.width === 0 ? UtopiaTheme.invisibleIndicatorSize : 0
    const extraHeight = size.height === 0 ? UtopiaTheme.invisibleIndicatorSize : 0
    const showingInvisibleElement = extraWidth !== 0 || extraHeight !== 0
    const borderRadius = showingInvisibleElement ? UtopiaTheme.invisibleIndicatorSize / 2 : 0
    const dimension = getDimension(size)

    return {
      extraWidth,
      extraHeight,
      showingInvisibleElement,
      borderRadius,
      dimension,
    }
  } else {
    return {
      extraWidth: 0,
      extraHeight: 0,
      showingInvisibleElement: false,
      borderRadius: 0,
      dimension: 'n/a',
    }
  }
}
