import { Size } from '../../../core/shared/math-utils'
import { UtopiaTheme } from '../../../uuiui'
import { isFeatureEnabled } from '../../../utils/feature-switches'

export function calculateExtraSizeForZeroSizedElement(
  size: Size,
): {
  extraWidth: number
  extraHeight: number
  showingInvisibleElement: boolean
  borderRadius: number
  dimension: 'point' | 'horizontal' | 'vertical' | 'n/a'
} {
  if (isFeatureEnabled('invisible_element_controls')) {
    const extraWidth = size.width === 0 ? UtopiaTheme.invisibleIndicatorSize : 0
    const extraHeight = size.height === 0 ? UtopiaTheme.invisibleIndicatorSize : 0
    const showingInvisibleElement = extraWidth !== 0 || extraHeight !== 0
    const borderRadius = showingInvisibleElement ? UtopiaTheme.invisibleIndicatorSize / 2 : 0
    const dimension = showingInvisibleElement
      ? size.width === 0
        ? size.height === 0
          ? 'point'
          : 'vertical'
        : 'horizontal'
      : 'n/a'

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
