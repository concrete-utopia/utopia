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
} {
  if (isFeatureEnabled('invisible_element_controls')) {
    const extraWidth = size.width === 0 ? UtopiaTheme.invisibleIndicatorSize : 0
    const extraHeight = size.height === 0 ? UtopiaTheme.invisibleIndicatorSize : 0
    const showingInvisibleElement = extraWidth !== 0 || extraHeight !== 0
    const borderRadius = showingInvisibleElement ? UtopiaTheme.invisibleIndicatorSize / 2 : 0

    return {
      extraWidth,
      extraHeight,
      showingInvisibleElement,
      borderRadius,
    }
  } else {
    return {
      extraWidth: 0,
      extraHeight: 0,
      showingInvisibleElement: false,
      borderRadius: 0,
    }
  }
}
