import type { ReactElement } from 'react'
import { createElement } from 'react'
import RU from '../utils/react-utils'
import type { LocalRectangle } from '../core/shared/math-utils'
import { colorTheme } from '../uuiui/styles/theme/utopia-theme'

const CornerOutlines = {
  render(
    key: string,
    rect: LocalRectangle,
    cornerLength: number,
    strokeWidth: number,
  ): ReactElement<any> {
    const stroke = colorTheme.canvasControlsCornerOutline.value
    const none = 'none'
    const minX = 0
    const minY = 0
    const maxX = rect.width
    const maxY = rect.height

    return RU.create(
      'svg',
      {
        key: key,
        style: {
          position: 'absolute',
          left: rect.x,
          top: rect.y,
          width: rect.width,
          height: rect.height,
        },
        x: rect.x,
        y: rect.y,
        width: rect.width,
        height: rect.height,
        viewBox: `0 0 ${rect.width} ${rect.height}`,
      },
      createElement('path', {
        d: `M ${minX} ${minY} L ${minX + cornerLength}, ${minY}`,
        fill: none,
        stroke: stroke,
        strokeWidth: strokeWidth,
      }),
      createElement('path', {
        d: `M ${minX} ${minY} L ${minX}, ${minY + cornerLength}`,
        fill: none,
        stroke: stroke,
        strokeWidth: strokeWidth,
      }),
      createElement('path', {
        d: `M ${maxX - cornerLength} ${minY} L ${maxX}, ${minY}`,
        fill: none,
        stroke: stroke,
        strokeWidth: strokeWidth,
      }),
      createElement('path', {
        d: `M ${maxX} ${minY} L ${maxX}, ${minY + cornerLength}`,
        fill: none,
        stroke: stroke,
        strokeWidth: strokeWidth,
      }),
      createElement('path', {
        d: `M ${minX} ${maxY - cornerLength} L ${minX}, ${maxY}`,
        fill: none,
        stroke: stroke,
        strokeWidth: strokeWidth,
      }),
      createElement('path', {
        d: `M ${minX} ${maxY} L ${minX + cornerLength}, ${maxY}`,
        fill: none,
        stroke: stroke,
        strokeWidth: strokeWidth,
      }),
      createElement('path', {
        d: `M ${maxX - cornerLength} ${maxY} L ${maxX}, ${maxY}`,
        fill: none,
        stroke: stroke,
        strokeWidth: strokeWidth,
      }),
      createElement('path', {
        d: `M ${maxX} ${maxY - cornerLength} L ${maxX}, ${maxY}`,
        fill: none,
        stroke: stroke,
        strokeWidth: strokeWidth,
      }),
    )
  },
}

export default CornerOutlines
