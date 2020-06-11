import * as React from 'react'
import { Sides } from 'utopia-api'
import { colorTheme } from 'uuiui'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'

interface PaddingControlsProps {
  padding: Partial<Sides> | null
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
}

export const PaddingControls = (props: PaddingControlsProps) => {
  if (props.padding == null) {
    return <></>
  } else {
    const leftElement =
      props.padding.left != null && props.padding.left !== 0 ? (
        <div
          className=' roleYogaPaddingControl f10  fw5 tc overflow-visible'
          style={{
            background: colorTheme.paddingFillTranslucent.value,
            color: colorTheme.paddingForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x,
            top: props.frame.y + props.canvasOffset.y,
            width: props.padding.left,
            height: props.frame.height,
            lineHeight: props.frame.height + 'px',
          }}
        >
          {props.padding.left}
        </div>
      ) : null
    const topElement =
      props.padding.top != null && props.padding.top !== 0 ? (
        <div
          className=' f10  fw4 tc overflow-visible'
          style={{
            background: colorTheme.paddingFillTranslucent.value,
            color: colorTheme.paddingForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x + (props.padding.left || 0),
            top: props.frame.y + props.canvasOffset.y,
            width: props.frame.width - (props.padding.left || 0) - (props.padding.right || 0),
            height: props.padding.top,
            lineHeight: props.padding.top + 'px',
          }}
        >
          {props.padding.top}
        </div>
      ) : null
    const rightElement =
      props.padding.right != null && props.padding.right !== 0 ? (
        <div
          className=' f10  fw4 tc overflow-visible'
          style={{
            background: colorTheme.paddingFillTranslucent.value,
            color: colorTheme.paddingForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x + props.frame.width - props.padding.right,
            top: props.frame.y + props.canvasOffset.y,
            width: props.padding.right,
            height: props.frame.height,
            lineHeight: props.frame.height + 'px',
          }}
        >
          {props.padding.right}
        </div>
      ) : null
    const bottomElement =
      props.padding.bottom != null && props.padding.bottom !== 0 ? (
        <div
          className=' f10  fw4 tc overflow-visible'
          style={{
            background: colorTheme.paddingFillTranslucent.value,
            color: colorTheme.paddingForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x + (props.padding.left || 0),
            top: props.frame.y + props.canvasOffset.y + props.frame.height - props.padding.bottom,
            width: props.frame.width - (props.padding.left || 0) - (props.padding.right || 0),
            height: props.padding.bottom,
            lineHeight: props.padding.bottom + 'px',
          }}
        >
          {props.padding.bottom}
        </div>
      ) : null
    const innerDiv =
      leftElement == null &&
      topElement == null &&
      rightElement == null &&
      bottomElement == null ? null : (
        <div
          className='ba bgtransparent'
          style={{
            position: 'absolute',
            pointerEvents: 'none',
            left: props.frame.x + props.canvasOffset.x + (props.padding.left || 0),
            top: props.frame.y + props.canvasOffset.y + (props.padding.top || 0),
            width: props.frame.width - (props.padding.left || 0) - (props.padding.right || 0),
            height: props.frame.height - (props.padding.top || 0) - (props.padding.bottom || 0),
          }}
        />
      )
    return <>{[leftElement, topElement, rightElement, bottomElement, innerDiv]}</>
  }
}
