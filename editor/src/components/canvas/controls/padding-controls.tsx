import React from 'react'
import type { Sides } from 'utopia-api/core'
import type { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'

interface PaddingControlsProps {
  padding: Partial<Sides> | null
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
}

export const PaddingControls = React.memo((props: PaddingControlsProps) => {
  const colorTheme = useColorTheme()
  if (props.padding == null) {
    return null
  } else {
    const leftElement =
      props.padding.left != null && props.padding.left !== 0 ? (
        <div
          key='padding-left'
          className=' roleFlexPaddingControl'
          style={{
            fontSize: 10,
            fontWeight: 5,
            textAlign: 'center',
            overflow: 'visible',
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
          key='padding-top'
          style={{
            fontSize: 10,
            fontWeight: 500,
            textAlign: 'center',
            overflow: 'visible',
            background: colorTheme.paddingFillTranslucent.value,
            color: colorTheme.paddingForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x + (props.padding.left ?? 0),
            top: props.frame.y + props.canvasOffset.y,
            width: props.frame.width - (props.padding.left ?? 0) - (props.padding.right ?? 0),
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
          key='padding-right'
          style={{
            fontSize: 10,
            fontWeight: 500,
            textAlign: 'center',
            overflow: 'visible',
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
          key='padding-bottom'
          style={{
            fontSize: 10,
            fontWeight: 500,
            textAlign: 'center',
            overflow: 'visible',
            background: colorTheme.paddingFillTranslucent.value,
            color: colorTheme.paddingForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x + (props.padding.left ?? 0),
            top: props.frame.y + props.canvasOffset.y + props.frame.height - props.padding.bottom,
            width: props.frame.width - (props.padding.left ?? 0) - (props.padding.right ?? 0),
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
          key='padding-inner-div'
          style={{
            background: 'transparent',
            position: 'absolute',
            pointerEvents: 'none',
            left: props.frame.x + props.canvasOffset.x + (props.padding.left ?? 0),
            top: props.frame.y + props.canvasOffset.y + (props.padding.top ?? 0),
            width: props.frame.width - (props.padding.left ?? 0) - (props.padding.right ?? 0),
            height: props.frame.height - (props.padding.top ?? 0) - (props.padding.bottom ?? 0),
          }}
        />
      )
    return <>{[leftElement, topElement, rightElement, bottomElement, innerDiv]}</>
  }
})
