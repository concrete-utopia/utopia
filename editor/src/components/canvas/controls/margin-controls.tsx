import * as React from 'react'
import { Sides } from 'utopia-api'
import { colorTheme } from 'uuiui'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { betterReactMemo } from '../../../uuiui-deps'

interface MarginControlsProps {
  margin: Partial<Sides> | null
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
}

export const MarginControls = betterReactMemo('MarginControls', (props: MarginControlsProps) => {
  if (props.margin == null) {
    return null
  } else {
    const leftElement =
      props.margin.left != null && props.margin.left !== 0 ? (
        <div
          key='marginLeft'
          className='f10 fw5 tc overflow-visible'
          style={{
            background: colorTheme.canvasLayoutFillTranslucent.value,
            color: colorTheme.canvasLayoutForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x - props.margin.left,
            top: props.frame.y + props.canvasOffset.y - (props.margin.top || 0),
            width: props.margin.left,
            height: props.frame.height + (props.margin.top || 0) + (props.margin.bottom || 0),
            lineHeight:
              props.frame.height + (props.margin.top || 0) + (props.margin.bottom || 0) + 'px',
            fontSize: 8 / props.scale,
          }}
        >
          {props.margin.left}
        </div>
      ) : null
    const topElement =
      props.margin.top != null && props.margin.top !== 0 ? (
        <div
          key='marginTop'
          className=' f10  fw5 tc overflow-visible'
          style={{
            background: colorTheme.canvasLayoutFillTranslucent.value,
            color: colorTheme.canvasLayoutForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x,
            top: props.frame.y + props.canvasOffset.y - props.margin.top,
            width: props.frame.width,
            height: props.margin.top,
            lineHeight: props.margin.top + 'px',
            textAlign: 'center',
            fontSize: 8 / props.scale,
          }}
        >
          {props.margin.top}
        </div>
      ) : null
    const rightElement =
      props.margin.right != null && props.margin.right !== 0 ? (
        <div
          key='marginRight'
          className=' f10  fw5 tc overflow-visible'
          style={{
            background: colorTheme.canvasLayoutFillTranslucent.value,
            color: colorTheme.canvasLayoutForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x + props.frame.width,
            top: props.frame.y + props.canvasOffset.y - (props.margin.top || 0),
            width: props.margin.right,
            height: props.frame.height + (props.margin.top || 0) + (props.margin.bottom || 0),
            lineHeight:
              props.frame.height + (props.margin.top || 0) + (props.margin.bottom || 0) + 'px',
            textAlign: 'center',
            fontSize: 8 / props.scale,
          }}
        >
          {props.margin.right}
        </div>
      ) : null
    const bottomElement =
      props.margin.bottom != null && props.margin.bottom !== 0 ? (
        <div
          key='marginBottom'
          className='f10 fw5 tc overflow-visible'
          style={{
            background: colorTheme.canvasLayoutFillTranslucent.value,
            color: colorTheme.canvasLayoutForeground.value,
            position: 'absolute',
            left: props.frame.x + props.canvasOffset.x,
            top: props.frame.y + props.canvasOffset.y + props.frame.height,
            width: props.frame.width,
            height: props.margin.bottom,
            lineHeight: props.margin.bottom + 'px',
            textAlign: 'center',
            fontSize: 8 / props.scale,
          }}
        >
          {props.margin.bottom}
        </div>
      ) : null
    const outerDiv =
      leftElement == null &&
      topElement == null &&
      rightElement == null &&
      bottomElement == null ? null : (
        <div
          key='outerDiv'
          className='bgtransparent'
          style={{
            border: colorTheme.canvasLayoutStroke.o(20).value,
            position: 'absolute',
            pointerEvents: 'none',
            left: props.frame.x + props.canvasOffset.x - (props.margin.left || 0),
            top: props.frame.y + props.canvasOffset.y - (props.margin.top || 0),
            width: props.frame.width + (props.margin.left || 0) + (props.margin.right || 0),
            height: props.frame.height + (props.margin.top || 0) + (props.margin.bottom || 0),
          }}
        />
      )

    return <>{[leftElement, topElement, rightElement, bottomElement, outerDiv]}</>
  }
})
