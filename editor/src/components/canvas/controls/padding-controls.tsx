import React from 'react'
import { Sides } from 'utopia-api/core'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'

interface PaddingControlsProps {
  padding: Partial<Sides> | null
  frame: CanvasRectangle
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
            overflow: 'visible',
            background: colorTheme.paddingFillTranslucent.value,
            color: colorTheme.paddingForeground.value,
            position: 'absolute',
            left: `calc(${props.frame.x}px + var(--utopia-canvas-offset-x))`,
            top: `calc(${props.frame.y}px + var(--utopia-canvas-offset-y))`,
            width: props.padding.left,
            height: props.frame.height,
            textAlign: 'center',
            lineHeight: props.frame.height + 'px',
          }}
        >
          <span
            style={{
              display: 'inline-block',
              zoom: 'calc(1/var(--utopia-canvas-zoom))',
              transform: 'scale(calc(1/(var(--utopia-canvas-transform-scale))))',
            }}
          >
            {props.padding.left}
          </span>
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
            left: `calc(${
              props.frame.x + (props.padding.left ?? 0)
            }px + var(--utopia-canvas-offset-x))`,
            top: `calc(${props.frame.y}px + var(--utopia-canvas-offset-y))`,
            width: props.frame.width - (props.padding.left ?? 0) - (props.padding.right ?? 0),
            height: props.padding.top,
            lineHeight: props.padding.top + 'px',
          }}
        >
          <span
            style={{
              display: 'inline-block',
              zoom: 'calc(1/var(--utopia-canvas-zoom))',
              transform: 'scale(calc(1/(var(--utopia-canvas-transform-scale))))',
            }}
          >
            {props.padding.top}
          </span>
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
            left: `calc(${
              props.frame.x + props.frame.width - props.padding.right
            }px + var(--utopia-canvas-offset-x))`,
            top: `calc(${props.frame.y}px + var(--utopia-canvas-offset-y))`,
            width: props.padding.right,
            height: props.frame.height,
            lineHeight: props.frame.height + 'px',
          }}
        >
          <span
            style={{
              display: 'inline-block',
              zoom: 'calc(1/var(--utopia-canvas-zoom))',
              transform: 'scale(calc(1/(var(--utopia-canvas-transform-scale))))',
            }}
          >
            {props.padding.right}
          </span>
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
            left: `calc(${
              props.frame.x + (props.padding.left ?? 0)
            }px + var(--utopia-canvas-offset-x))`,
            top: `calc(${
              props.frame.y + props.frame.height - props.padding.bottom
            }px + var(--utopia-canvas-offset-y))`,
            width: props.frame.width - (props.padding.left ?? 0) - (props.padding.right ?? 0),
            height: props.padding.bottom,
            lineHeight: props.padding.bottom + 'px',
          }}
        >
          <span
            style={{
              display: 'inline-block',
              zoom: 'calc(1/var(--utopia-canvas-zoom))',
              transform: 'scale(calc(1/(var(--utopia-canvas-transform-scale))))',
            }}
          >
            {props.padding.bottom}
          </span>
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
            left: `calc(${
              props.frame.x + (props.padding.left ?? 0)
            }px + var(--utopia-canvas-offset-x))`,
            top: `calc(${
              props.frame.y + (props.padding.top ?? 0)
            }px + var(--utopia-canvas-offset-y))`,
            width: props.frame.width - (props.padding.left ?? 0) - (props.padding.right ?? 0),
            height: props.frame.height - (props.padding.top ?? 0) - (props.padding.bottom ?? 0),
          }}
        />
      )
    return <>{[leftElement, topElement, rightElement, bottomElement, innerDiv]}</>
  }
})
