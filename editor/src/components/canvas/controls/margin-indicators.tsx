import React from 'react'
import { Sides } from 'utopia-api/core'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const MarginIndicators = React.memo(() => {
  const isInteractionActive = useEditorState(
    (store) => store.editor.canvas.interactionSession != null,
    'MarginIndicators isInteractionActive',
  )
  const scale = useEditorState((store) => store.editor.canvas.scale, 'MarginIndicators scale')
  const framesAndMargins = useEditorState((store) => {
    return mapDropNulls((path) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)
      if (frame != null) {
        return {
          margin: MetadataUtils.getElementMargin(path, store.editor.jsxMetadata),
          frame: frame,
        }
      } else {
        return null
      }
    }, store.editor.selectedViews)
  }, 'MarginIndicators margin')

  if (isInteractionActive) {
    return null
  }
  return (
    <>
      {framesAndMargins.map((frameInfo, i) => (
        <MarginIndicator key={i} margin={frameInfo.margin} frame={frameInfo.frame} scale={scale} />
      ))}
    </>
  )
})

interface MarginIndicatorProps {
  margin: Partial<Sides> | null
  frame: CanvasRectangle
  scale: number
}

export const MarginIndicator = React.memo((props: MarginIndicatorProps) => {
  const colorTheme = useColorTheme()

  if (props.margin == null) {
    return null
  } else {
    const leftElement =
      props.margin.left != null && props.margin.left !== 0 ? (
        <div
          key='marginLeft'
          style={{
            background: colorTheme.canvasLayoutFillTranslucent.value,
            fontWeight: 500,
            textAlign: 'center',
            overflow: 'visible',
            color: colorTheme.canvasLayoutForeground.value,
            position: 'absolute',
            left: props.frame.x - props.margin.left,
            top: props.frame.y - (props.margin.top ?? 0),
            width: props.margin.left,
            height: props.frame.height + (props.margin.top ?? 0) + (props.margin.bottom ?? 0),
            lineHeight:
              props.frame.height + (props.margin.top ?? 0) + (props.margin.bottom ?? 0) + 'px',
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
          style={{
            fontWeight: 500,
            overflow: 'visible',
            background: colorTheme.canvasLayoutFillTranslucent.value,
            color: colorTheme.canvasLayoutForeground.value,
            position: 'absolute',
            left: props.frame.x,
            top: props.frame.y - props.margin.top,
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
          style={{
            fontWeight: 500,
            overflow: 'visible',
            background: colorTheme.canvasLayoutFillTranslucent.value,
            color: colorTheme.canvasLayoutForeground.value,
            position: 'absolute',
            left: props.frame.x + props.frame.width,
            top: props.frame.y - (props.margin.top ?? 0),
            width: props.margin.right,
            height: props.frame.height + (props.margin.top ?? 0) + (props.margin.bottom ?? 0),
            lineHeight:
              props.frame.height + (props.margin.top ?? 0) + (props.margin.bottom ?? 0) + 'px',
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
          style={{
            fontWeight: 500,
            overflow: 'visible',
            background: colorTheme.canvasLayoutFillTranslucent.value,
            color: colorTheme.canvasLayoutForeground.value,
            position: 'absolute',
            left: props.frame.x,
            top: props.frame.y + props.frame.height,
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
          style={{
            background: 'transparent',
            border: colorTheme.canvasLayoutStroke.o(20).value,
            position: 'absolute',
            pointerEvents: 'none',
            left: props.frame.x - (props.margin.left ?? 0),
            top: props.frame.y - (props.margin.top ?? 0),
            width: props.frame.width + (props.margin.left ?? 0) + (props.margin.right ?? 0),
            height: props.frame.height + (props.margin.top ?? 0) + (props.margin.bottom ?? 0),
          }}
        />
      )

    return (
      <CanvasOffsetWrapper>
        {[leftElement, topElement, rightElement, bottomElement, outerDiv]}
      </CanvasOffsetWrapper>
    )
  }
})
