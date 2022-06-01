import React from 'react'
import { Sides } from 'utopia-api/core'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { CanvasRectangle, CanvasPoint } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const PaddingControls = React.memo(() => {
  const isInteractionActive = useEditorState(
    (store) => store.editor.canvas.interactionSession != null,
    'MarginControls isInteractionActive',
  )
  const scale = useEditorState((store) => store.editor.canvas.scale, 'PaddingControls scale')
  const framesAndpaddings = useEditorState((store) => {
    return mapDropNulls((path) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)
      if (frame != null) {
        return {
          padding: MetadataUtils.getElementPadding(path, store.editor.jsxMetadata),
          frame: frame,
        }
      } else {
        return null
      }
    }, store.editor.selectedViews)
  }, 'PaddingControls padding')

  if (isInteractionActive) {
    return null
  }
  return (
    <>
      {framesAndpaddings.map((frameInfo, i) => (
        <PaddingControl key={i} padding={frameInfo.padding} frame={frameInfo.frame} scale={scale} />
      ))}
    </>
  )
})

interface PaddingControlProps {
  padding: Partial<Sides> | null
  frame: CanvasRectangle
  scale: number
}

export const PaddingControl = React.memo((props: PaddingControlProps) => {
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
            left: props.frame.x,
            top: props.frame.y,
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
            left: props.frame.x + (props.padding.left ?? 0),
            top: props.frame.y,
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
            left: props.frame.x + props.frame.width - props.padding.right,
            top: props.frame.y,
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
            left: props.frame.x + (props.padding.left ?? 0),
            top: props.frame.y + props.frame.height - props.padding.bottom,
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
            left: props.frame.x + (props.padding.left ?? 0),
            top: props.frame.y + (props.padding.top ?? 0),
            width: props.frame.width - (props.padding.left ?? 0) - (props.padding.right ?? 0),
            height: props.frame.height - (props.padding.top ?? 0) - (props.padding.bottom ?? 0),
          }}
        />
      )
    return (
      <CanvasOffsetWrapper>
        {[leftElement, topElement, rightElement, bottomElement, innerDiv]}
      </CanvasOffsetWrapper>
    )
  }
})
