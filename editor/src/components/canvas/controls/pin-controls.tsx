import * as React from 'react'
import * as TP from '../../../core/shared/template-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { OutlineControlsProps } from './outline-control'
import { colorTheme } from '../../../uuiui'

interface PinOutlineProps {
  key: string
  size: number
  value: string
  isHorizontalLine: boolean
  top: number
  left: number
}

const PinOutline = (props: PinOutlineProps): JSX.Element => {
  return (
    <>
      <div
        key={props.key}
        style={{
          position: 'absolute',
          backgroundColor: colorTheme.brandPurple.value,
          top: props.top,
          left: props.left,
          width: props.isHorizontalLine ? props.size : 1,
          height: props.isHorizontalLine ? 1 : props.size,
          display: 'flex',
          justifyContent: 'center',
          flexDirection: props.isHorizontalLine ? 'row' : 'column',
        }}
      >
        <div
          style={{
            color: colorTheme.brandPurple.value,
            padding: 2,
          }}
        >
          {props.value}
        </div>
      </div>
      <div
        style={{
          position: 'absolute',
          backgroundColor: colorTheme.brandPurple.value,
          top: props.isHorizontalLine ? props.top - 5 : props.top,
          left: props.isHorizontalLine ? props.left : props.left - 5,
          width: props.isHorizontalLine ? 1 : 10,
          height: props.isHorizontalLine ? 10 : 1,
        }}
      />
      <div
        style={{
          position: 'absolute',
          backgroundColor: colorTheme.brandPurple.value,
          top: props.isHorizontalLine ? props.top - 5 : props.top + props.size,
          left: props.isHorizontalLine ? props.left + props.size : props.left - 5,
          width: props.isHorizontalLine ? 1 : 10,
          height: props.isHorizontalLine ? 10 : 1,
        }}
      />
    </>
  )
}

export const PinControls = (props: OutlineControlsProps): JSX.Element | null => {
  return (
    <>
      {props.selectedViews.map((view) => {
        const containingBlockParent = MetadataUtils.findContainingBlock(
          props.componentMetadata,
          view,
        )
        const containingRectangle = MetadataUtils.getElementByTemplatePathMaybe(
          props.componentMetadata,
          containingBlockParent,
        )?.globalFrame

        const frame = MetadataUtils.getFrameInCanvasCoords(view, props.componentMetadata)
        const element = MetadataUtils.getElementByTemplatePathMaybe(props.componentMetadata, view)
        if (
          element != null &&
          frame != null &&
          containingRectangle != null &&
          element.props.style != null &&
          element.props.style.position === 'absolute'
        ) {
          const style = element.props.style
          let pins: PinOutlineProps[] = []
          if (style.left != null) {
            pins.push({
              key: 'left',
              isHorizontalLine: true,
              value: style.left,
              size: frame.x - containingRectangle.x,
              top: frame.y + frame.height / 2 + props.canvasOffset.y,
              left: containingRectangle.x + props.canvasOffset.x,
            })
          }
          if (style.right != null) {
            pins.push({
              key: 'right',
              isHorizontalLine: true,
              value: style.right,
              size: containingRectangle.x + containingRectangle.width - (frame.x + frame.width),
              top: frame.y + frame.height / 2 + props.canvasOffset.y,
              left: frame.width + frame.x + props.canvasOffset.x,
            })
          }
          if (style.top != null) {
            pins.push({
              key: 'top',
              isHorizontalLine: false,
              value: style.top,
              size: frame.y - containingRectangle.y,
              top: containingRectangle.y + props.canvasOffset.y,
              left: frame.x + frame.width / 2 + props.canvasOffset.x,
            })
          }
          if (style.bottom != null) {
            pins.push({
              key: 'bottom',
              isHorizontalLine: false,
              value: style.bottom,
              size: containingRectangle.y + containingRectangle.height - (frame.y + frame.height),
              top: frame.height + frame.y + props.canvasOffset.y,
              left: frame.x + frame.width / 2 + props.canvasOffset.x,
            })
          }
          if (style.width != null) {
            pins.push({
              key: 'width',
              isHorizontalLine: true,
              value: style.width,
              size: frame.width,
              top: frame.y - 5 + props.canvasOffset.y,
              left: frame.x + props.canvasOffset.x,
            })
          }
          if (style.height != null) {
            pins.push({
              key: 'height',
              isHorizontalLine: false,
              value: style.height,
              size: frame.height,
              top: frame.y + props.canvasOffset.y,
              left: frame.x - 5 + props.canvasOffset.x,
            })
          }
          return (
            <div key={TP.toString(view)}>
              {pins.map((pin) => (
                <PinOutline {...pin} key={pin.key} />
              ))}
            </div>
          )
        } else {
          return null
        }
      })}
    </>
  )
}
