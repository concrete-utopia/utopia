import * as React from 'react'
import * as TP from '../../../core/shared/template-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { OutlineControlsProps } from './outline-control'
import { colorTheme } from '../../../uuiui'
import { isJSXElement } from '../../../core/shared/element-template'
import { eitherToMaybe, isRight, right } from '../../../core/shared/either'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'

interface PinOutlineProps {
  key: string
  size: number
  value: string | number
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
          MetadataUtils.isPositionAbsolute(element) &&
          isRight(element.element) &&
          isJSXElement(element.element.value)
        ) {
          const attributes = element.element.value.props
          const left = eitherToMaybe(getLayoutProperty('PinnedLeft', right(attributes)))
          const top = eitherToMaybe(getLayoutProperty('PinnedTop', right(attributes)))
          const styleRight = eitherToMaybe(getLayoutProperty('PinnedRight', right(attributes)))
          const bottom = eitherToMaybe(getLayoutProperty('PinnedBottom', right(attributes)))
          const width = eitherToMaybe(getLayoutProperty('Width', right(attributes)))
          const height = eitherToMaybe(getLayoutProperty('Height', right(attributes)))
          const centerX = eitherToMaybe(getLayoutProperty('PinnedCenterX', right(attributes)))
          const centerY = eitherToMaybe(getLayoutProperty('PinnedCenterY', right(attributes)))
          let pins: PinOutlineProps[] = []
          if (left != null) {
            pins.push({
              key: 'left',
              isHorizontalLine: true,
              value: left,
              size: frame.x - containingRectangle.x,
              top: frame.y + frame.height / 2 + props.canvasOffset.y,
              left: containingRectangle.x + props.canvasOffset.x,
            })
          }
          if (styleRight != null) {
            pins.push({
              key: 'right',
              isHorizontalLine: true,
              value: styleRight,
              size: containingRectangle.x + containingRectangle.width - (frame.x + frame.width),
              top: frame.y + frame.height / 2 + props.canvasOffset.y,
              left: frame.width + frame.x + props.canvasOffset.x,
            })
          }
          if (top != null) {
            pins.push({
              key: 'top',
              isHorizontalLine: false,
              value: top,
              size: frame.y - containingRectangle.y,
              top: containingRectangle.y + props.canvasOffset.y,
              left: frame.x + frame.width / 2 + props.canvasOffset.x,
            })
          }
          if (bottom != null) {
            pins.push({
              key: 'bottom',
              isHorizontalLine: false,
              value: bottom,
              size: containingRectangle.y + containingRectangle.height - (frame.y + frame.height),
              top: frame.height + frame.y + props.canvasOffset.y,
              left: frame.x + frame.width / 2 + props.canvasOffset.x,
            })
          }
          if (width != null) {
            pins.push({
              key: 'width',
              isHorizontalLine: true,
              value: width,
              size: frame.width,
              top: frame.y - 10 + props.canvasOffset.y,
              left: frame.x + props.canvasOffset.x,
            })
          }
          if (height != null) {
            pins.push({
              key: 'height',
              isHorizontalLine: false,
              value: height,
              size: frame.height,
              top: frame.y + props.canvasOffset.y,
              left: frame.x - 10 + props.canvasOffset.x,
            })
          }
          if (centerX != null) {
            const frameCenter =
              containingRectangle.x + frame.x + frame.width / 2 + props.canvasOffset.x
            const containerCenter =
              containingRectangle.x + containingRectangle.width / 2 + props.canvasOffset.x
            pins.push({
              key: 'centerX',
              isHorizontalLine: true,
              value: centerX,
              size: Math.abs(frameCenter - containerCenter),
              top: containingRectangle.y + containingRectangle.height / 2 + props.canvasOffset.y,
              left: Math.min(frameCenter, containerCenter),
            })
          }
          if (centerY != null) {
            const frameCenter =
              containingRectangle.y + frame.y + frame.height / 2 + props.canvasOffset.y
            const containerCenter =
              containingRectangle.y + containingRectangle.height / 2 + props.canvasOffset.y
            pins.push({
              key: 'centerY',
              isHorizontalLine: false,
              value: centerY,
              size: Math.abs(frameCenter - containerCenter),
              top: Math.min(frameCenter, containerCenter),
              left: containingRectangle.x + containingRectangle.width / 2 + props.canvasOffset.x,
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
