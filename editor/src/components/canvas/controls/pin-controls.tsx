import * as React from 'react'
import * as TP from '../../../core/shared/template-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { OutlineControlsProps } from './outline-control'
import { colorTheme } from '../../../uuiui'
import { isJSXElement, jsxAttributeValue } from '../../../core/shared/element-template'
import { eitherToMaybe, isRight, right } from '../../../core/shared/either'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { isPercentPin } from 'utopia-api'
import {
  createLayoutPropertyPath,
  LayoutProp,
  StyleLayoutProp,
} from '../../../core/layout/layout-helpers-new'
import { useEditorState } from '../../editor/store/store-hook'
import { setProp_UNSAFE, unsetProperty } from '../../editor/actions/actions'
import { InstancePath } from '../../../core/shared/project-file-types'
import { isFeatureEnabled } from '../../../utils/feature-switches'

interface PinOutlineProps {
  key: string
  propName: LayoutProp | StyleLayoutProp
  size: number
  value: string | number
  isHorizontalLine: boolean
  top: number
  left: number
  onMouseDown: (
    propName: LayoutProp | StyleLayoutProp,
    event: React.MouseEvent<HTMLDivElement>,
  ) => void
}

const PinOutline = (props: PinOutlineProps): JSX.Element => {
  const borderStyle = isPercentPin(props.value) ? 'dotted' : 'solid'
  const canBeDragged =
    props.propName === 'PinnedLeft' ||
    props.propName === 'PinnedRight' ||
    props.propName === 'PinnedTop' ||
    props.propName === 'PinnedBottom'
  return (
    <>
      <div
        style={{
          position: 'absolute',
          top: props.top,
          left: props.left,
          width: props.isHorizontalLine ? props.size : 0,
          height: props.isHorizontalLine ? 0 : props.size,
          borderTop: props.isHorizontalLine
            ? `1px ${borderStyle} ${colorTheme.brandPurple.value}`
            : 'none',
          borderLeft: props.isHorizontalLine
            ? 'none'
            : `1px ${borderStyle} ${colorTheme.brandPurple.value}`,
        }}
      />
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
      <div
        style={{
          color: colorTheme.brandPurple.value,
          position: 'absolute',
          top: props.top,
          left: props.left,
          width: props.isHorizontalLine ? props.size : 0,
          height: props.isHorizontalLine ? 0 : props.size,
          display: 'flex',
          justifyContent: 'center',
          flexDirection: props.isHorizontalLine ? 'row' : 'column',
        }}
      >
        <div style={{ padding: 2 }}>{props.value}</div>
      </div>
      <div
        className='pin-control-click-area'
        onMouseDown={(e) => props.onMouseDown(props.propName, e)}
        style={{
          position: 'absolute',
          top: props.top,
          left: props.left,
          width: props.isHorizontalLine ? props.size : 5,
          height: props.isHorizontalLine ? 5 : props.size,
          cursor: isFeatureEnabled('Drag Pin Controls') && canBeDragged ? 'pointer' : undefined,
        }}
      />
    </>
  )
}

const PinControls = (props: OutlineControlsProps): JSX.Element | null => {
  const isDraggable = isFeatureEnabled('Drag Pin Controls')
  const target = props.selectedViews[0] as InstancePath
  const dispatch = useEditorState((state) => state.dispatch)
  const containingBlockParent = MetadataUtils.findContainingBlock(props.componentMetadata, target)
  const containingRectangle = MetadataUtils.getElementByTemplatePathMaybe(
    props.componentMetadata,
    containingBlockParent,
  )?.globalFrame

  const frame = MetadataUtils.getFrameInCanvasCoords(target, props.componentMetadata)
  const element = MetadataUtils.getElementByTemplatePathMaybe(props.componentMetadata, target)
  const [draggedProp, setDraggedProp] = React.useState<null | LayoutProp | StyleLayoutProp>(null)

  const jsxAttributes =
    element != null && isRight(element.element) && isJSXElement(element.element.value)
      ? element.element.value.props
      : null

  const onMouseMove = React.useCallback(
    (event: MouseEvent) => {
      if (
        isDraggable &&
        draggedProp != null &&
        frame != null &&
        containingRectangle != null &&
        jsxAttributes != null
      ) {
        const cursorPosition = props.windowToCanvasPosition(event).canvasPositionRaw
        if (draggedProp === 'PinnedLeft' || draggedProp === 'PinnedRight') {
          const hasLeft =
            eitherToMaybe(getLayoutProperty('PinnedLeft', right(jsxAttributes))) != null
          const hasRight =
            eitherToMaybe(getLayoutProperty('PinnedRight', right(jsxAttributes))) != null
          if (cursorPosition.x > frame.x + frame.width && hasLeft) {
            const newRight = containingRectangle.width - frame.x - frame.width
            dispatch(
              [
                unsetProperty(target, createLayoutPropertyPath('PinnedLeft')),
                setProp_UNSAFE(
                  target,
                  createLayoutPropertyPath('PinnedRight'),
                  jsxAttributeValue(newRight),
                ),
              ],
              'canvas',
            )
          } else if (cursorPosition.x < frame.x && hasRight) {
            dispatch(
              [
                unsetProperty(target, createLayoutPropertyPath('PinnedRight')),
                setProp_UNSAFE(
                  target,
                  createLayoutPropertyPath('PinnedLeft'),
                  jsxAttributeValue(frame.x),
                ),
              ],
              'canvas',
            )
          }
        }
        if (draggedProp === 'PinnedTop' || draggedProp === 'PinnedBottom') {
          const hasTop = eitherToMaybe(getLayoutProperty('PinnedTop', right(jsxAttributes))) != null
          const hasBottom =
            eitherToMaybe(getLayoutProperty('PinnedBottom', right(jsxAttributes))) != null
          if (cursorPosition.y > frame.y + frame.height && hasTop) {
            const newBottom = containingRectangle.height - frame.y - frame.height
            dispatch(
              [
                unsetProperty(target, createLayoutPropertyPath('PinnedTop')),
                setProp_UNSAFE(
                  target,
                  createLayoutPropertyPath('PinnedBottom'),
                  jsxAttributeValue(newBottom),
                ),
              ],
              'canvas',
            )
          } else if (cursorPosition.y < frame.y && hasBottom) {
            dispatch(
              [
                unsetProperty(target, createLayoutPropertyPath('PinnedBottom')),
                setProp_UNSAFE(
                  target,
                  createLayoutPropertyPath('PinnedTop'),
                  jsxAttributeValue(frame.y),
                ),
              ],
              'canvas',
            )
          }
        }
      }
    },
    [draggedProp, props, frame, dispatch, target, containingRectangle, jsxAttributes, isDraggable],
  )

  const onMouseUp = React.useCallback(() => {
    setDraggedProp(null)
  }, [setDraggedProp])

  React.useEffect(() => {
    window.addEventListener('mousemove', onMouseMove)
    return () => {
      window.removeEventListener('mousemove', onMouseMove)
    }
  }, [onMouseMove])
  React.useEffect(() => {
    window.addEventListener('mouseup', onMouseUp)
    return () => {
      window.removeEventListener('mouseup', onMouseUp)
    }
  }, [onMouseUp])

  const onMouseDown = React.useCallback(
    (propName: LayoutProp | StyleLayoutProp, event: React.MouseEvent<HTMLDivElement>) => {
      if (isDraggable) {
        setDraggedProp(propName)
      }
    },
    [setDraggedProp, isDraggable],
  )

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
        propName: 'PinnedLeft',
        isHorizontalLine: true,
        value: left,
        size: frame.x - containingRectangle.x,
        top: frame.y + frame.height / 2 + props.canvasOffset.y,
        left: containingRectangle.x + props.canvasOffset.x,
        onMouseDown: onMouseDown,
      })
    }
    if (styleRight != null) {
      pins.push({
        key: 'right',
        propName: 'PinnedRight',
        isHorizontalLine: true,
        value: styleRight,
        size: containingRectangle.x + containingRectangle.width - (frame.x + frame.width),
        top: frame.y + frame.height / 2 + props.canvasOffset.y,
        left: frame.width + frame.x + props.canvasOffset.x,
        onMouseDown: onMouseDown,
      })
    }
    if (top != null) {
      pins.push({
        key: 'top',
        propName: 'PinnedTop',
        isHorizontalLine: false,
        value: top,
        size: frame.y - containingRectangle.y,
        top: containingRectangle.y + props.canvasOffset.y,
        left: frame.x + frame.width / 2 + props.canvasOffset.x,
        onMouseDown: onMouseDown,
      })
    }
    if (bottom != null) {
      pins.push({
        key: 'bottom',
        propName: 'PinnedBottom',
        isHorizontalLine: false,
        value: bottom,
        size: containingRectangle.y + containingRectangle.height - (frame.y + frame.height),
        top: frame.height + frame.y + props.canvasOffset.y,
        left: frame.x + frame.width / 2 + props.canvasOffset.x,
        onMouseDown: onMouseDown,
      })
    }
    if (width != null) {
      pins.push({
        key: 'width',
        propName: 'Width',
        isHorizontalLine: true,
        value: width,
        size: frame.width,
        top: frame.y - 10 + props.canvasOffset.y,
        left: frame.x + props.canvasOffset.x,
        onMouseDown: onMouseDown,
      })
    }
    if (height != null) {
      pins.push({
        key: 'height',
        propName: 'Height',
        isHorizontalLine: false,
        value: height,
        size: frame.height,
        top: frame.y + props.canvasOffset.y,
        left: frame.x - 10 + props.canvasOffset.x,
        onMouseDown: onMouseDown,
      })
    }
    if (centerX != null) {
      const frameCenter = containingRectangle.x + frame.x + frame.width / 2 + props.canvasOffset.x
      const containerCenter =
        containingRectangle.x + containingRectangle.width / 2 + props.canvasOffset.x
      pins.push({
        key: 'centerX',
        propName: 'PinnedCenterX',
        isHorizontalLine: true,
        value: centerX,
        size: Math.abs(frameCenter - containerCenter),
        top: containingRectangle.y + containingRectangle.height / 2 + props.canvasOffset.y,
        left: Math.min(frameCenter, containerCenter),
        onMouseDown: onMouseDown,
      })
    }
    if (centerY != null) {
      const frameCenter = containingRectangle.y + frame.y + frame.height / 2 + props.canvasOffset.y
      const containerCenter =
        containingRectangle.y + containingRectangle.height / 2 + props.canvasOffset.y
      pins.push({
        key: 'centerY',
        propName: 'PinnedCenterY',
        isHorizontalLine: false,
        value: centerY,
        size: Math.abs(frameCenter - containerCenter),
        top: Math.min(frameCenter, containerCenter),
        left: containingRectangle.x + containingRectangle.width / 2 + props.canvasOffset.x,
        onMouseDown: onMouseDown,
      })
    }
    return (
      <div>
        {pins.map((pin) => (
          <PinOutline {...pin} key={pin.key} />
        ))}
      </div>
    )
  } else {
    return null
  }
}

export const CanvasPinControls = (props: OutlineControlsProps) => {
  if (props.selectedViews.length !== 1 || props.selectedViews.every(TP.isScenePath)) {
    return null
  } else {
    return <PinControls {...props} />
  }
}
