import React from 'react'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { eitherToMaybe, isRight, left, right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import { isJSXElement } from '../../../core/shared/element-template'
import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'

interface PositionOutlineProps {
  path: ElementPath
  frame: CanvasRectangle
  canvasOffset: CanvasPoint
  scale: number
}

export const PositionOutline = React.memo((props: PositionOutlineProps) => {
  const containingFrame = useContainingFrameForElement(props.path)
  const attributes = usePropsOrJSXAttributes(props.path)
  if (containingFrame != null) {
    let pins: PinOutlineProps[] = collectPinOutlines(
      attributes,
      props.frame,
      containingFrame,
      props.canvasOffset,
      props.scale,
    )
    return (
      <div key={EP.toString(props.path)}>
        {pins.map((pin) => (
          <PinOutline {...pin} key={pin.key} />
        ))}
      </div>
    )
  } else {
    return null
  }
})

const usePropsOrJSXAttributes = (path: ElementPath): PropsOrJSXAttributes => {
  return useEditorState(
    React.useCallback(
      (store) => {
        const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
        if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
          return right(element.element.value.props)
        } else {
          return left(element?.props ?? {})
        }
      },
      [path],
    ),
    'usePropsOrJSXAttributes',
  )
}

const useContainingFrameForElement = (path: ElementPath): CanvasRectangle | null => {
  return useEditorState(
    React.useCallback(
      (store) => {
        const containingBlockPath = MetadataUtils.findContainingBlock(
          store.editor.jsxMetadata,
          path,
        )
        if (containingBlockPath != null && !EP.isStoryboardPath(containingBlockPath)) {
          return MetadataUtils.getFrameInCanvasCoords(containingBlockPath, store.editor.jsxMetadata)
        } else {
          return null
        }
      },
      [path],
    ),
    'useContainingFrameForElement',
  )
}

const collectPinOutlines = (
  attributes: PropsOrJSXAttributes,
  frame: CanvasRectangle,
  containingFrame: CanvasRectangle,
  canvasOffset: CanvasPoint,
  scale: number,
): PinOutlineProps[] => {
  const pinLeft = eitherToMaybe(getLayoutProperty('left', attributes, ['style']))
  const pinTop = eitherToMaybe(getLayoutProperty('top', attributes, ['style']))
  const pinRight = eitherToMaybe(getLayoutProperty('right', attributes, ['style']))
  const pinBottom = eitherToMaybe(getLayoutProperty('bottom', attributes, ['style']))
  let pins: PinOutlineProps[] = []
  if (pinLeft != null) {
    pins.push({
      key: 'left',
      isHorizontalLine: true,
      size: frame.x - containingFrame.x,
      startX: containingFrame.x + canvasOffset.x,
      startY: frame.y + frame.height / 2 + canvasOffset.y,
      scale: scale,
    })
  }
  if (pinTop != null) {
    pins.push({
      key: 'top',
      isHorizontalLine: false,
      size: frame.y - containingFrame.y,
      startX: frame.x + frame.width / 2 + canvasOffset.x,
      startY: containingFrame.y + canvasOffset.y,
      scale: scale,
    })
  }
  if (pinRight != null) {
    pins.push({
      key: 'right',
      isHorizontalLine: true,
      size: containingFrame.x + containingFrame.width - (frame.x + frame.width),
      startX: frame.width + frame.x + canvasOffset.x,
      startY: frame.y + frame.height / 2 + canvasOffset.y,
      scale: scale,
    })
  }
  if (pinBottom != null) {
    pins.push({
      key: 'bottom',
      isHorizontalLine: false,
      size: containingFrame.y + containingFrame.height - (frame.y + frame.height),
      startX: frame.x + frame.width / 2 + canvasOffset.x,
      startY: frame.height + frame.y + canvasOffset.y,
      scale: scale,
    })
  }
  return pins
}

interface PinOutlineProps {
  key: string
  isHorizontalLine: boolean
  startX: number
  startY: number
  size: number
  scale: number
}

const PinOutline = React.memo(
  (props: PinOutlineProps): JSX.Element => {
    const colorTheme = useColorTheme()
    const width = props.isHorizontalLine ? props.size : 0
    const height = props.isHorizontalLine ? 0 : props.size
    const borderTop = props.isHorizontalLine
      ? `${1 / props.scale}px dashed ${colorTheme.primary.value}`
      : 'none'
    const borderLeft = props.isHorizontalLine
      ? 'none'
      : `${1 / props.scale}px dashed ${colorTheme.primary.value}`
    return (
      <div
        style={{
          position: 'absolute',
          left: props.startX,
          top: props.startY,
          width: width,
          height: height,
          borderTop: borderTop,
          borderLeft: borderLeft,
        }}
      />
    )
  },
)
