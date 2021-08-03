import React from 'react'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { eitherToMaybe, isRight, left, right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import { isJSXElement } from '../../../core/shared/element-template'
import { CanvasPoint, CanvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { useEditorState } from '../../editor/store/store-hook'

interface PositionOutlineProps {
  path: ElementPath
  frame: CanvasRectangle
}

export const PositionOutline = betterReactMemo('PositionOutline', (props: PositionOutlineProps) => {
  const containingFrame = useContainingFrameForElement(props.path)
  const attributes = usePropsOrJSXAttributes(props.path)
  const canvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'PositionOutline canvasOffset',
  )
  if (containingFrame != null) {
    let pins: PinOutlineProps[] = useCollectPinOutlines(
      attributes,
      props.frame,
      containingFrame,
      canvasOffset,
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
  return useEditorState((store) => {
    const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
    if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
      return right(element.element.value.props)
    } else {
      return left(element?.props ?? {})
    }
  }, 'usePropsOrJSXAttributes')
}

const useContainingFrameForElement = (path: ElementPath): CanvasRectangle | null => {
  return useEditorState((store) => {
    const containingBlockPath = MetadataUtils.findContainingBlock(store.editor.jsxMetadata, path)
    return (
      MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, containingBlockPath)
        ?.globalFrame ?? null
    )
  }, 'useContainingFrameForElement')
}

const useCollectPinOutlines = (
  attributes: PropsOrJSXAttributes,
  frame: CanvasRectangle,
  containingFrame: CanvasRectangle,
  canvasOffset: CanvasPoint,
): PinOutlineProps[] => {
  const pinLeft = eitherToMaybe(getLayoutProperty('PinnedLeft', attributes))
  const pinTop = eitherToMaybe(getLayoutProperty('PinnedTop', attributes))
  const pinRight = eitherToMaybe(getLayoutProperty('PinnedRight', attributes))
  const pinBottom = eitherToMaybe(getLayoutProperty('PinnedBottom', attributes))
  let pins: PinOutlineProps[] = []
  if (pinLeft != null) {
    pins.push({
      key: 'left',
      isHorizontalLine: true,
      size: frame.x - containingFrame.x,
      startX: containingFrame.x + canvasOffset.x,
      startY: frame.y + frame.height / 2 + canvasOffset.y,
    })
  }
  if (pinTop != null) {
    pins.push({
      key: 'top',
      isHorizontalLine: false,
      size: frame.y - containingFrame.y,
      startX: frame.x + frame.width / 2 + canvasOffset.x,
      startY: containingFrame.y + canvasOffset.y,
    })
  }
  if (pinRight != null) {
    pins.push({
      key: 'right',
      isHorizontalLine: true,
      size: containingFrame.x + containingFrame.width - (frame.x + frame.width),
      startX: frame.width + frame.x + canvasOffset.x,
      startY: frame.y + frame.height / 2 + canvasOffset.y,
    })
  }
  if (pinBottom != null) {
    pins.push({
      key: 'bottom',
      isHorizontalLine: false,
      size: containingFrame.y + containingFrame.height - (frame.y + frame.height),
      startX: frame.x + frame.width / 2 + canvasOffset.x,
      startY: frame.height + frame.y + canvasOffset.y,
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
}

const PinOutline = betterReactMemo(
  'PinOutline',
  (props: PinOutlineProps): JSX.Element => {
    const colorTheme = useColorTheme()
    const width = props.isHorizontalLine ? props.size : 0
    const height = props.isHorizontalLine ? 0 : props.size
    const borderTop = props.isHorizontalLine ? `1px dashed ${colorTheme.primary.value}` : 'none'
    const borderLeft = props.isHorizontalLine ? 'none' : `1px dashed ${colorTheme.primary.value}`
    return (
      <>
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
      </>
    )
  },
)
