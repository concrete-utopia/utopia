import React from 'react'
import { styleStringInArray } from '../../../utils/common-constants'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import type { PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { eitherToMaybe, isRight, left, right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import { isJSXElement } from '../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import { CanvasPoint, isFiniteRectangle } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { ElementPathKeepDeepEquality } from '../../../utils/deep-equality-instances'
import { useColorTheme } from '../../../uuiui'
import { CanvasRectangleKeepDeepEquality } from '../../editor/store/store-deep-equality-instances'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const PinLines = React.memo(() => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'PinLines scale',
  )
  const elementsAndFrames = useEditorState(
    Substores.fullStore,
    (store) => {
      const selectedViewsNotHidden = store.editor.selectedViews.filter(
        (sv) => !store.editor.hiddenInstances.includes(sv),
      )
      return mapDropNulls((path) => {
        const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
        const isAbsolute = MetadataUtils.isPositionAbsolute(element)
        const frame = element?.globalFrame
        if (isAbsolute && frame != null && isFiniteRectangle(frame)) {
          return {
            path: path,
            frame: frame,
          }
        } else {
          return null
        }
      }, selectedViewsNotHidden)
    },
    'PinLines',
    (oldValue, newValue) => {
      return (
        oldValue.length === newValue.length &&
        oldValue.every(
          (old, index) =>
            CanvasRectangleKeepDeepEquality(old.frame, newValue[index]?.frame).areEqual &&
            ElementPathKeepDeepEquality(old.path, newValue[index]?.path).areEqual,
        )
      )
    },
  )

  return (
    <>
      {elementsAndFrames.map((frameInfo) => (
        <PositionOutline
          key={EP.toString(frameInfo.path)}
          frame={frameInfo.frame}
          path={frameInfo.path}
          scale={scale}
        />
      ))}
    </>
  )
})

interface PositionOutlineProps {
  path: ElementPath
  frame: CanvasRectangle
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
      props.scale,
    )
    return (
      <CanvasOffsetWrapper>
        {pins.map((pin) => (
          <PinOutline {...pin} key={pin.name} />
        ))}
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})

const usePropsOrJSXAttributes = (path: ElementPath): PropsOrJSXAttributes => {
  return useEditorState(
    Substores.metadata,
    (store) => {
      const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
      if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
        return right(element.element.value.props)
      } else {
        const elementProps = store.editor.allElementProps[EP.toString(path)]
        return left(elementProps ?? {})
      }
    },
    'usePropsOrJSXAttributes',
  )
}

const useContainingFrameForElement = (path: ElementPath): CanvasRectangle | null => {
  return useEditorState(
    Substores.metadata,
    (store) => {
      const metadata = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
      if (metadata != null && !EP.isStoryboardChild(path)) {
        return metadata?.specialSizeMeasurements.coordinateSystemBounds
      } else {
        return null
      }
    },
    'useContainingFrameForElement',
  )
}

const collectPinOutlines = (
  attributes: PropsOrJSXAttributes,
  frame: CanvasRectangle,
  containingFrame: CanvasRectangle,
  scale: number,
): PinOutlineProps[] => {
  const pinLeft = eitherToMaybe(getLayoutProperty('left', attributes, styleStringInArray))
  const pinTop = eitherToMaybe(getLayoutProperty('top', attributes, styleStringInArray))
  const pinRight = eitherToMaybe(getLayoutProperty('right', attributes, styleStringInArray))
  const pinBottom = eitherToMaybe(getLayoutProperty('bottom', attributes, styleStringInArray))
  let pins: PinOutlineProps[] = []
  if (pinLeft != null) {
    pins.push({
      name: 'left',
      isHorizontalLine: true,
      size: frame.x - containingFrame.x,
      startX: containingFrame.x,
      startY: frame.y + frame.height / 2,
      scale: scale,
    })
  }
  if (pinTop != null) {
    pins.push({
      name: 'top',
      isHorizontalLine: false,
      size: frame.y - containingFrame.y,
      startX: frame.x + frame.width / 2,
      startY: containingFrame.y,
      scale: scale,
    })
  }
  if (pinRight != null) {
    pins.push({
      name: 'right',
      isHorizontalLine: true,
      size: containingFrame.x + containingFrame.width - (frame.x + frame.width),
      startX: frame.width + frame.x,
      startY: frame.y + frame.height / 2,
      scale: scale,
    })
  }
  if (pinBottom != null) {
    pins.push({
      name: 'bottom',
      isHorizontalLine: false,
      size: containingFrame.y + containingFrame.height - (frame.y + frame.height),
      startX: frame.x + frame.width / 2,
      startY: frame.height + frame.y,
      scale: scale,
    })
  }
  return pins
}

interface PinOutlineProps {
  name: string
  isHorizontalLine: boolean
  startX: number
  startY: number
  size: number
  scale: number
}

const PinOutline = React.memo((props: PinOutlineProps): JSX.Element => {
  const colorTheme = useColorTheme()
  const width = props.isHorizontalLine ? props.size : 0
  const height = props.isHorizontalLine ? 0 : props.size
  const borderTop = props.isHorizontalLine
    ? `${1 / props.scale}px dashed ${colorTheme.primary.value}`
    : 'none'
  const borderLeft = props.isHorizontalLine
    ? 'none'
    : `${1 / props.scale}px dashed ${colorTheme.primary.value}`
  const lineLeft = props.startX - 0.5 / props.scale
  const lineTop = props.startY - 0.5 / props.scale

  return (
    <div
      style={{
        position: 'absolute',
        left: lineLeft,
        top: lineTop,
        width: width,
        height: height,
        borderTop: borderTop,
        borderLeft: borderLeft,
        pointerEvents: 'none',
      }}
      data-testid={`pin-line-${props.name}`}
    />
  )
})
