import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { CanvasPoint, magnitude, offsetRect } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { ElementProps } from '../../editor/store/editor-state'
import { useEditorState, useEditorStateFull } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

function getRelativeOffset(
  element: ElementInstanceMetadata | null,
  elementProps: ElementProps,
): CanvasPoint {
  if (element?.specialSizeMeasurements.position === 'relative') {
    const { left, right, top, bottom } = elementProps?.style
    const horizontalOffset = left ? -left : right ?? 0
    const verticalOffset = top ? -top : bottom ?? 0
    return { x: horizontalOffset, y: verticalOffset } as CanvasPoint
  } else {
    return { x: 0, y: 0 } as CanvasPoint
  }
}

export const FlowPositionMarker = React.memo(() => {
  const colorTheme = useColorTheme()
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'FlowPositionMarker canvas scale',
  )

  const frameInFlowPosition = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const path = store.editor.selectedViews[0]
      const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)

      const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
      const elementProps = store.editor.allElementProps[EP.toString(path)] ?? {}
      const relativeOffset = getRelativeOffset(element, elementProps)

      const relativeOffsetSize = magnitude(relativeOffset)

      return frame == null || relativeOffsetSize < 10 ? null : offsetRect(frame, relativeOffset)
    } else {
      return null
    }
  }, 'FlowPositionMarker frameInFlowPosition')

  return frameInFlowPosition == null ? null : (
    <CanvasOffsetWrapper key={`flow-position-marker`}>
      <div
        style={{
          position: 'absolute',
          left: frameInFlowPosition.x,
          top: frameInFlowPosition.y,
          width: frameInFlowPosition.width,
          height: frameInFlowPosition.height,
          outlineStyle: 'dashed',
          outlineColor: colorTheme.primary.value,
          outlineWidth: 1 / scale,
          pointerEvents: 'none',
        }}
      />
    </CanvasOffsetWrapper>
  )
})

export const FlowStartingPositionMarker = React.memo(() => {
  const colorTheme = useColorTheme()
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'FlowPositionMarker canvas scale',
  )

  const framesInFlowPosition = useEditorStateFull((store) => {
    if (store.unpatchedEditor.selectedViews.length === 1) {
      const target = store.unpatchedEditor.selectedViews[0]
      const siblings = MetadataUtils.getSiblingPaths(store.unpatchedEditor.jsxMetadata, target)
      return mapDropNulls((path) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(path, store.unpatchedEditor.jsxMetadata)

        const element = MetadataUtils.findElementByElementPath(
          store.unpatchedEditor.jsxMetadata,
          path,
        )
        const elementProps = store.unpatchedEditor.allElementProps[EP.toString(path)] ?? {}
        const relativeOffset = getRelativeOffset(element, elementProps)

        return frame == null ? null : offsetRect(frame, relativeOffset)
      }, siblings)
    } else {
      return null
    }
  })

  const lineColor = colorTheme.subduedForeground.o(20).value
  const lineWidth = 1 / scale

  return (
    <CanvasOffsetWrapper key={`flow-starting-position-marker`}>
      {framesInFlowPosition?.map((frameInFlowPosition, i) => (
        <div
          key={`frame-in-flow-position-${i}`}
          style={{
            position: 'absolute',
            left: frameInFlowPosition.x,
            top: frameInFlowPosition.y,
            width: frameInFlowPosition.width,
            height: frameInFlowPosition.height,
            outlineStyle: 'solid',
            outlineColor: lineColor,
            outlineWidth: lineWidth,
            pointerEvents: 'none',
            background: `repeating-linear-gradient(45deg, #00000000 0px, #00000000 ${
              lineWidth * 10
            }px, ${lineColor} ${lineWidth * 10}px, ${lineColor} ${lineWidth * 10 + lineWidth}px)`,
          }}
        />
      ))}
    </CanvasOffsetWrapper>
  )
})
