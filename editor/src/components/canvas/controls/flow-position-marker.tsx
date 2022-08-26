import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../core/shared/element-template'
import { CanvasPoint, magnitude, offsetRect } from '../../../core/shared/math-utils'
import { useColorTheme } from '../../../uuiui'
import { ElementProps } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
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

  // Get frame of replaced element. Find the flow position of that. Render the marker there
  const frameOfReplacedElementInFlowPosition = useEditorState((store) => {
    const newIndex = store.strategyState.customStrategyState.lastReorderIdx
    if (newIndex != null && store.editor.selectedViews.length === 1) {
      const draggedElement = store.editor.selectedViews[0]
      const siblings = MetadataUtils.getSiblingPaths(store.editor.jsxMetadata, draggedElement)
      const existingIndex = siblings.findIndex((sibling) => EP.pathsEqual(sibling, draggedElement))
      const movedForward = newIndex > existingIndex
      const isMovingToTheEnd = movedForward && siblings.length === newIndex + 1
      // We give the impression that this will render before the element it is over, so when moving forward
      // we want to render it over the next element, or after the current if moving to the end
      const newIndexToUse = movedForward && !isMovingToTheEnd ? newIndex + 1 : newIndex

      const path = siblings[newIndexToUse]
      const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)

      const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
      const elementProps = store.editor.allElementProps[EP.toString(path)] ?? {}
      const relativeOffset = getRelativeOffset(element, elementProps)
      const movingToTheEndOffset = isMovingToTheEnd ? frame?.height ?? 0 : 0
      const combinedOffset = {
        x: relativeOffset.x,
        y: relativeOffset.y + movingToTheEndOffset,
      } as CanvasPoint
      return frame == null ? null : offsetRect(frame, combinedOffset)
    } else {
      return null
    }
  }, 'FlowPositionMarker frameOfReplacedElementInFlowPosition')

  const currentFrameInFlowPosition = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const path = store.editor.selectedViews[0]
      const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)

      const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path)
      const elementProps = store.editor.allElementProps[EP.toString(path)] ?? {}
      const relativeOffset = getRelativeOffset(element, elementProps)

      const relativeOffsetSize = magnitude(relativeOffset)

      return frame == null /* || relativeOffsetSize < 10 */
        ? null
        : offsetRect(frame, relativeOffset)
    } else {
      return null
    }
  }, 'FlowPositionMarker currentFrameInFlowPosition')

  const frameInFlowPosition =
    frameOfReplacedElementInFlowPosition == null || currentFrameInFlowPosition == null
      ? null
      : {
          x: frameOfReplacedElementInFlowPosition.x,
          y:
            frameOfReplacedElementInFlowPosition.y +
            (frameOfReplacedElementInFlowPosition.height - currentFrameInFlowPosition.height),
          width: currentFrameInFlowPosition.width,
          height: currentFrameInFlowPosition.height,
        }

  const lineColor = colorTheme.primary.value
  const lineWidth = 1 / scale

  return frameInFlowPosition == null ? null : (
    <CanvasOffsetWrapper key={`flow-position-marker`}>
      <div
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
            lineWidth * 5
          }px, ${lineColor} ${lineWidth * 5}px, ${lineColor} ${lineWidth * 5 + lineWidth}px)`,
        }}
      />
    </CanvasOffsetWrapper>
  )
})

export const FlowStartingPositionMarker = React.memo(() => {
  const colorTheme = useColorTheme()
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'FlowStartingPositionMarker canvas scale',
  )

  const framesInFlowPosition = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const target = store.editor.selectedViews[0]
      const metadata =
        store.editor.canvas.interactionSession == null
          ? store.editor.jsxMetadata
          : store.strategyState.startingMetadata
      const siblings = MetadataUtils.getSiblingPaths(metadata, target)
      return mapDropNulls((path) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(path, metadata)

        const element = MetadataUtils.findElementByElementPath(metadata, path)
        const elementProps = store.editor.allElementProps[EP.toString(path)] ?? {}
        const relativeOffset = getRelativeOffset(element, elementProps)

        return frame == null ? null : offsetRect(frame, relativeOffset)
      }, siblings)
    } else {
      return null
    }
  }, 'FlowStartingPositionMarker framesInFlowPosition')

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
