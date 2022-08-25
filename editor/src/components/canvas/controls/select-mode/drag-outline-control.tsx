import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { CanvasPoint, offsetRect } from '../../../../core/shared/math-utils'
import { useColorTheme } from '../../../../uuiui'
import { ElementProps } from '../../../editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'
import { getMultiselectBounds } from '../../canvas-strategies/shared-absolute-move-strategy-helpers'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

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

export const DragOutlineControl = React.memo(() => {
  const scale = useEditorState((store) => store.editor.canvas.scale, 'OutlineControl scale')
  const frame = useEditorState((store) => {
    if (
      store.editor.selectedViews.length === 1 &&
      MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )?.specialSizeMeasurements.position === 'relative'
    ) {
      const target = store.editor.selectedViews[0]
      const targetFrame = MetadataUtils.getFrameInCanvasCoords(
        target,
        store.strategyState.startingMetadata,
      )
      if (targetFrame == null) {
        return null
      }

      const element = MetadataUtils.findElementByElementPath(
        store.strategyState.startingMetadata,
        target,
      )
      const elementProps = store.strategyState.startingAllElementProps[EP.toString(target)] ?? {}
      const relativeOffset = getRelativeOffset(element, elementProps)

      return offsetRect(targetFrame, relativeOffset)
    } else {
      return getMultiselectBounds(store.strategyState.startingMetadata, store.editor.selectedViews)
    }
  }, 'GhostOutline frame')
  const dragVector = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      return store.editor.canvas.interactionSession.interactionData.drag
    } else {
      return null
    }
  }, 'GhostOutline dragVector')

  const colorTheme = useColorTheme()

  if (frame == null || dragVector == null) {
    return null
  } else {
    return (
      <CanvasOffsetWrapper>
        <div
          style={{
            position: 'absolute',
            top: frame.y + dragVector.y,
            left: frame.x + dragVector.x,
            width: frame.width,
            height: frame.height,
            boxSizing: 'border-box',
            boxShadow: `0px 0px 0px ${1 / scale}px  ${colorTheme.canvasSelectionFocusable.value}`,
            opacity: '50%',
          }}
        />
      </CanvasOffsetWrapper>
    )
  }
})
