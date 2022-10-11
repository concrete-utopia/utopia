import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { getMultiselectBounds } from '../canvas-strategies/strategies/shared-move-strategies-helpers'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

const useColorForDisplayType = (colorTheme: any) => {
  return useEditorState((store) => {
    if (store.editor.selectedViews.length > 0) {
      const metadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )
      return metadata?.specialSizeMeasurements.display === 'block'
        ? colorTheme.canvasDragOutlineBlock.value
        : colorTheme.canvasDragOutlineInline.value
    } else {
      return colorTheme.canvasSelectionPrimaryOutline.value
    }
  }, 'FlowReorderDragOutline color')
}

interface FlowReorderDragOutlineProps {
  targets: Array<ElementPath>
}
export const FlowReorderDragOutline = controlForStrategyMemoized(
  ({ targets }: FlowReorderDragOutlineProps) => {
    const scale = useEditorState(
      (store) => store.editor.canvas.scale,
      'FlowReorderDragOutline scale',
    )
    const frame = useEditorState((store) => {
      return getMultiselectBounds(store.strategyState.startingMetadata, targets)
    }, 'FlowReorderDragOutline frame')
    const dragVector = useEditorState((store) => {
      if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
        return store.editor.canvas.interactionSession.interactionData.drag
      } else {
        return null
      }
    }, 'FlowReorderDragOutline dragVector')

    const colorTheme = useColorTheme()
    const color = useColorForDisplayType(colorTheme)

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
              boxShadow: `0px 0px 0px ${1 / scale}px  ${color}`,
              opacity: '50%',
            }}
          />
        </CanvasOffsetWrapper>
      )
    }
  },
)
