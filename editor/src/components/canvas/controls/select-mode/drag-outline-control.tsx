import React from 'react'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { getMultiselectBounds } from '../../canvas-strategies/strategies/shared-move-strategies-helpers'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const DragOutlineControl = controlForStrategyMemoized(() => {
  const scale = useEditorState((store) => store.editor.canvas.scale, 'OutlineControl scale')
  const frame = useEditorState((store) => {
    return getMultiselectBounds(store.strategyState.startingMetadata, store.editor.selectedViews)
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
