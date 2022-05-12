import React from 'react'
import { zeroCanvasPoint } from '../../../../core/shared/math-utils'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { getMultiselectBounds } from '../../canvas-strategies/shared-absolute-move-strategy-helpers'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const DragOutlineControl = React.memo(() => {
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'GhostOutline selectedViews',
  )
  const scale = useEditorState((store) => store.editor.canvas.scale, 'OutlineControl scale')

  const dragVector = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      return store.editor.canvas.interactionSession.interactionData.drag ?? zeroCanvasPoint
    } else {
      return zeroCanvasPoint
    }
  }, 'GhostOutline dragVector')

  const outlineRef = useBoundingBox(selectedViews, (ref, boundingBox) => {
    ref.current.style.left = `${dragVector.x + boundingBox.x + 0.5 / scale}px`
    ref.current.style.top = `${dragVector.y + boundingBox.y + 0.5 / scale}px`
    ref.current.style.width = `${boundingBox.width - (0.5 / scale) * 3}px`
    ref.current.style.height = `${boundingBox.height - (0.5 / scale) * 3}px`
  })

  const colorTheme = useColorTheme()

  return (
    <CanvasOffsetWrapper>
      <div
        ref={outlineRef}
        style={{
          position: 'absolute',
          boxSizing: 'border-box',
          backgroundColor: 'black',
          boxShadow: `0px 0px 0px ${1 / scale}px  ${colorTheme.canvasSelectionFocusable.value}`,
          opacity: '50%',
        }}
      />
    </CanvasOffsetWrapper>
  )
})
