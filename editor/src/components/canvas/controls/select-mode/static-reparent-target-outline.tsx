import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isFiniteRectangle } from '../../../../core/shared/math-utils'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { HighlightControl } from '../highlight-control'

export const StaticReparentTargetOutlineIndicator = controlForStrategyMemoized(() => {
  const colorTheme = useColorTheme()
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'FlexReparentTargetIndicator scale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.realCanvasOffset,
    'FlexReparentTargetIndicator scale',
  )
  const parentFrame = useEditorState(
    Substores.canvasAndMetadata,
    (store) => {
      const parentPath = store.editor.canvas.controls.parentOutlineHighlight
      if (parentPath != null) {
        return MetadataUtils.getFrameInCanvasCoords(parentPath, store.editor.jsxMetadata)
      } else {
        return null
      }
    },
    'StaticReparentTargetOutlineIndicator',
  )

  if (parentFrame != null && isFiniteRectangle(parentFrame)) {
    return (
      <HighlightControl
        canvasOffset={canvasOffset}
        scale={scale}
        color={colorTheme.canvasSelectionPrimaryOutline.value}
        frame={parentFrame}
        displayZeroSized={'do-not-display-zero-sized'}
      />
    )
  } else {
    return null
  }
})
