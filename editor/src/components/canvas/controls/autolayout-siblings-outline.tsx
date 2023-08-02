import React from 'react'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import { colorTheme } from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

interface AutoLayoutSiblingsOutlineProps {
  bounds: CanvasRectangle | null
}

export const AutoLayoutSiblingsOutline = controlForStrategyMemoized(
  (props: AutoLayoutSiblingsOutlineProps) => {
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'OutlineControl scale',
    )

    const { bounds } = props

    if (bounds == null) {
      return null
    }

    return (
      <CanvasOffsetWrapper>
        <div
          className='role-outline'
          style={{
            position: 'absolute',
            display: 'block',
            left: `${bounds.x - 0.5 / scale}px`,
            top: `${bounds.y - 0.5 / scale}px`,
            width: `${bounds.width + 1 / scale}px`,
            height: `${bounds.height + 1 / scale}px`,
            borderColor: colorTheme.canvasSelectionSecondaryOutline.value,
            borderWidth: `${1 / scale}px`,
            borderStyle: 'dotted',
            pointerEvents: 'none',
          }}
        />
      </CanvasOffsetWrapper>
    )
  },
)
