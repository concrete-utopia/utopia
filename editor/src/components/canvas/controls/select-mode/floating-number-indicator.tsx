import React from 'react'
import { CanvasPoint } from '../../../../core/shared/math-utils'
import { useColorTheme } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { CanvasLabel } from './controls-common'

export const FloatingCSSNumberIndicatorTestId = 'PaddingValueIndicatorTestId'

export interface FloatingIndicatorProps {
  value: string | number
  position: CanvasPoint
}

export const FloatingIndicator = controlForStrategyMemoized<FloatingIndicatorProps>((props) => {
  const { value, position } = props
  const colorTheme = useColorTheme()

  const scale = useEditorState((store) => store.editor.canvas.scale, 'FloatingIndicator scale')

  return (
    <CanvasOffsetWrapper>
      <div
        data-testid={FloatingCSSNumberIndicatorTestId}
        style={{
          position: 'absolute',
          left: position.x,
          top: position.y,
        }}
      >
        <CanvasLabel
          value={value}
          scale={scale}
          color={colorTheme.brandNeonPink.value}
          textColor={colorTheme.white.value}
        />
      </div>
    </CanvasOffsetWrapper>
  )
})
