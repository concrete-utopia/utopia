import React from 'react'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { CanvasLabel } from './controls-common'

export const FloatingCSSNumberIndicatorTestId = 'PaddingValueIndicatorTestId'

export interface FloatingIndicatorProps {
  value: string | number
  position: CanvasPoint
  color?: string
}

export const FloatingIndicator = controlForStrategyMemoized<FloatingIndicatorProps>((props) => {
  const { value, position } = props
  const colorTheme = useColorTheme()

  const color = props.color ?? colorTheme.brandNeonPink.value

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'FloatingIndicator scale',
  )

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
        <CanvasLabel value={value} scale={scale} color={color} textColor={colorTheme.white.value} />
      </div>
    </CanvasOffsetWrapper>
  )
})
