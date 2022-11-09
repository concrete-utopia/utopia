import React from 'react'
import { CanvasPoint } from '../../../../core/shared/math-utils'
import { useColorTheme } from '../../../../uuiui'
import { CSSNumber } from '../../../inspector/common/css-utils'
import { useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { CSSNumberLabel } from './controls-common'

export const FloatingCSSNumberIndicatorTestId = 'PaddingValueIndicatorTestId'

export interface FloatingCSSNumberIndicatorProps {
  value: CSSNumber
  position: CanvasPoint
  label?: string
}

export const FloatingCSSNumberIndicator =
  controlForStrategyMemoized<FloatingCSSNumberIndicatorProps>((props) => {
    const { value, position, label } = props
    const colorTheme = useColorTheme()

    const actualPaddingValue: CSSNumber = {
      unit: value.unit,
      value: Math.max(0, value.value),
    }

    const scale = useEditorState(
      (store) => store.editor.canvas.scale,
      'PaddingValueIndicator scale',
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
          <CSSNumberLabel
            value={actualPaddingValue}
            scale={scale}
            color={colorTheme.brandNeonPink.value}
            prefix={label}
          />
        </div>
      </CanvasOffsetWrapper>
    )
  })
