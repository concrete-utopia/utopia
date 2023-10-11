import React from 'react'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const FlexReparentIndicatorSize = 2

export const FlexReparentTargetIndicator = controlForStrategyMemoized(() => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'FlexReparentTargetIndicator scale',
  )
  const flexReparentIndicatorScaledSize = React.useMemo(
    () => FlexReparentIndicatorSize / scale,
    [scale],
  )
  const reparentTargetLines = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.controls.flexReparentTargetLines,
    'FlexReparentTargetIndicator lines',
  )
  const isHorizontal = React.useMemo(
    () => reparentTargetLines.every((line) => line.height === 0),
    [reparentTargetLines],
  )
  const positionOffsetX = isHorizontal ? 0 : -flexReparentIndicatorScaledSize / 2
  const positionOffsetY = isHorizontal ? -flexReparentIndicatorScaledSize / 2 : 0

  return (
    <CanvasOffsetWrapper>
      <div style={{ display: 'block' }}>
        {reparentTargetLines.map((line, i) => (
          <div
            data-testid={`flex-reparent-indicator-${i}`}
            key={i}
            style={{
              position: 'absolute',
              left: line.x + positionOffsetX,
              top: line.y + positionOffsetY,
              width: isHorizontal ? line.width : flexReparentIndicatorScaledSize,
              height: !isHorizontal ? line.height : flexReparentIndicatorScaledSize,
              backgroundColor: 'blue',
            }}
          ></div>
        ))}
      </div>
    </CanvasOffsetWrapper>
  )
})
