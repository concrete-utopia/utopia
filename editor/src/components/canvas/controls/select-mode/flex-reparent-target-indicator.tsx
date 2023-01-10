import React from 'react'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const FlexReparentTargetIndicator = controlForStrategyMemoized(() => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'FlexReparentTargetIndicator scale',
  )
  const FlexReparentIndicatorSize = React.useMemo(() => 2 / scale, [scale])
  const reparentTargetLines = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.controls.flexReparentTargetLines,
    'FlexReparentTargetIndicator lines',
  )
  const isHorizontal = React.useMemo(
    () => reparentTargetLines.every((line) => line.height === 0),
    [reparentTargetLines],
  )
  const positionOffsetX = isHorizontal ? 0 : -FlexReparentIndicatorSize / 2
  const positionOffsetY = isHorizontal ? -FlexReparentIndicatorSize / 2 : 0

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
              width: isHorizontal ? line.width : FlexReparentIndicatorSize,
              height: !isHorizontal ? line.height : FlexReparentIndicatorSize,
              backgroundColor: 'blue',
            }}
          ></div>
        ))}
      </div>
    </CanvasOffsetWrapper>
  )
})
