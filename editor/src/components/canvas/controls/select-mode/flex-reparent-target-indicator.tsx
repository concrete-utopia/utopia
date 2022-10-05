import React from 'react'
import { useColorTheme } from '../../../../uuiui'
import { Utils } from '../../../../uuiui-deps'
import { useEditorState } from '../../../editor/store/store-hook'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export const FlexReparentTargetIndicator = React.memo(() => {
  const colorTheme = useColorTheme()
  const [mouseOverSizeOffset, setMouseOverSizeOffset] = React.useState(0)
  const reparentTargetLines = useEditorState(
    (store) => store.editor.canvas.controls.flexReparentTargetLines,
    'FlexReparentTargetIndicator lines',
  )

  const isHoverInteraction = useEditorState(
    (store) => store.editor.canvas.interactionSession?.interactionData.type === 'HOVER',
    'FlexReparentTargetIndicator isHoverInteraction',
  )
  const mouseover = React.useCallback(() => {
    setMouseOverSizeOffset(4)
  }, [setMouseOverSizeOffset])

  return (
    <CanvasOffsetWrapper>
      <div style={{ display: 'block' }}>
        {reparentTargetLines.map((line, i) => (
          <div
            onMouseOver={isHoverInteraction ? mouseover : Utils.NO_OP}
            data-testid={`flex-reparent-indicator-${i}`}
            key={i}
            style={{
              position: 'absolute',
              top: line.y - (line.height === 0 ? mouseOverSizeOffset / 2 : 0),
              left: line.x - (line.width === 0 ? mouseOverSizeOffset / 2 : 0),
              width: line.width + (line.width === 0 ? mouseOverSizeOffset : 0),
              height: line.height + (line.height === 0 ? mouseOverSizeOffset : 0),
              backgroundColor: colorTheme.white.value,
              border: `1px solid ${colorTheme.primary.value}`,
              borderRadius: 2,
              boxShadow: `0px 0px 1px 0px ${colorTheme.primary.value}`,
            }}
          ></div>
        ))}
      </div>
    </CanvasOffsetWrapper>
  )
})
