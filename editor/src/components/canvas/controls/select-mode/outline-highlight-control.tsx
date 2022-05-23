import React from 'react'
import { useEditorState } from '../../../editor/store/store-hook'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

const animation = `
  .outline-animation {
    animation: glow3 2s ease;
    animation-iteration-count: 1;
  }
  @keyframes glow3 {
    0% {
    }
    50% {
      box-shadow: 0px 0px 2px 2px blue, 0px 0px 4px 4px white;
    }
    100% {
    }
}`

export const OutlineHighlightControl = React.memo(() => {
  const outlineFrames = useEditorState(
    (store) => store.editor.canvas.controls.outlineHighlights,
    'ConversionHighlightOutline frames',
  )
  return (
    <CanvasOffsetWrapper>
      <style>{animation}</style>
      <div style={{ display: 'block' }}>
        {outlineFrames.map((frame, i) => (
          <div
            key={i}
            style={{
              position: 'absolute',
              top: frame.y,
              left: frame.x,
              width: frame.width,
              height: frame.height,
            }}
            className={'outline-animation'}
          ></div>
        ))}
      </div>
    </CanvasOffsetWrapper>
  )
})
