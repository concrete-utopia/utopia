import React from 'react'
import { useRoundedCanvasOffset } from './canvas-atoms'
import { canvasPoint } from '../../core/shared/math-utils'

export const CanvasVarProvider = React.memo(
  (props: React.PropsWithChildren<{ style: React.CSSProperties; className: string }>) => {
    const [roundedCanvasOffset, setRoundedCanvasOffset] = useRoundedCanvasOffset()
    // const roundedCanvasOffset = useEditorState(
    //   (store) => store.editor.canvas.roundedCanvasOffset,
    //   'CanvasVarProvider roundedCanvasOffset',
    // )
    return (
      <div
        style={props.style}
        className={`utopia-canvas-vars ${props.className}`}
        onWheel={(event) => {
          setRoundedCanvasOffset((offset) =>
            canvasPoint({ x: offset.x - event.deltaX / 2, y: offset.y - event.deltaY / 2 }),
          )
        }}
      >
        <style>
          {`.utopia-canvas-vars {
          --utopia-canvas-offset-x: ${roundedCanvasOffset.x}px;
          --utopia-canvas-offset-y: ${roundedCanvasOffset.y}px;
          --utopia-canvas-scale: 1;
          }`}
        </style>
        {props.children}
      </div>
    )
  },
)
