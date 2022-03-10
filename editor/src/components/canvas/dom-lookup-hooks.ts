import React from 'react'
import { WindowPoint } from '../../core/shared/math-utils'
import { EditorStorePatched } from '../editor/store/editor-state'
import { useRefEditorState } from '../editor/store/store-hook'
import { CanvasPositions } from './canvas-types'
import { windowToCanvasCoordinates } from './dom-lookup'

const canvasSelector = (store: EditorStorePatched) => store.editor.canvas

export function useWindowToCanvasCoordinates(): (screenPoint: WindowPoint) => CanvasPositions {
  const canvasStateRef = useRefEditorState(canvasSelector)
  return React.useCallback(
    (screenPoint: WindowPoint) => {
      return windowToCanvasCoordinates(
        canvasStateRef.current.scale,
        canvasStateRef.current.roundedCanvasOffset,
        screenPoint,
      )
    },
    [canvasStateRef],
  )
}
