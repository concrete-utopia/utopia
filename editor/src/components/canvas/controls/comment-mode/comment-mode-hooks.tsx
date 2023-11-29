import React from 'react'
import type { MouseCallbacks } from '../select-mode/select-mode-hooks'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { switchEditorMode } from '../../../editor/actions/action-creators'
import type { CommentId } from '../../../editor/editor-modes'
import { EditorModes, newComment } from '../../../editor/editor-modes'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { windowPoint } from '../../../../core/shared/math-utils'

export function useCommentModeSelectAndHover(comment: CommentId | null): MouseCallbacks {
  const dispatch = useDispatch()

  const storeRef = useRefEditorState((store) => {
    return {
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  })

  const onMouseUp = React.useCallback(
    (event: React.MouseEvent) => {
      if (comment == null) {
        const loc = windowToCanvasCoordinates(
          storeRef.current.scale,
          storeRef.current.canvasOffset,
          windowPoint({ x: event.clientX, y: event.clientY }),
        )
        dispatch([
          switchEditorMode(
            EditorModes.commentMode(newComment(loc.canvasPositionRounded), 'not-dragging'),
          ),
        ])
      } else {
        dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))])
      }
    },
    [dispatch, comment, storeRef],
  )

  return useKeepShallowReferenceEquality({
    onMouseMove: NO_OP,
    onMouseDown: NO_OP,
    onMouseUp: onMouseUp,
  })
}
