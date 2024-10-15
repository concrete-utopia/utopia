import type { CanvasVector } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import { Modifier } from '../../../../utils/modifiers'
import type { EditorDispatch } from '../../../editor/action-types'
import CanvasActions from '../../canvas-actions'
import { createInteractionViaMouse, gridGapHandle } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import type { Axis } from '../../gap-utils'

export function startInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  canvasOffset: CanvasVector,
  scale: number,
  axis: Axis,
) {
  if (event.buttons === 1 && event.button !== 2) {
    event.stopPropagation()
    const canvasPositions = windowToCanvasCoordinates(
      scale,
      canvasOffset,
      windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
    )
    dispatch([
      CanvasActions.createInteractionSession(
        createInteractionViaMouse(
          canvasPositions.canvasPositionRaw,
          Modifier.modifiersForEvent(event),
          gridGapHandle(axis),
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}
