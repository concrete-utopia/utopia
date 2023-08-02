import type { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import type { Modifiers } from '../../../utils/modifiers'
import type { CanvasControlType, InteractionSessionWithoutMetadata } from './interaction-state'
import { createInteractionViaMouse, updateInteractionViaMouse } from './interaction-state'

export function createMouseInteractionForTests(
  mouseDownPoint: CanvasPoint,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
  drag: CanvasVector,
): InteractionSessionWithoutMetadata {
  return updateInteractionViaMouse(
    createInteractionViaMouse(mouseDownPoint, modifiers, activeControl, 'zero-drag-not-permitted'),
    'DRAG',
    drag,
    modifiers,
    null,
  )
}
