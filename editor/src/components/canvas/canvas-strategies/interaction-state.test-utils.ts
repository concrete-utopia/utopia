import { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import { Modifiers } from '../../../utils/modifiers'
import {
  CanvasControlType,
  createInteractionViaMouse,
  InteractionSessionWithoutMetadata,
  updateInteractionViaMouse,
} from './interaction-state'

export function createMouseInteractionForTests(
  mouseDownPoint: CanvasPoint,
  modifiers: Modifiers,
  activeControl: CanvasControlType,
  drag: CanvasVector,
): InteractionSessionWithoutMetadata {
  return updateInteractionViaMouse(
    createInteractionViaMouse(mouseDownPoint, modifiers, activeControl, false),
    'DRAG',
    drag,
    modifiers,
    null,
  )
}
