import { InteractionSession, isDragInteractionData } from './interaction-state'

export function interactionSessionStartedWithLeftMouseButton(
  interactionSession: InteractionSession | null | undefined,
): boolean {
  if (interactionSession == null) {
    return false
  } else {
    if (isDragInteractionData(interactionSession.interactionData)) {
      return interactionSession.interactionData.startedWithButton === 1
    } else {
      return false
    }
  }
}
