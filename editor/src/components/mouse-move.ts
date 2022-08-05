// HACK Chrome 59 for some unknown reason fires multiple mouse events per frame, even if the frame hasn't been updated for long time
// In order to avoid a domino effect where we spend 100+ ms on rerendering stuff and discarding it only to rerender a new mouse event
// For specific user events we introduce this super primitive throttling
// I hope this can be removed as soon as we upgrade Chrome and/or we understand what is preventing chrome from drawing to the fucking screen after a successful render
export let didWeHandleMouseMoveForThisFrame = false
export let didWeHandleWheelForThisFrame = false

export function mouseMoveHandled() {
  didWeHandleMouseMoveForThisFrame = true
}

export function mouseWheelHandled() {
  didWeHandleWheelForThisFrame = true
}

let resetMouseStatusCallbackIdentifier: number | null = null

export function resetMouseStatus(): void {
  stopResettingMouseStatus()
  innerResetMouseStatus()
}

export function stopResettingMouseStatus(): void {
  if (resetMouseStatusCallbackIdentifier != null) {
    cancelAnimationFrame(resetMouseStatusCallbackIdentifier)
  }
}

function innerResetMouseStatus(): void {
  didWeHandleMouseMoveForThisFrame = false
  didWeHandleWheelForThisFrame = false
  resetMouseStatusCallbackIdentifier = requestAnimationFrame(innerResetMouseStatus)
}
