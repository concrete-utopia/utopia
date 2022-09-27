// Mouse events, particulary mousemove, can fire multiple times in a single frame, especially if that frame is taking a long time.
// to render. In order to keep the application as performant as possible, we have this primitive form of throttling which will unlock
// mousemove and wheel event handling at the start of each frame, and so that we can then lock it again as soon as we have handled the
// first of each such event.
// Note that although Chrome already does this for us, aligning mouse events with the start of an animation frame, this buffering
// is explicitly disabled when the Chrome devtools are open, and there are no plans to change this behaviour:
// https://bugs.chromium.org/p/chromium/issues/detail?id=992954
// Because of this, and the fact that it is technically valid for multiple mousemove events to fire in a given frame, we need to keep
// some form of this throttling to prevent the app from grinding to a halt
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
