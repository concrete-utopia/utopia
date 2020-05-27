export const isValidReactParameterOrEventName = (parameterName: string | null) => {
  if (typeof parameterName !== 'string') {
    return false
  }

  if (parameterName.trim() !== parameterName) {
    return false
  }

  if (reservedReactParameters.includes(parameterName.trim())) {
    return false
  }

  try {
    new Function(parameterName, 'var ' + parameterName)
  } catch (_) {
    return false
  }
  return true
}

/* source: https://reactjs.org/docs/events.html */

const reservedReactParameters = ['key', 'id', 'className']
const pointerEvents = [
  'onPointerDown',
  'onPointerMove',
  'onPointerUp',
  'onPointerCancel',
  'onGotPointerCapture',
  ' onLostPointerCapture',
  'onPointerEnter',
  'onPointerLeave',
  'onPointerOver',
  'onPointerOut',
]
const clipboardEvents = ['onCopy', 'onCut', 'onPaste']
const compositionEvents = ['onCompositionEnd', 'onCompositionStart', 'onCompositionUpdate']
const keyboardEvents = ['onKeyDown', 'onKeyPress', 'onKeyUp']
const focusEvents = ['onFocus', 'onBlur']
const formEvents = ['onChange', 'onInput', 'onInvalid', 'onSubmit']
const mouseEvents = [
  'onClick',
  'onContextMenu',
  'onDoubleClick',
  'onDrag',
  'onDragEnd',
  'onDragEnter',
  'onDragExit',
  'onDragLeave',
  'onDragOver',
  'onDragStart',
  'onDrop',
  'onMouseDown',
  'onMouseEnter',
  'onMouseLeave',
  'onMouseMove',
  'onMouseOut',
  'onMouseOver',
  'onMouseUp',
]
const selectEvents = ['onSelect']
const touchEvents = ['onTouchCancel', 'onTouchEnd', 'onTouchMove', 'onTouchStart']
const uiEvents = ['onScroll']
const wheelEvents = ['onWheel']
const mediaEvents = [
  'onAbort',
  'onCanPlay',
  'onCanPlayThrough',
  'onDurationChange',
  'onEmptied',
  'onEncrypted',
  'onEnded',
  'onError',
  'onLoadedData',
  'onLoadedMetadata',
  'onLoadStart',
  'onPause',
  'onPlay',
  'onPlaying',
  'onProgress',
  'onRateChange',
  'onSeeked',
  'onSeeking',
  'onStalled',
  'onSuspend',
  'onTimeUpdate',
  'onVolumeChange',
  'onWaiting',
]
const imageEvents = ['onLoad', 'onError']
const animationEvents = ['onAnimationStart', 'onAnimationEnd', 'onAnimationIteration']
const transitionEvents = ['onTransitionEnd']
const otherEvents = ['onToggle']

export const reactEvents = [
  ...pointerEvents,
  ...clipboardEvents,
  ...compositionEvents,
  ...keyboardEvents,
  ...focusEvents,
  ...formEvents,
  ...mouseEvents,
  ...selectEvents,
  ...touchEvents,
  ...uiEvents,
  ...wheelEvents,
  ...mediaEvents,
  ...imageEvents,
  ...animationEvents,
  ...transitionEvents,
  ...otherEvents,
]

export const reservedReactParametersAndEvents = [...reactEvents, ...reservedReactParameters]
