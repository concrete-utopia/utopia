import * as ResizeObserverSyntheticDefault from 'resize-observer-polyfill'

export const ResizeObserver =
  window.ResizeObserver ?? ResizeObserverSyntheticDefault.default ?? ResizeObserverSyntheticDefault

export const ObserversAvailable = window.MutationObserver != null && ResizeObserver != null
