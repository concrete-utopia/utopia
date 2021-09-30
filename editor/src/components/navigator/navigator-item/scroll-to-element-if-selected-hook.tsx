import React from 'react'
import { cancelIdleCallback, requestIdleCallback } from '../../../utils/request-idle-callback-shim'

// Modified from this: https://stackoverflow.com/a/15203639/305009
function isElementVisible(element: HTMLElement) {
  const rect = element.getBoundingClientRect()
  const vWidth = window.innerWidth ?? document.documentElement!.clientWidth
  const vHeight = window.innerHeight ?? document.documentElement!.clientHeight
  const elementFromPoint = function (x: number, y: number) {
    return document.elementFromPoint(x, y)
  }

  if (process.env.JEST_WORKER_ID != null) {
    // Jest mitigation, jsdom doesn't have document.elementFromPoint or scrollIntoView
    return true
  }

  if (rect.right < 0 || rect.bottom < 0 || rect.left > vWidth || rect.top > vHeight) {
    // Return false if it's not in the viewport
    return false
  } else {
    // Return true if any of its four corners are visible
    return (
      element.contains(elementFromPoint(rect.left, rect.top)) ||
      element.contains(elementFromPoint(rect.right, rect.top)) ||
      element.contains(elementFromPoint(rect.right, rect.bottom)) ||
      element.contains(elementFromPoint(rect.left, rect.bottom))
    )
  }
}
