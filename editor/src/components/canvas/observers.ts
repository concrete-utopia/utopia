import * as ResizeObserverSyntheticDefault from 'resize-observer-polyfill'
import type { ElementPath } from 'utopia-shared/src/types'
import { getDeepestPathOnDomElement } from '../../core/shared/uid-utils'
import * as EP from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import { CanvasContainerID } from './canvas-types'

export const ResizeObserver =
  window.ResizeObserver ?? ResizeObserverSyntheticDefault.default ?? ResizeObserverSyntheticDefault

export const ObserversAvailable = window.MutationObserver != null && ResizeObserver != null

export type ChangeCallback = (change: 'updated' | 'deleted') => void

export interface TargetAndCallback {
  target: ElementPath
  callback: ChangeCallback
}

let targetsAndCallbacks: Array<TargetAndCallback> = []
let currentMutationObserver: MutationObserver | null = null
let observersSetup = false
let priorMountCount: number = -1

function cleanupObservers(): void {
  if (currentMutationObserver != null) {
    currentMutationObserver.disconnect()
    currentMutationObserver = null
  }
  observersSetup = false
}

function setupObservers(mountCount: number): void {
  // If the observers can't be used bail out right from the start.
  if (!ObserversAvailable) {
    return
  }

  // Skip setting up the observers if they have already been setup and the mount count hasn't changed.
  // Where the mount count changing implies the entire canvas will have been unmounted and recreated.
  if (observersSetup && mountCount === priorMountCount) {
    return
  }

  // Disconnect the old observer if it exists.
  cleanupObservers()

  // Find the canvas container to observe.
  const canvasContainer = document.getElementById(CanvasContainerID)
  if (canvasContainer == null) {
    return
  }

  const mutationObserver = new MutationObserver((mutations) => {
    for (const mutation of mutations) {
      switch (mutation.type) {
        case 'childList':
        case 'attributes':
          function maybeTriggerCallback(targetNode: Node, change: 'updated' | 'deleted'): void {
            if (targetNode instanceof Element) {
              // Find the path of this particular element.
              const pathOnElement = getDeepestPathOnDomElement(targetNode)
              if (pathOnElement != null) {
                for (const targetAndCallback of targetsAndCallbacks) {
                  if (EP.pathsEqual(targetAndCallback.target, pathOnElement)) {
                    targetAndCallback.callback(change)
                  }
                }
              }
            }
          }

          // Handle updates to the target first, then deletions from the children followed by
          // additions to the children.
          maybeTriggerCallback(mutation.target, 'updated')
          mutation.removedNodes.forEach((removedNode) => {
            maybeTriggerCallback(removedNode, 'deleted')
          })
          mutation.addedNodes.forEach((addedNode) => {
            maybeTriggerCallback(addedNode, 'updated')
          })
          break
        case 'characterData':
          break
        default:
          assertNever(mutation.type)
      }
    }
  })

  // Connect up the mutation observer.
  mutationObserver.observe(canvasContainer, {
    childList: true,
    subtree: true,
    attributes: true,
  })

  // Record our current state of affairs.
  currentMutationObserver = mutationObserver
  priorMountCount = mountCount
  observersSetup = true
}

export function addChangeCallback(
  mountCount: number,
  target: ElementPath,
  callback: ChangeCallback,
): void {
  setupObservers(mountCount)
  targetsAndCallbacks.push({ target: target, callback: callback })
}

export function removeChangeCallback(target: ElementPath, callback: ChangeCallback): void {
  targetsAndCallbacks = targetsAndCallbacks.filter(
    (item) => item.target !== target || item.callback !== callback,
  )
}
