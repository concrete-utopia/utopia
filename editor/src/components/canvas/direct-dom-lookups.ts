import { UTOPIA_PATH_KEY } from '../../core/model/utopia-constants'
import { CanvasContainerID } from './canvas-types'
import { getDeepestPathOnDomElement } from '../../core/shared/uid-utils'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from 'utopia-shared/src/types'

export type FromElement<T> = (element: HTMLElement) => T

export function getFromElement<T>(path: ElementPath, fromElement: FromElement<T>): T | undefined {
  const pathString = EP.toString(path)
  const elements = document.querySelectorAll(
    `#${CanvasContainerID} [${UTOPIA_PATH_KEY}^="${pathString}"]`,
  )
  for (const element of Array.from(elements)) {
    const pathFromElement = getDeepestPathOnDomElement(element)
    if (
      (EP.pathsEqual(path, pathFromElement) &&
        pathFromElement != null &&
        !EP.isRootElementOfInstance(pathFromElement)) ||
      EP.isRootElementOf(pathFromElement, path)
    ) {
      if (element instanceof HTMLElement) {
        return fromElement(element)
      }
    }
  }
  return undefined
}

export function getFromParent<T>(path: ElementPath, fromElement: FromElement<T>): T | undefined {
  const pathString = EP.toString(path)
  const elements = document.querySelectorAll(
    `#${CanvasContainerID} [${UTOPIA_PATH_KEY}^="${pathString}"]`,
  )
  for (const element of Array.from(elements)) {
    const pathFromElement = getDeepestPathOnDomElement(element)
    if (
      (EP.pathsEqual(path, pathFromElement) &&
        pathFromElement != null &&
        !EP.isRootElementOfInstance(pathFromElement)) ||
      EP.isRootElementOf(pathFromElement, path)
    ) {
      if (element instanceof HTMLElement && element.parentElement != null) {
        return fromElement(element.parentElement)
      }
    }
  }
  return undefined
}
