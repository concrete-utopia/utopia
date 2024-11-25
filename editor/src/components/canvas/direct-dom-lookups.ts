import { UTOPIA_PATH_KEY } from '../../core/model/utopia-constants'
import { CanvasContainerID } from './canvas-types'
import { getDeepestPathOnDomElement } from '../../core/shared/uid-utils'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from 'utopia-shared/src/types'
import { assertNever } from '../../core/shared/utils'
import type { ElementOrParent } from './controls/grid-measurements'

export type FromElement<T> = (element: HTMLElement) => T

export function getFromElement<T>(
  path: ElementPath,
  fromElement: FromElement<T>,
  elementOrParent: ElementOrParent,
): T | undefined {
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
      const realElement = (() => {
        switch (elementOrParent) {
          case 'element':
            return element
          case 'parent':
            return element.parentElement
          default:
            assertNever(elementOrParent)
        }
      })()
      if (realElement instanceof HTMLElement) {
        return fromElement(realElement)
      }
    }
  }
  return undefined
}
