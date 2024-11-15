import { UTOPIA_PATH_KEY } from '../../core/model/utopia-constants'
import { CanvasContainerID } from './canvas-types'
import { getDeepestPathOnDomElement } from '../../core/shared/uid-utils'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from 'utopia-shared/src/types'
import type { ElementOrParent } from './controls/grid-controls-for-strategies'
import { assertNever } from '../../core/shared/utils'

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
      if (element instanceof HTMLElement) {
        switch (elementOrParent) {
          case 'element':
            if (element.parentElement != null) {
              return fromElement(element.parentElement)
            }
            break
          case 'parent':
            return fromElement(element)
          default:
            assertNever(elementOrParent)
        }
      }
    }
  }
  return undefined
}
