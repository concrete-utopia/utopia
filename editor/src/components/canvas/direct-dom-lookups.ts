import { UTOPIA_PATH_KEY } from '../../core/model/utopia-constants'
import { CanvasContainerID } from './canvas-types'
import { getDeepestPathOnDomElement } from '../../core/shared/uid-utils'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from 'utopia-shared/src/types'
import type { SimpleRectangle } from '../../core/shared/math-utils'

// Figure out how to include elements to be watched.
// Figure out how to remove elements from being watched.

// Do the update lazily when it is requested.
// Have a function which clears the cache, which we trigger from the dispatch flow?
// Probably best if the above works with elements to re-render?

interface ComputedStyleAndBoundingRect {
  computedStyle: CSSStyleDeclaration
  boundingRectangle: SimpleRectangle
}

let elementComputedStylesAndBoundingRects: { [path: string]: ComputedStyleAndBoundingRect | null } =
  {}

export function clearComputedStylesAndBoundingRects(): void {
  elementComputedStylesAndBoundingRects = {}
}

export function getComputedStyleAndBoundingRectFromElement(
  path: ElementPath,
): ComputedStyleAndBoundingRect | undefined {
  const pathString = EP.toString(path)
  if (!(pathString in elementComputedStylesAndBoundingRects)) {
    const elements = document.querySelectorAll(
      `#${CanvasContainerID} [${UTOPIA_PATH_KEY}="${pathString}"]`,
    )
    for (const elementKey in elements) {
      if (elements.hasOwnProperty(elementKey)) {
        const element = elements[elementKey]
        const pathFromElement = getDeepestPathOnDomElement(element)
        if (EP.pathsEqual(path, pathFromElement)) {
          const boundingRect = element.getBoundingClientRect()
          elementComputedStylesAndBoundingRects[pathString] = {
            computedStyle: window.getComputedStyle(element),
            boundingRectangle: {
              x: boundingRect.x,
              y: boundingRect.y,
              width: boundingRect.width,
              height: boundingRect.height,
            },
          }
        }
      }
    }
  }
  return elementComputedStylesAndBoundingRects[pathString] ?? undefined
}
