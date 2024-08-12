import type { ElementPath } from 'utopia-shared/src/types'
import { UTOPIA_PATH_KEY } from '../../core/model/utopia-constants'
import * as EP from '../../core/shared/element-path'
import type { DomElementMetadata } from '../../core/shared/element-template'
import type { CanvasRectangle } from '../../core/shared/math-utils'
import { canvasPoint } from '../../core/shared/math-utils'
import { getPathWithStringsOnDomElement } from '../../core/shared/uid-utils'
import { createElementInstanceMetadataForElement, lazyValue } from './dom-walker'
import { CanvasContainerID } from './canvas-types'
import { optionalMap } from '../../core/shared/optional-utils'
import { getCanvasRectangleFromElement } from '../../core/shared/dom-utils'

function collectMetadataForElementPath(
  path: ElementPath,
  globalProps: {
    validPaths: Array<ElementPath>
    scale: number
    containerRectLazy: () => CanvasRectangle // TODO probably no need to be lazy anymore
    invalidatedPathsForStylesheetCache: Set<string>
    selectedViews: Array<ElementPath>
  },
): DomElementMetadata {
  const foundElement = document.querySelector(
    `[${UTOPIA_PATH_KEY}^="${EP.toString(path)}"]`,
  ) as HTMLElement | null

  if (foundElement != null) {
    const parentPoint = canvasPoint({ x: 0, y: 0 }) // TODO this is not sensible

    const collectForElement = (element: Node): DomElementMetadata => {
      if (element instanceof HTMLElement) {
        const pathsWithStrings = getPathWithStringsOnDomElement(element)
        if (pathsWithStrings.length == 0) {
          throw new Error('No path found on element')
        } else {
          const foundValidPaths = pathsWithStrings.filter((pathWithString) => {
            const staticPath = EP.makeLastPartOfPathStatic(pathWithString.path)
            return globalProps.validPaths.some((vp) => EP.pathsEqual(vp, staticPath)) // this is from the old implementation, no descendants are included
          })

          return createElementInstanceMetadataForElement(
            element,
            parentPoint,
            path,
            foundValidPaths.map((p) => p.path),
            globalProps,
          )
        }
      }
      throw new Error('Element not found')
    }

    return collectForElement(foundElement)
  }

  throw new Error(`Element not found for ${EP.toString(path)}`)
}

function getValidPathsFromCanvasContainer(canvasRootContainer: HTMLElement): Array<ElementPath> {
  const validPaths: Array<ElementPath> | null = optionalMap(
    (paths) => paths.split(' ').map(EP.fromString),
    canvasRootContainer.getAttribute('data-utopia-valid-paths'),
  )

  if (validPaths == null) {
    throw new Error(
      'Utopia Internal Error: Running DOM-walker without canvasRootPath or validRootPaths',
    )
  }

  return validPaths
}

export function collectMetadataForValidPaths(options: {
  scale: number
  selectedViews: Array<ElementPath>
  metadataToUpdate: { [path: string]: DomElementMetadata }
}): { [path: string]: DomElementMetadata } {
  const canvasRootContainer = document.getElementById(CanvasContainerID)
  if (canvasRootContainer == null) {
    throw new Error('Canvas root container not found')
  }

  const validPaths = getValidPathsFromCanvasContainer(canvasRootContainer)

  const containerRectLazy = lazyValue(() => {
    return getCanvasRectangleFromElement(
      canvasRootContainer,
      options.scale,
      'without-text-content',
      'nearest-half',
    )
  })

  let updatedMetadataMap = { ...options.metadataToUpdate }

  validPaths.forEach((path) => {
    try {
      const metadata = collectMetadataForElementPath(path, {
        validPaths: validPaths,
        scale: options.scale,
        containerRectLazy: containerRectLazy,
        invalidatedPathsForStylesheetCache: new Set(),
        selectedViews: options.selectedViews,
      })
      updatedMetadataMap[EP.toString(path)] = metadata
    } catch (error) {
      console.error(error)
    }
  })

  return updatedMetadataMap
}
