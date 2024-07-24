import type { ElementPath } from 'utopia-shared/src/types'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { AllElementProps } from '../../editor/store/editor-state'
import type { CanvasCommand } from '../../canvas/commands/commands'
import {
  flexContainerProps,
  gridContainerProps,
  nukeAllAbsolutePositioningPropsCommands,
  prunePropsCommands,
  sizeToVisualDimensions,
} from '../../inspector/inspector-common'
import { getChildrenPathsForContainer } from './convert-strategies-common'
import type { CanvasFrameAndTarget } from '../../canvas/canvas-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { setProperty } from '../../canvas/commands/set-property-command'
import * as PP from '../../../core/shared/property-path'

function guessLayoutInfoAlongAxis(
  children: Array<CanvasFrameAndTarget>,
  sortFn: (a: CanvasFrameAndTarget, b: CanvasFrameAndTarget) => number,
  comesAfter: (a: CanvasFrameAndTarget, b: CanvasFrameAndTarget) => boolean,
  gapBetween: (a: CanvasFrameAndTarget, b: CanvasFrameAndTarget) => number,
): { nChildren: number; averageGap: number } {
  if (children.length === 0) {
    return { nChildren: 0, averageGap: 0 }
  }

  const sortedChildren = children.sort(sortFn)
  let childrenAlongAxis = 1
  let gaps: number[] = []
  let currentChild = sortedChildren[0]
  for (const child of sortedChildren.slice(1)) {
    if (comesAfter(currentChild, child)) {
      childrenAlongAxis += 1
      gaps.push(gapBetween(currentChild, child))
      currentChild = child
    }
  }

  const averageGap =
    gaps.length === 0 ? 0 : Math.floor(gaps.reduce((a, b) => a + b, 0) / gaps.length)

  return {
    nChildren: childrenAlongAxis,
    averageGap: averageGap,
  }
}

function guessMatchingGridSetup(children: Array<CanvasFrameAndTarget>): {
  gap: number
  numberOfColumns: number
  numberOfRows: number
} {
  const horizontalData = guessLayoutInfoAlongAxis(
    children,
    (a, b) => a.frame.x - b.frame.x,
    (a, b) => a.frame.x + a.frame.width <= b.frame.x,
    (a, b) => b.frame.x - (a.frame.x + a.frame.width),
  )
  const verticalData = guessLayoutInfoAlongAxis(
    children,
    (a, b) => a.frame.y - b.frame.y,
    (a, b) => a.frame.y + a.frame.height <= b.frame.y,
    (a, b) => b.frame.y - (a.frame.y + a.frame.height),
  )

  return {
    gap: (horizontalData.averageGap + verticalData.averageGap) / 2,
    numberOfColumns: horizontalData.nChildren,
    numberOfRows: verticalData.nChildren,
  }
}

export function convertLayoutToGridCommands(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  elementPaths: Array<ElementPath>,
  allElementProps: AllElementProps,
): Array<CanvasCommand> {
  return elementPaths.flatMap((elementPath) => {
    const childrenPaths = getChildrenPathsForContainer(
      metadata,
      elementPathTree,
      elementPath,
      allElementProps,
    )
    const childFrames: Array<CanvasFrameAndTarget> = childrenPaths.map((child) => ({
      target: child,
      frame: MetadataUtils.getFrameOrZeroRectInCanvasCoords(child, metadata),
    }))

    const { gap, numberOfColumns, numberOfRows } = guessMatchingGridSetup(childFrames)

    return [
      ...prunePropsCommands(flexContainerProps, elementPath),
      ...prunePropsCommands(gridContainerProps, elementPath),
      ...childrenPaths.flatMap((child) => [
        ...nukeAllAbsolutePositioningPropsCommands(child),
        ...sizeToVisualDimensions(metadata, elementPathTree, child),
      ]),
      setProperty('always', elementPath, PP.create('style', 'display'), 'grid'),
      setProperty('always', elementPath, PP.create('style', 'gap'), gap),
      setProperty(
        'always',
        elementPath,
        PP.create('style', 'gridTemplateColumns'),
        Array(numberOfColumns).fill('1fr').join(' '),
      ),
      setProperty(
        'always',
        elementPath,
        PP.create('style', 'gridTemplateRows'),
        Array(numberOfRows).fill('1fr').join(' '),
      ),
    ]
  })
}
