import { LayoutDimension } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { pluck, sortBy } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { fastForEach } from '../../../core/shared/utils'
import { CanvasFrameAndTarget } from '../../canvas/canvas-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  convertWidthToFlexGrowOptionally,
  nukeAllAbsolutePositioningPropsCommands,
  sizeToVisualDimensions,
} from '../../inspector/inspector-common'
import { setHugContentForAxis } from '../../inspector/inspector-strategies/hug-contents-basic-strategy'

type FlexDirection = 'row' | 'column' // a limited subset as we won't never guess row-reverse or column-reverse

export function convertLayoutToFlexCommands(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): Array<CanvasCommand> {
  return elementPaths.flatMap((path) => {
    const childrenPaths = MetadataUtils.getChildrenPaths(metadata, path)
    const { direction, sortedChildren, averageGap } = guessLayoutDirection(
      metadata,
      path,
      childrenPaths,
    )

    return [
      setProperty('always', path, PP.create('style', 'display'), 'flex'),
      setProperty('always', path, PP.create('style', 'flexDirection'), direction),
      setProperty('always', path, PP.create('style', 'gap'), averageGap),
      setHugContentForAxis('horizontal', path),
      setHugContentForAxis('vertical', path),
      ...childrenPaths.flatMap((child) => [
        ...nukeAllAbsolutePositioningPropsCommands(child),
        ...sizeToVisualDimensions(metadata, child),
        ...convertWidthToFlexGrowOptionally(metadata, child),
      ]),
    ]
  })
}

function guessLayoutDirection(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
  children: Array<ElementPath>,
): { direction: FlexDirection; sortedChildren: Array<CanvasFrameAndTarget>; averageGap: number } {
  const parentSize = MetadataUtils.getFrameInCanvasCoords(target, metadata) ?? {
    width: 0,
    height: 0,
  }
  const firstGuess: FlexDirection = parentSize.width > parentSize.height ? 'row' : 'column'
  const firstGuessResult = isThereOverlapInDirection(metadata, children, firstGuess)
  if (firstGuessResult.childrenDontOverlap) {
    return firstGuessResult
  }
  const secondGuess = firstGuess === 'row' ? 'column' : 'row'
  const secondGuessResult = isThereOverlapInDirection(metadata, children, secondGuess)
  if (secondGuessResult.childrenDontOverlap) {
    return secondGuessResult
  }

  // since none of the directions are great, let's fall back to our first guess
  return firstGuessResult
}

function isThereOverlapInDirection(
  metadata: ElementInstanceMetadataMap,
  children: Array<ElementPath>,
  direction: FlexDirection,
): {
  childrenDontOverlap: boolean
  direction: FlexDirection
  sortedChildren: Array<CanvasFrameAndTarget>
  averageGap: number
} {
  const childFrames: Array<CanvasFrameAndTarget> = children.map((child) => ({
    target: child,
    frame: MetadataUtils.getFrameInCanvasCoords(child, metadata)!,
  }))
  const sortedChildren = sortBy(childFrames, (l, r) =>
    direction === 'row' ? l.frame.x - r.frame.x : l.frame.y - r.frame.y,
  )

  let childrenDontOverlap: boolean = true
  let gapSum = 0

  fastForEach(sortedChildren, (child, i) => {
    if (i === 0) {
      return
    } else {
      const prevFrame: CanvasRectangle = sortedChildren[i - 1].frame
      const gap =
        direction === 'row'
          ? child.frame.x - (prevFrame.x + prevFrame.width)
          : child.frame.y - (prevFrame.y + prevFrame.height)

      gapSum += gap
      childrenDontOverlap = childrenDontOverlap && gap > -1
    }
  })

  return {
    childrenDontOverlap,
    sortedChildren,
    direction: direction,
    averageGap: gapSum / (sortedChildren.length - 1),
  }
}
