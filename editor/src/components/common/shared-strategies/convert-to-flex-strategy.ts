import { LayoutDimension } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { pluck, sortBy } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CanvasFrameAndTarget } from '../../canvas/canvas-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import { FlexDirection } from '../../inspector/common/css-utils'
import {
  Axis,
  convertWidthToFlexGrowOptionally,
  nukeAllAbsolutePositioningPropsCommands,
  sizeToVisualDimensions,
} from '../../inspector/inspector-common'
import { setHugContentForAxis } from '../../inspector/inspector-strategies/hug-contents-basic-strategy'

export function convertLayoutToFlexCommands(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): Array<CanvasCommand> {
  return elementPaths.flatMap((path) => {
    const childrenPaths = MetadataUtils.getChildrenPaths(metadata, path)
    const { direction, sortedChildren } = guessLayoutDirection(metadata, path, childrenPaths)

    return [
      setProperty('always', path, PP.create('style', 'display'), 'flex'),
      setProperty('always', path, PP.create('style', 'flexDirection'), direction),
      setProperty('always', path, PP.create('style', 'gap'), 15),
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
): { direction: FlexDirection; sortedChildren: Array<CanvasFrameAndTarget> } {
  const parentSize = MetadataUtils.getFrameInCanvasCoords(target, metadata) ?? {
    width: 0,
    height: 0,
  }
  const firstGuess = parentSize.width > parentSize.height ? 'horizontal' : 'vertical'
  const firstGuessResult = isThereOverlapInDirection(metadata, children, firstGuess)
  if (firstGuessResult.childrenDontOverlap) {
    return {
      direction: firstGuessResult.direction,
      sortedChildren: firstGuessResult.sortedChildren,
    }
  }
  const secondGuess = firstGuess === 'horizontal' ? 'vertical' : 'horizontal'
  const secondGuessResult = isThereOverlapInDirection(metadata, children, secondGuess)
  if (secondGuessResult.childrenDontOverlap) {
    return {
      direction: secondGuessResult.direction,
      sortedChildren: secondGuessResult.sortedChildren,
    }
  }

  // since none of the directions are great, let's fall back to our first guess
  return {
    direction: firstGuessResult.direction,
    sortedChildren: secondGuessResult.sortedChildren,
  }
}

function isThereOverlapInDirection(
  metadata: ElementInstanceMetadataMap,
  children: Array<ElementPath>,
  direction: Axis,
): {
  childrenDontOverlap: boolean
  direction: FlexDirection
  sortedChildren: Array<CanvasFrameAndTarget>
} {
  const childFrames: Array<CanvasFrameAndTarget> = children.map((child) => ({
    target: child,
    frame: MetadataUtils.getFrameInCanvasCoords(child, metadata)!,
  }))
  const sortedChildren = sortBy(childFrames, (l, r) =>
    direction === 'horizontal' ? l.frame.x - r.frame.x : l.frame.y - r.frame.y,
  )

  const childrenDontOverlap = sortedChildren.every((child, i) => {
    if (i === 0) {
      return true
    } else {
      const prevFrame: CanvasRectangle = sortedChildren[i - 1].frame
      if (direction === 'horizontal') {
        // all elements are on the right side of the previous sibling's right edge
        return child.frame.x > prevFrame.x + prevFrame.width
      } else {
        // all elements are below the previous sibling's bottom edge
        return child.frame.y > prevFrame.y + prevFrame.height
      }
    }
  })

  return {
    childrenDontOverlap,
    sortedChildren,
    direction: direction === 'horizontal' ? 'row' : 'column',
  }
}
