import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { last, sortBy } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasRectangle, zeroCanvasRect } from '../../../core/shared/math-utils'
import { maybeToArray } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { fastForEach } from '../../../core/shared/utils'
import { CanvasFrameAndTarget } from '../../canvas/canvas-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty, setPropertyOmitNullProp } from '../../canvas/commands/set-property-command'
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
    const { direction, sortedChildren, averageGap, padding } = guessMatchingFlexSetup(
      metadata,
      path,
      childrenPaths,
    )

    return [
      setProperty('always', path, PP.create('style', 'display'), 'flex'),
      setProperty('always', path, PP.create('style', 'flexDirection'), direction),
      ...setPropertyOmitNullProp('always', path, PP.create('style', 'gap'), averageGap),
      setHugContentForAxis('horizontal', path),
      setHugContentForAxis('vertical', path),
      ...setPropertyOmitNullProp('always', path, PP.create('style', 'padding'), padding),
      ...childrenPaths.flatMap((child) => [
        ...nukeAllAbsolutePositioningPropsCommands(child),
        ...sizeToVisualDimensions(metadata, child),
        ...convertWidthToFlexGrowOptionally(metadata, child),
      ]),
    ]
  })
}

function guessMatchingFlexSetup(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
  children: Array<ElementPath>,
): {
  direction: FlexDirection
  sortedChildren: Array<CanvasFrameAndTarget>
  averageGap: number | null
  padding: string | null
} {
  const result = guessLayoutDirection(metadata, target, children)

  if (result.sortedChildren.length === 0) {
    return { ...result, padding: null }
  }

  const padding: string | null = guessPadding(
    result.direction,
    result.parentRect,
    result.sortedChildren,
  )

  return { ...result, padding: padding }
}

function guessLayoutDirection(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
  children: Array<ElementPath>,
): {
  direction: FlexDirection
  sortedChildren: Array<CanvasFrameAndTarget>
  parentRect: CanvasRectangle
  averageGap: number | null
} {
  const parentRect = MetadataUtils.getFrameInCanvasCoords(target, metadata) ?? zeroCanvasRect
  const firstGuess: FlexDirection = parentRect.width > parentRect.height ? 'row' : 'column'
  const firstGuessResult = isThereOverlapInDirection(metadata, children, firstGuess, parentRect)
  if (firstGuessResult.childrenDontOverlap) {
    return firstGuessResult
  }
  const secondGuess = firstGuess === 'row' ? 'column' : 'row'
  const secondGuessResult = isThereOverlapInDirection(metadata, children, secondGuess, parentRect)
  if (secondGuessResult.childrenDontOverlap) {
    return secondGuessResult
  }

  // since none of the directions are great, let's fall back to our first guess
  return firstGuessResult
}

function guessPadding(
  direction: FlexDirection,
  parentRect: CanvasRectangle,
  sortedChildren: Array<CanvasFrameAndTarget>,
): string | null {
  const firstChild = sortedChildren[0]
  const lastChild = last(sortedChildren)!

  if (direction === 'row') {
    const paddingLeft = firstChild.frame.x - parentRect.x
    const paddingRight =
      parentRect.x + parentRect.width - (lastChild?.frame.x + lastChild?.frame.width)
    const horizontalPadding = Math.min(paddingLeft, paddingRight)
    if (horizontalPadding === 0) {
      return null
    }
    return `0 ${horizontalPadding}px`
  } else {
    const paddingTop = firstChild.frame.y - parentRect.y
    const paddingBottom =
      parentRect.y + parentRect.height - (lastChild?.frame.y + lastChild?.frame.height)
    const verticalPadding = Math.max(0, Math.min(paddingTop, paddingBottom))
    if (verticalPadding === 0) {
      return null
    }
    return `${verticalPadding}px 0`
  }
}

function isThereOverlapInDirection(
  metadata: ElementInstanceMetadataMap,
  children: Array<ElementPath>,
  direction: FlexDirection,
  parentRect: CanvasRectangle,
): {
  childrenDontOverlap: boolean
  direction: FlexDirection
  sortedChildren: Array<CanvasFrameAndTarget>
  averageGap: number | null
  parentRect: CanvasRectangle
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

  const averageGap = Math.max(0, gapSum / (sortedChildren.length - 1))

  return {
    childrenDontOverlap: childrenDontOverlap,
    sortedChildren: sortedChildren,
    direction: direction,
    averageGap: averageGap === 0 ? null : averageGap,
    parentRect: parentRect,
  }
}
