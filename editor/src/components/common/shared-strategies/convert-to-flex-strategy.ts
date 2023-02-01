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
  childIs100PercentSizedInEitherDirection,
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

    if (childrenPaths.length === 0) {
      // fall back to a simple prop-setting without any kind of guessing
      return [setProperty('always', path, PP.create('style', 'display'), 'flex')]
    }

    const { direction, sortedChildren, averageGap, padding } = guessMatchingFlexSetup(
      metadata,
      path,
      childrenPaths,
    )

    const [childWidth100Percent, childHeight100Percent] = childIs100PercentSizedInEitherDirection(
      metadata,
      childrenPaths[0],
    )

    if (childrenPaths.length === 1 && (childWidth100Percent || childHeight100Percent)) {
      // special case: we only have a single child which has a size of 100%.
      return [
        setProperty('always', path, PP.create('style', 'display'), 'flex'),
        setProperty('always', path, PP.create('style', 'flexDirection'), direction),
        ...(childWidth100Percent ? [] : [setHugContentForAxis('horizontal', path)]),
        ...(childHeight100Percent ? [] : [setHugContentForAxis('vertical', path)]),
        ...childrenPaths.flatMap((child) => [
          ...nukeAllAbsolutePositioningPropsCommands(child),
          ...convertWidthToFlexGrowOptionally(metadata, child, direction),
        ]),
      ]
    }

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
  const firstGuessResult = detectConfigurationInDirection(
    metadata,
    children,
    firstGuess,
    parentRect,
  )
  if (firstGuessResult.childrenDontOverlap) {
    return firstGuessResult
  }
  const secondGuess = firstGuess === 'row' ? 'column' : 'row'
  const secondGuessResult = detectConfigurationInDirection(
    metadata,
    children,
    secondGuess,
    parentRect,
  )
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

  const paddingLeft = firstChild.frame.x - parentRect.x
  const paddingRight =
    parentRect.x + parentRect.width - (lastChild?.frame.x + lastChild?.frame.width)
  const horizontalPadding = Math.min(paddingLeft, paddingRight)
  const paddingTop = firstChild.frame.y - parentRect.y
  const paddingBottom =
    parentRect.y + parentRect.height - (lastChild?.frame.y + lastChild?.frame.height)
  const verticalPadding = Math.max(0, Math.min(paddingTop, paddingBottom))

  if (horizontalPadding === 0 && verticalPadding === 0) {
    return null
  }

  return `${appendPx(verticalPadding)} ${appendPx(horizontalPadding)}`
}

function appendPx(value: number): string {
  return value === 0 ? '0' : `${value}px`
}

function detectConfigurationInDirection(
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

  if (children.length < 2) {
    return {
      childrenDontOverlap: true,
      direction: direction,
      parentRect: parentRect,
      sortedChildren: sortedChildren,
      averageGap: null,
    }
  }

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
