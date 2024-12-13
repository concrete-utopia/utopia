import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { mapDropNulls, reverse } from '../../../../../core/shared/array-utils'
import type { ElementInstanceMetadataMap } from '../../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../../../core/shared/math-utils'
import {
  canvasRectangle,
  isInfinityRectangle,
  rectFromTwoPoints,
  zeroCanvasRect,
} from '../../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../../core/shared/project-file-types'
import type { Direction, ForwardOrReverse } from '../../../../inspector/common/css-utils'

export const ExtraPadding = (canvasScale: number): number => 10 / canvasScale
export function drawTargetRectanglesForChildrenOfElement(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  singleAxisAutolayoutContainerPath: ElementPath,
  targetRectangleSize: 'padded-edge' | 'full-size',
  canvasScale: number,
  simpleFlexDirection: Direction | null,
  forwardsOrBackwards: ForwardOrReverse | null,
): Array<{ rect: CanvasRectangle; insertionIndex: number }> {
  const extraPadding = ExtraPadding(canvasScale)
  const parentBoundsOrInfinity = MetadataUtils.getFrameInCanvasCoords(
    singleAxisAutolayoutContainerPath,
    metadata,
  )

  const parentBounds =
    parentBoundsOrInfinity != null && isInfinityRectangle(parentBoundsOrInfinity)
      ? zeroCanvasRect
      : parentBoundsOrInfinity

  if (parentBounds == null || simpleFlexDirection == null || forwardsOrBackwards == null) {
    // TODO should we throw an error?
    return []
  }

  const leftOrTop = simpleFlexDirection === 'horizontal' ? 'x' : 'y'
  const leftOrTopComplement = simpleFlexDirection === 'horizontal' ? 'y' : 'x'
  const widthOrHeight = simpleFlexDirection === 'horizontal' ? 'width' : 'height'
  const widthOrHeightComplement = simpleFlexDirection === 'horizontal' ? 'height' : 'width'

  const children = MetadataUtils.getChildrenPathsOrdered(
    pathTrees,
    singleAxisAutolayoutContainerPath,
  )

  interface ElemBounds {
    start: number
    size: number
    end: number
    index: number
  }

  const pseudoElementLeftOrTop: ElemBounds = {
    start: parentBounds[leftOrTop],
    size: 0,
    end: parentBounds[leftOrTop],
    index: forwardsOrBackwards === 'forward' ? -1 : children.length,
  }
  const pseudoElementRightOrBottom: ElemBounds = {
    start: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
    size: 0,
    end: parentBounds[leftOrTop] + parentBounds[widthOrHeight],
    index: forwardsOrBackwards === 'forward' ? children.length : -1,
  }

  const childrenBounds: Array<ElemBounds> = mapDropNulls((childPath, index) => {
    if (!MetadataUtils.targetParticipatesInAutoLayout(metadata, childPath)) {
      return null
    }

    const bounds = MetadataUtils.getFrameInCanvasCoords(childPath, metadata)
    if (bounds == null || isInfinityRectangle(bounds)) {
      return null
    }
    return {
      start: bounds[leftOrTop],
      size: bounds[widthOrHeight],
      end: bounds[leftOrTop] + bounds[widthOrHeight],
      index: index,
    }
  }, children)

  const childrenBoundsAlongAxis: Array<ElemBounds> = [
    pseudoElementLeftOrTop,
    ...(forwardsOrBackwards === 'forward' ? childrenBounds : reverse(childrenBounds)),
    pseudoElementRightOrBottom,
  ]

  let flexInsertionTargets: Array<{ rect: CanvasRectangle; insertionIndex: number }> = []

  if (targetRectangleSize === 'padded-edge') {
    for (let index = 0; index < childrenBoundsAlongAxis.length; index++) {
      const bounds = childrenBoundsAlongAxis[index]

      const normalizedStart = Math.min(bounds.start, bounds.end)
      const normalizedEnd = Math.max(bounds.start, bounds.end)

      flexInsertionTargets.push(
        {
          insertionIndex: forwardsOrBackwards === 'forward' ? bounds.index : bounds.index + 1,
          rect: rectFromTwoPoints(
            {
              [leftOrTop]: normalizedStart - extraPadding,
              [leftOrTopComplement]: parentBounds[leftOrTopComplement],
            } as any as CanvasPoint, // TODO improve my type
            {
              [leftOrTop]: Math.min(
                normalizedStart + extraPadding,
                normalizedStart + bounds.size / 2,
              ),
              [leftOrTopComplement]:
                parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
            } as any as CanvasPoint, // TODO improve my type
          ),
        },
        {
          insertionIndex: forwardsOrBackwards === 'forward' ? bounds.index + 1 : bounds.index,
          rect: rectFromTwoPoints(
            {
              [leftOrTop]: Math.max(normalizedEnd - extraPadding, normalizedEnd - bounds.size / 2),
              [leftOrTopComplement]: parentBounds[leftOrTopComplement],
            } as any as CanvasPoint, // TODO improve my type
            {
              [leftOrTop]: normalizedEnd + extraPadding,
              [leftOrTopComplement]:
                parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
            } as any as CanvasPoint, // TODO improve my type
          ),
        },
      )
    }
  } else {
    // full size target rectangles, covering the entire flex element
    for (let index = 0; index < childrenBoundsAlongAxis.length - 1; index++) {
      const start = childrenBoundsAlongAxis[index].start + childrenBoundsAlongAxis[index].size / 2
      const end =
        childrenBoundsAlongAxis[index + 1].start + childrenBoundsAlongAxis[index + 1].size / 2

      const normalizedStart = Math.min(start, end)
      const normalizedEnd = Math.max(start, end)

      flexInsertionTargets.push({
        insertionIndex:
          forwardsOrBackwards === 'forward'
            ? childrenBoundsAlongAxis[index].index + 1
            : childrenBoundsAlongAxis[index].index,
        rect: rectFromTwoPoints(
          {
            [leftOrTop]: normalizedStart,
            [leftOrTopComplement]: parentBounds[leftOrTopComplement],
          } as any as CanvasPoint, // TODO improve my type
          {
            [leftOrTop]: normalizedEnd,
            [leftOrTopComplement]:
              parentBounds[leftOrTopComplement] + parentBounds[widthOrHeightComplement],
          } as any as CanvasPoint, // TODO improve my type
        ),
      })
    }
  }

  return flexInsertionTargets
}

export function getSiblingMidPointPosition(
  precedingSiblingPosition: CanvasRectangle,
  succeedingSiblingPosition: CanvasRectangle,
  direction: Direction,
  forwardOrReverse: ForwardOrReverse,
): number {
  let getStartPosition: (rect: CanvasRectangle) => number
  let getEndPosition: (rect: CanvasRectangle) => number
  switch (direction) {
    case 'horizontal':
      getStartPosition = (rect: CanvasRectangle) => {
        return rect.x
      }
      getEndPosition = (rect: CanvasRectangle) => {
        return rect.x + rect.width
      }
      break
    case 'vertical':
      getStartPosition = (rect: CanvasRectangle) => {
        return rect.y
      }
      getEndPosition = (rect: CanvasRectangle) => {
        return rect.y + rect.height
      }
      break
    default:
      const _exhaustiveCheck: never = direction
      throw new Error(`Unhandled direction of ${JSON.stringify(direction)}`)
  }

  const value =
    forwardOrReverse === 'forward'
      ? (getEndPosition(precedingSiblingPosition) + getStartPosition(succeedingSiblingPosition)) / 2
      : (getEndPosition(succeedingSiblingPosition) + getStartPosition(precedingSiblingPosition)) / 2

  return value
}

export interface SiblingPosition {
  frame: CanvasRectangle
  index: number
}

export function siblingAndPseudoPositions(
  parentFlexDirection: Direction,
  forwardsOrBackwards: ForwardOrReverse,
  parentRect: CanvasRectangle,
  siblingsOfTarget: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): Array<SiblingPosition> {
  const siblingsFiltered = siblingsOfTarget.filter((sibling) =>
    MetadataUtils.targetParticipatesInAutoLayout(metadata, sibling),
  )
  const siblingsPossiblyReversed =
    forwardsOrBackwards === 'forward' ? siblingsFiltered : reverse(siblingsFiltered)

  const pseudoElements = createPseudoElements(
    siblingsPossiblyReversed,
    parentFlexDirection,
    parentRect,
    metadata,
  )

  const siblingFramesAndIndexInAutoLayout = mapDropNulls((sibling, index) => {
    if (MetadataUtils.targetParticipatesInAutoLayout(metadata, sibling)) {
      return {
        frame: MetadataUtils.getFrameOrZeroRectInCanvasCoords(sibling, metadata) ?? zeroCanvasRect,
        index: index + 1,
      }
    } else {
      return null
    }
  }, siblingsOfTarget)

  if (forwardsOrBackwards === 'forward') {
    return [
      {
        frame: pseudoElements.before,
        index: 0,
      },
      ...siblingFramesAndIndexInAutoLayout,
      {
        frame: pseudoElements.after,
        index: siblingsOfTarget.length + 1,
      },
    ]
  } else {
    return [
      {
        frame: pseudoElements.after,
        index: 0,
      },
      ...siblingFramesAndIndexInAutoLayout,
      {
        frame: pseudoElements.before,
        index: siblingsOfTarget.length + 1,
      },
    ]
  }
}

function createPseudoElements(
  siblings: Array<ElementPath>,
  parentFlexDirection: Direction,
  parentFrame: CanvasRectangle,
  metadata: ElementInstanceMetadataMap,
): { before: CanvasRectangle; after: CanvasRectangle } {
  const firstElementPath = siblings[0]
  const lastElementPath = siblings[siblings.length - 1]

  const flexGap = MetadataUtils.getParentFlexGap(firstElementPath, metadata)

  const firstElementFrame =
    MetadataUtils.getFrameOrZeroRectInCanvasCoords(firstElementPath, metadata) ?? zeroCanvasRect
  const firstElementMargin = MetadataUtils.getElementMargin(firstElementPath, metadata)

  const lastElementFrame =
    MetadataUtils.getFrameOrZeroRectInCanvasCoords(lastElementPath, metadata) ?? zeroCanvasRect
  const lastElementMargin = MetadataUtils.getElementMargin(lastElementPath, metadata)

  if (parentFlexDirection === 'horizontal') {
    const marginLeftAndGapOffset = ((firstElementMargin?.left ?? 0) + flexGap) * 2
    const marginRightAndGapOffset = ((lastElementMargin?.right ?? 0) + flexGap) * 2
    return {
      before: canvasRectangle({
        x: Math.max(firstElementFrame.x - marginLeftAndGapOffset, parentFrame.x),
        y: firstElementFrame.y,
        width: 0,
        height: firstElementFrame.height,
      }),
      after: canvasRectangle({
        x: Math.min(
          lastElementFrame.x + lastElementFrame.width + marginRightAndGapOffset,
          parentFrame.x + parentFrame.width,
        ),
        y: lastElementFrame.y,
        width: 0,
        height: lastElementFrame.height,
      }),
    }
  } else {
    const marginTopAndGapOffset = ((firstElementMargin?.top ?? 0) + flexGap) * 2
    const marginBottomAndGapOffset = ((lastElementMargin?.bottom ?? 0) + flexGap) * 2

    return {
      before: canvasRectangle({
        x: firstElementFrame.x,
        y: Math.max(firstElementFrame.y - marginTopAndGapOffset, parentFrame.y),
        height: 0,
        width: firstElementFrame.width,
      }),
      after: canvasRectangle({
        x: lastElementFrame.x,
        y: Math.min(
          lastElementFrame.y + lastElementFrame.height + marginBottomAndGapOffset,
          parentFrame.y + parentFrame.height,
        ),
        height: 0,
        width: lastElementFrame.width,
      }),
    }
  }
}
