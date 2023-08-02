import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import Utils from '../../../../utils/utils'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import { isFiniteRectangle } from '../../../../core/shared/math-utils'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'

export function areYogaChildren(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
): boolean {
  if (selectedViews.length === 1) {
    return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      selectedViews[0],
      componentMetadata,
    )
  } else {
    return false
  }
}

export function anyInstanceYogaLayouted(
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
): boolean {
  return selectedViews.some((selectedView) => {
    return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      selectedView,
      componentMetadata,
    )
  })
}

export function getReorderDirection(flexDirection: string): 'horizontal' | 'vertical' {
  switch (flexDirection) {
    case 'row':
    case 'row-reverse':
      return 'horizontal'
    case 'column':
    case 'column-reverse':
    default:
      return 'vertical'
  }
}

export function isYogaReverse(flexDirection: string): boolean {
  switch (flexDirection) {
    case 'row-reverse':
    case 'column-reverse':
      return true
    case 'row':
    case 'column':
    default:
      return false
  }
}

export function getNewIndex(
  componentMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  target: ElementPath,
  parent: ElementPath | null,
  flexDirection: string,
  draggedFrame: CanvasRectangle,
): number | null {
  const yogaForwards = !isYogaReverse(flexDirection)

  // if the target is not yet a children of the parent, we set currentIndex to Infinity so the logic below acts as if it would be the last sibling
  let alreadyAChild: boolean = true
  let currentIndex: number = Infinity
  const viewZIndex = MetadataUtils.getIndexInParent(componentMetadata, elementPathTree, target)
  if (viewZIndex >= 0) {
    currentIndex = viewZIndex
  } else {
    alreadyAChild = false
  }

  // Note: includes the element we're moving.
  const siblings =
    parent == null
      ? []
      : MetadataUtils.getImmediateChildrenOrdered(componentMetadata, elementPathTree, parent)
  const siblingTPs = siblings.map((child) => child.elementPath)

  const yogaDirection = getReorderDirection(flexDirection)
  let resultIndexes: Array<number> = []
  Utils.fastForEach(siblingTPs, (sibling, index) => {
    const siblingFrame = MetadataUtils.getFrameInCanvasCoords(sibling, componentMetadata)
    if (siblingFrame != null && isFiniteRectangle(siblingFrame)) {
      // this sibling is non-layoutable, so it doesn't participate in the yoga layout
      // I hope it won't screw the logic up
      const targetBegin = yogaDirection === 'horizontal' ? draggedFrame.x : draggedFrame.y
      const targetEnd =
        targetBegin + (yogaDirection === 'horizontal' ? draggedFrame.width : draggedFrame.height)
      const ccPoint = Utils.getRectCenter(siblingFrame)
      const compareToCenter = yogaDirection === 'horizontal' ? ccPoint.x : ccPoint.y
      if (!alreadyAChild || index !== currentIndex) {
        const compareBeginEdge = yogaForwards ? index < currentIndex : index > currentIndex
        // check if the current frame's top is above any of the preceding frames' center
        if (compareBeginEdge && targetBegin < compareToCenter) {
          resultIndexes.push(index)
        }
        // or its bottom below any succeeding frames' center
        if (!compareBeginEdge && targetEnd > compareToCenter) {
          resultIndexes.push(index)
        }
      }
    }
  })

  if (resultIndexes.length > 0) {
    return yogaForwards ? resultIndexes[resultIndexes.length - 1] : resultIndexes[0]
  } else {
    return null
  }
}
