import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { offsetPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { reorderElement } from '../commands/reorder-element-command'
import { CanvasStrategy } from './canvas-strategy-types'
import {
  CanvasPoint,
  canvasPoint,
  CanvasVector,
  CanvasRectangle,
} from '../../../core/shared/math-utils'
import * as EP from '../../../core/shared/element-path'
import { reverse, stripNulls } from '../../../core/shared/array-utils'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'

export const flexReorderStrategy: CanvasStrategy = {
  id: 'FLEX_REORDER',
  name: 'Flex Reorder',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length == 1) {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        canvasState.selectedElements[0],
        metadata,
      )
    } else {
      return false
    }
  },
  controlsToRender: [
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    return flexReorderStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type !== 'DRAG' ||
      interactionState.interactionData.drag == null
    ) {
      return {
        commands: [],
        customState: null,
      }
    }
    const { selectedElements } = canvasState
    const target = selectedElements[0]

    const siblingsOfTarget = MetadataUtils.getSiblings(sessionState.startingMetadata, target).map(
      (element) => element.elementPath,
    )

    const pointOnCanvas = offsetPoint(
      interactionState.interactionData.dragStart,
      interactionState.interactionData.drag,
    )
    const oldIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
    const newIndex = getReorderIndex(
      sessionState.startingMetadata,
      siblingsOfTarget,
      target,
      pointOnCanvas,
    )
    if (newIndex == null || newIndex === oldIndex) {
      return {
        commands: [],
        customState: null,
      }
    }

    return {
      commands: [reorderElement('permanent', target, newIndex)],
      customState: null,
    }
  },
}

function getReorderIndex(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
  target: ElementPath,
  point: CanvasVector,
) {
  const flexSiblingsOfTarget = siblings.filter((child) =>
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(child, metadata),
  )
  const newIndexAmongFlexSiblings = getReorderIndexAmongFlexSiblings(
    metadata,
    flexSiblingsOfTarget,
    target,
    point,
  )

  if (newIndexAmongFlexSiblings == null) {
    return null
  } else {
    const elementToReplaceWith = flexSiblingsOfTarget[newIndexAmongFlexSiblings]
    const newIndex = siblings.indexOf(elementToReplaceWith)
    return newIndex > -1 ? newIndex : null
  }
}

function getReorderIndexAmongFlexSiblings(
  metadata: ElementInstanceMetadataMap,
  siblings: Array<ElementPath>,
  target: ElementPath,
  point: CanvasVector,
) {
  if (siblings.length === 0) {
    return null
  }
  const parentPath = EP.parentPath(target)
  const parent = MetadataUtils.findElementByElementPath(metadata, parentPath)
  const flexDirection = parent?.specialSizeMeasurements.flexDirection

  const isLeftToRight = flexDirection === 'row' || flexDirection === 'column'

  const frames = stripNulls(
    siblings.map((sibling) => MetadataUtils.getFrameInCanvasCoords(sibling, metadata)),
  )
  const framesLeftToRight = isLeftToRight ? frames : reverse(frames)

  // We compare the current mouse position to these points, when crossing a separator
  // the strategy switches to a different index. The order is from left to right.
  const separatorPoints = getSeparatorPoints(framesLeftToRight)

  const coordToCheck = flexDirection === 'row' || flexDirection == 'row-reverse' ? 'x' : 'y'

  const isBeforeFirst = point[coordToCheck] <= separatorPoints[0][coordToCheck]
  if (isBeforeFirst) {
    return reverseIndexIfRightToLeft(0, siblings.length, isLeftToRight)
  }
  const isAfterLast =
    point[coordToCheck] >= separatorPoints[separatorPoints.length - 1][coordToCheck]
  if (isAfterLast) {
    return reverseIndexIfRightToLeft(siblings.length - 1, siblings.length, isLeftToRight)
  }

  for (let i = 0; i < separatorPoints.length - 1; i++) {
    const isBetweenCurrentAndNext =
      i < separatorPoints.length - 1 &&
      point[coordToCheck] >= separatorPoints[i][coordToCheck] &&
      point[coordToCheck] <= separatorPoints[i + 1][coordToCheck]
    if (isBetweenCurrentAndNext) {
      return reverseIndexIfRightToLeft(i, siblings.length, isLeftToRight)
    }
  }

  return null
}

function getSeparatorPoints(frames: Array<CanvasRectangle>): Array<CanvasPoint> {
  if (frames.length === 0) {
    return []
  }
  const firstFrame = frames[0]
  const lastFrame = frames[frames.length - 1]

  // First point is top left of the first frame
  const firstPoint = canvasPoint({ x: firstFrame.x, y: firstFrame.y })

  // Last point is bottom right of the last frame
  const lastPoint = canvasPoint({
    x: lastFrame.x + lastFrame.width,
    y: lastFrame.y + lastFrame.height,
  })

  // Middle points are in the center between the ith frame bottom right and (i+1)th frame top left corner
  // This way the reordering happens when the mouse is in the middle of the gap between the two frames, regardless of whether it is a row or a column layout
  let middlePoints: Array<CanvasPoint> = []
  for (let i = 0; i < frames.length - 1; i++) {
    middlePoints.push(
      canvasPoint({
        x: (frames[i].x + frames[i].width + frames[i + 1].x) / 2,
        y: (frames[i].y + frames[i].height + frames[i + 1].y) / 2,
      }),
    )
  }
  return [firstPoint, ...middlePoints, lastPoint]
}

function reverseIndexIfRightToLeft(idx: number, length: number, isLeftToRight: boolean) {
  return isLeftToRight ? idx : length - 1 - idx
}
