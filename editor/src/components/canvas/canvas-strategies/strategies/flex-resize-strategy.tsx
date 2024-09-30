import { styleStringInArray } from '../../../../utils/common-constants'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  nullIfInfinity,
  zeroCanvasPoint,
  boundingRectangleArray,
} from '../../../../core/shared/math-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import type { EdgePosition } from '../../canvas-types'
import { SnappingThreshold } from '../../canvas-utils'
import type { LengthPropertyToAdjust } from '../../commands/adjust-css-length-command'
import {
  AdjustCssLengthProperties,
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import { setCursorCommand } from '../../commands/set-cursor-command'

import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import type {
  CanvasStrategy,
  InteractionCanvasState,
  InteractionLifecycle,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { honoursPropsSize } from './absolute-utils'
import {
  getLockedAspectRatio,
  isAnySelectedElementAspectRatioLocked,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
} from './resize-helpers'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import { cssKeyword } from '../../../inspector/common/css-utils'
import type { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import { detectFillHugFixedState, MaxContent } from '../../../inspector/inspector-common'
import * as EP from '../../../../core/shared/element-path'
import { deleteProperties } from '../../commands/delete-properties-command'
import { getElementDimensions } from './flex-resize-helpers'
import { setCssLengthProperty, setExplicitCssValue } from '../../commands/set-css-length-command'
import type { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../guideline'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { strictEvery, mapDropNulls } from '../../../../core/shared/array-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { treatElementAsGroupLike } from './group-helpers'
import { queueTrueUpElement } from '../../commands/queue-true-up-command'
import { pushIntendedBoundsAndUpdateGroups } from '../../commands/push-intended-bounds-and-update-groups-command'
import { trueUpGroupElementChanged } from '../../../editor/store/editor-state'

export const FLEX_RESIZE_STRATEGY_ID = 'FLEX_RESIZE'

export function flexResizeStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  if (
    selectedElements.length !== 1 ||
    !MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      selectedElements[0],
      canvasState.startingMetadata,
    ) ||
    !honoursPropsSize(canvasState, selectedElements[0])
  ) {
    return null
  }
  const target = selectedElements[0]
  const metadata = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, target)

  const elementDimensionsProps = metadata != null ? getElementDimensions(metadata) : null
  const elementParentBounds = metadata?.specialSizeMeasurements.immediateParentBounds ?? null
  const elementParentFlexDirection = metadata?.specialSizeMeasurements.parentFlexDirection ?? null

  const widthPropToUse: 'flexBasis' | 'width' =
    (elementParentFlexDirection === 'row' || elementParentFlexDirection === 'row-reverse') &&
    elementDimensionsProps?.flexBasis != null
      ? 'flexBasis'
      : 'width'
  const heightPropToUse: 'flexBasis' | 'height' =
    (elementParentFlexDirection === 'column' || elementParentFlexDirection === 'column-reverse') &&
    elementDimensionsProps?.flexBasis != null
      ? 'flexBasis'
      : 'height'

  const hasSizedParent =
    elementParentBounds != null &&
    elementParentBounds.width !== 0 &&
    elementParentBounds.height !== 0

  if (MetadataUtils.isGridCell(canvasState.startingMetadata, selectedElements[0])) {
    return null
  }

  return {
    id: FLEX_RESIZE_STRATEGY_ID,
    name: 'Flex Resize',
    descriptiveLabel: 'Resizing Flex Elements',
    icon: {
      category: 'modalities',
      type: 'resize',
    },
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: selectedElements, pathsWereReplaced: false },
        key: 'absolute-resize-control',
        show: 'always-visible',
      }),
      controlWithProps({
        control: ZeroSizeResizeControlWrapper,
        props: { targets: selectedElements },
        key: 'zero-size-resize-control',
        show: 'always-visible',
      }),
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: selectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: selectedElements },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'RESIZE_HANDLE' &&
      hasSizedParent
        ? 2
        : 0,
    apply: (_strategyLifecycle: InteractionLifecycle) => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE'
      ) {
        // no multiselection support yet
        const selectedElement = selectedElements[0]
        const edgePosition = interactionSession.activeControl.edgePosition
        if (interactionSession.interactionData.drag != null) {
          const drag = interactionSession.interactionData.drag
          const originalBounds = MetadataUtils.getFrameInCanvasCoords(
            selectedElement,
            canvasState.startingMetadata,
          )

          if (originalBounds == null || isInfinityRectangle(originalBounds)) {
            return emptyStrategyApplicationResult
          }

          if (metadata == null) {
            return emptyStrategyApplicationResult
          }

          const anySelectedElementAspectRatioLocked = isAnySelectedElementAspectRatioLocked(
            canvasState.startingMetadata,
            [selectedElement],
          )
          const lockedAspectRatio = getLockedAspectRatio(
            interactionSession,
            interactionSession.interactionData.modifiers,
            originalBounds,
            anySelectedElementAspectRatioLocked,
          )

          const parentMetadata = MetadataUtils.getParent(
            canvasState.startingMetadata,
            metadata.elementPath,
          )
          const isCenterBasedResize =
            parentMetadata?.specialSizeMeasurements.alignItems === 'center'
          const resizedBounds = resizeBoundingBox(
            originalBounds,
            drag,
            edgePosition,
            lockedAspectRatio,
            isCenterBasedResize ? 'center-based' : 'non-center-based',
          )

          const elementIsGroup = treatElementAsGroupLike(
            canvasState.startingMetadata,
            selectedElement,
          )
          const groupChildren = elementIsGroup
            ? MetadataUtils.getChildrenUnordered(canvasState.startingMetadata, selectedElement)
            : []

          let lengthPropertiesToAdjust: Array<LengthPropertyToAdjust> = []
          function addResizeProperty(
            name: 'width' | 'height' | 'flexBasis',
            elementDimension: number | null | undefined,
            original: number,
            resized: number,
            parent: number | undefined,
          ): void {
            lengthPropertiesToAdjust.push(
              lengthPropertyToAdjust(
                stylePropPathMappingFn(name, styleStringInArray),
                elementDimension != null ? resized - original : resized,
                parent,
                'create-if-not-existing',
              ),
            )
          }

          const snapToParentEdge =
            lockedAspectRatio == null
              ? shouldSnapToParentEdge(
                  edgePosition,
                  resizedBounds,
                  elementParentBounds,
                  elementParentFlexDirection,
                  metadata,
                  canvasState.startingMetadata,
                  canvasState.startingElementPathTree,
                  interactionSession.latestMetadata,
                  interactionSession.latestElementPathTree,
                )
              : null

          const snapToHug =
            lockedAspectRatio == null
              ? snapToHugChildren(
                  edgePosition,
                  resizedBounds,
                  elementParentFlexDirection,
                  metadata,
                  canvasState.startingMetadata,
                  canvasState.startingElementPathTree,
                )
              : null

          // only update width or height based on dragged edge/corner
          const dimensionToUpdate =
            lockedAspectRatio != null
              ? { width: true, height: true }
              : dimensionToSetForEdgePosition(edgePosition)

          let resizeCommands: Array<CanvasCommand> = []
          if (dimensionToUpdate.width) {
            if (
              snapToParentEdge != null &&
              snapToParentEdge.snapDirection === 'horizontal' &&
              snapToParentEdge.snap
            ) {
              resizeCommands.push(
                ...getFillCommands(selectedElement, widthPropToUse, snapToParentEdge.guideline),
              )
            } else if (
              snapToHug != null &&
              snapToHug.snapDirection === 'horizontal' &&
              snapToHug.isSnapping
            ) {
              resizeCommands.push(
                ...getHugCommands(
                  selectedElement,
                  widthPropToUse,
                  snapToHug.guideline,
                  elementParentFlexDirection,
                ),
              )
            } else {
              addResizeProperty(
                widthPropToUse,
                elementDimensionsProps?.[widthPropToUse],
                originalBounds.width,
                resizedBounds.width,
                elementParentBounds?.width,
              )
            }
          }
          if (dimensionToUpdate.height) {
            if (
              snapToParentEdge != null &&
              snapToParentEdge.snapDirection === 'vertical' &&
              snapToParentEdge.snap
            ) {
              resizeCommands.push(
                ...getFillCommands(selectedElement, heightPropToUse, snapToParentEdge.guideline),
              )
            } else if (
              snapToHug != null &&
              snapToHug.snapDirection === 'vertical' &&
              snapToHug.isSnapping
            ) {
              resizeCommands.push(
                ...getHugCommands(
                  selectedElement,
                  heightPropToUse,
                  snapToHug.guideline,
                  elementParentFlexDirection,
                ),
              )
            } else {
              addResizeProperty(
                heightPropToUse,
                elementDimensionsProps?.[heightPropToUse],
                originalBounds.height,
                resizedBounds.height,
                elementParentBounds?.height,
              )
            }
          }

          resizeCommands.push(
            adjustCssLengthProperties(
              'always',
              selectedElement,
              elementParentFlexDirection,
              lengthPropertiesToAdjust,
            ),
          )

          if (snapToParentEdge != null && !snapToParentEdge.snap) {
            resizeCommands.push(
              deleteProperties('always', selectedElement, [
                stylePropPathMappingFn('flexGrow', styleStringInArray),
              ]),
            )
          }

          const newFrame = resizeBoundingBox(
            originalBounds,
            drag,
            edgePosition,
            lockedAspectRatio,
            'center-based',
          )

          return strategyApplicationResult(
            [
              ...resizeCommands,
              updateHighlightedViews('mid-interaction', []),
              setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
              pushIntendedBoundsAndUpdateGroups(
                [{ target: selectedElement, frame: newFrame }],
                'starting-metadata',
              ),
              ...groupChildren.map((c) =>
                queueTrueUpElement([trueUpGroupElementChanged(c.elementPath)]),
              ),
            ],
            selectedElements,
          )
        } else {
          return strategyApplicationResult(
            [
              updateHighlightedViews('mid-interaction', []),
              setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
            ],
            [],
          )
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

function getFillCommands(
  selectedElement: ElementPath,
  propToUpdate: 'width' | 'height' | 'flexBasis',
  guideline: GuidelineWithSnappingVectorAndPointsOfRelevance,
): Array<CanvasCommand> {
  return [
    setProperty(
      'always',
      selectedElement,
      stylePropPathMappingFn('flexGrow', styleStringInArray),
      1,
    ),
    deleteProperties('always', selectedElement, [
      stylePropPathMappingFn(propToUpdate, styleStringInArray),
    ]),
    setSnappingGuidelines('mid-interaction', [guideline]),
  ]
}

function getHugCommands(
  selectedElement: ElementPath,
  propToUpdate: 'width' | 'height' | 'flexBasis',
  guideline: GuidelineWithSnappingVectorAndPointsOfRelevance,
  elementParentFlexDirection: FlexDirection | null,
): Array<CanvasCommand> {
  return [
    setCssLengthProperty(
      'always',
      selectedElement,
      stylePropPathMappingFn(propToUpdate, styleStringInArray),
      setExplicitCssValue(cssKeyword(MaxContent)),
      elementParentFlexDirection,
    ),
    setSnappingGuidelines('mid-interaction', [guideline]),
  ]
}

function shouldSnapToParentEdge(
  edgePosition: EdgePosition,
  resizedBounds: CanvasRectangle,
  parentBounds: CanvasRectangle | null,
  parentFlexDirection: FlexDirection | null,
  element: ElementInstanceMetadata,
  startingMetadata: ElementInstanceMetadataMap,
  startingPathTrees: ElementPathTrees,
  latestMetadata: ElementInstanceMetadataMap,
  latestPathTrees: ElementPathTrees,
): {
  snapDirection: 'horizontal' | 'vertical'
  snap: boolean
  guideline: GuidelineWithSnappingVectorAndPointsOfRelevance
} | null {
  const parentPadding = element.specialSizeMeasurements.parentPadding
  const parentJustifyContent = element.specialSizeMeasurements.parentJustifyContent
  const parentGap = element.specialSizeMeasurements.parentFlexGap
  const direction = parentFlexDirection === 'row' ? 'horizontal' : 'vertical'

  const flexSiblingsWithoutSelected = MetadataUtils.getSiblingsOrdered(
    startingMetadata,
    startingPathTrees,
    element.elementPath,
  )
    .filter((sibling) => !EP.pathsEqual(sibling.elementPath, element.elementPath))
    .filter(MetadataUtils.elementParticipatesInAutoLayout)

  const anySiblingFillSized = flexSiblingsWithoutSelected.some((sibling) => {
    const fillHugFixedState = detectFillHugFixedState(
      direction,
      startingMetadata,
      sibling.elementPath,
    ).fixedHugFill
    return fillHugFixedState?.type === 'fill'
  })

  if (anySiblingFillSized || element.specialSizeMeasurements.parentHugsOnMainAxis) {
    return null
  }

  if (parentFlexDirection === 'row-reverse' || parentFlexDirection === 'column-reverse') {
    return null
  }

  if (parentBounds == null) {
    return null
  }

  const siblingFrames = flexSiblingsWithoutSelected.map((sibling) =>
    MetadataUtils.getFrameInCanvasCoords(sibling.elementPath, startingMetadata),
  )

  // only this fn uses latestMetadata because on insertion the siblings are not ordered correctly based on the startingmetadata
  const siblingIndex = MetadataUtils.getSiblingsOrdered(
    latestMetadata,
    latestPathTrees,
    element.elementPath,
  ).findIndex((sibling) => EP.pathsEqual(element.elementPath, sibling.elementPath))
  const isFirstSibling = siblingIndex === 0
  const isLastSibling = siblingIndex === flexSiblingsWithoutSelected.length

  // snap to fill can be applied only to the element closest to the parent edge dragged on the closest side/corner
  const isDraggingRightEdgeOrCorner = edgePosition.x === 1
  const isDraggingBottomEdgeOrCorner = edgePosition.y === 1
  const isLastEdgeOrCorner =
    isLastSibling &&
    ((isDraggingRightEdgeOrCorner && direction === 'horizontal') ||
      (isDraggingBottomEdgeOrCorner && direction === 'vertical'))

  const isDraggingLeftEdgeOrCorner = edgePosition.x === 0
  const isDraggingTopEdgeOrCorner = edgePosition.y === 0

  const isFirstEdgeOrCorner =
    isFirstSibling &&
    ((isDraggingLeftEdgeOrCorner && direction === 'horizontal') ||
      (isDraggingTopEdgeOrCorner && direction === 'vertical'))

  const isElementDraggedOnOuterEdgeOrCorner =
    ((parentJustifyContent == null || parentJustifyContent === 'flex-start') &&
      isLastEdgeOrCorner) ||
    (parentJustifyContent === 'center' && (isLastEdgeOrCorner || isFirstEdgeOrCorner)) ||
    (parentJustifyContent === 'flex-end' && isFirstEdgeOrCorner)

  if (isElementDraggedOnOuterEdgeOrCorner) {
    const parentInnerBounds = canvasRectangle({
      x: parentBounds.x + (parentPadding.left ?? 0),
      y: parentBounds.y + (parentPadding.top ?? 0),
      width: parentBounds.width - ((parentPadding.left ?? 0) + (parentPadding.right ?? 0)),
      height: parentBounds.height - ((parentPadding.top ?? 0) + (parentPadding.bottom ?? 0)),
    })
    const dimensionToUse = direction === 'horizontal' ? 'width' : 'height'

    // positive free space is calculated with sibling frames, gap and paddings
    const siblingsSize = siblingFrames.reduce((working, frame) => {
      return frame != null && isFiniteRectangle(frame) ? frame[dimensionToUse] + working : working
    }, 0)
    const siblingSize = siblingsSize + siblingFrames.length * parentGap
    const shouldSnap =
      siblingSize < parentInnerBounds[dimensionToUse] && // there is open space in the layout
      Math.abs(siblingSize + resizedBounds[dimensionToUse] - parentInnerBounds[dimensionToUse]) <=
        SnappingThreshold

    const guideline = collectGuideline(edgePosition, direction, parentBounds, resizedBounds)

    return {
      snapDirection: direction,
      snap: shouldSnap,
      guideline: guideline,
    }
  }

  return null
}

function dimensionToSetForEdgePosition(edgePosition: EdgePosition): {
  width: boolean
  height: boolean
} {
  return {
    width: edgePosition.x === 0 || edgePosition.x === 1,
    height: edgePosition.y === 0 || edgePosition.y === 1,
  }
}

function snapToHugChildren(
  edgePosition: EdgePosition,
  resizedBounds: CanvasRectangle,
  parentFlexDirection: FlexDirection | null,
  element: ElementInstanceMetadata,
  startingMetadata: ElementInstanceMetadataMap,
  startingPathTrees: ElementPathTrees,
): {
  snapDirection: 'horizontal' | 'vertical'
  isSnapping: boolean
  guideline: GuidelineWithSnappingVectorAndPointsOfRelevance
} | null {
  if (MetadataUtils.isFlexLayoutedContainer(element)) {
    const direction = parentFlexDirection === 'row' ? 'horizontal' : 'vertical'

    const children = MetadataUtils.getChildrenOrdered(
      startingMetadata,
      startingPathTrees,
      element.elementPath,
    ).filter(MetadataUtils.elementParticipatesInAutoLayout)
    const childrenFrames = mapDropNulls((child) => nullIfInfinity(child.globalFrame), children)
    const childrenBoundingBox = boundingRectangleArray(childrenFrames)

    const areAllChildrenFixed = strictEvery(children, (child) => {
      const fillHugFixedState = detectFillHugFixedState(
        direction,
        startingMetadata,
        child.elementPath,
      ).fixedHugFill
      return fillHugFixedState?.type === 'fixed'
    })
    if (!areAllChildrenFixed || childrenBoundingBox == null) {
      return null
    }

    const resizeDirection = dimensionToSetForEdgePosition(edgePosition)
    if (
      (direction === 'horizontal' && resizeDirection.width) ||
      (direction === 'vertical' && resizeDirection.height)
    ) {
      const snapResult = isSnappingToChildren(
        direction,
        element,
        childrenFrames,
        resizedBounds,
        parentFlexDirection,
      )

      const guideline = collectGuideline(
        edgePosition,
        direction,
        childrenBoundingBox,
        resizedBounds,
      )

      return {
        snapDirection: direction,
        isSnapping: snapResult,
        guideline: guideline,
      }
    }
  }

  return null
}

function isSnappingToChildren(
  direction: 'vertical' | 'horizontal',
  element: ElementInstanceMetadata,
  childrenFrames: Array<CanvasRectangle>,
  resizedBounds: CanvasRectangle,
  parentFlexDirection: FlexDirection | null,
): boolean {
  const elementFlexDirection = element.specialSizeMeasurements.flexDirection
  const gap = element.specialSizeMeasurements.gap ?? 0

  const elementPadding = element.specialSizeMeasurements.padding
  const paddingLeftAndRight = (elementPadding.left ?? 0) + (elementPadding.right ?? 0)
  const paddingTopAndBottom = (elementPadding.top ?? 0) + (elementPadding.bottom ?? 0)

  const dimensionToUse = direction === 'horizontal' ? 'width' : 'height'
  const paddingsMatchingDirection =
    direction === 'horizontal' ? paddingLeftAndRight : paddingTopAndBottom

  const childrenSizeWithGapAndPadding = (() => {
    if (parentFlexDirection === elementFlexDirection) {
      // the element is in a row and it has children in a row it snaps on to all children + gaps + paddings
      // same when using column plus the element is flex column
      const childrenSize = childrenFrames.reduce(
        (size, child) => size + (child?.[dimensionToUse] ?? 0),
        0,
      )
      return childrenSize + gap * childrenFrames.length + paddingsMatchingDirection
    } else {
      // when the element is in row and it has children in a column only the widest child is needed and paddings
      // same when using column plus the element is flex column, then it needs only the tallest child
      const maxSize = Math.max(...childrenFrames.map((child) => child?.[dimensionToUse] ?? 0))
      return maxSize + paddingsMatchingDirection
    }
  })()

  return (
    Math.abs(resizedBounds[dimensionToUse] - childrenSizeWithGapAndPadding) <= SnappingThreshold
  )
}

/** show guideline on snap to fill with small x marks positioned on parent corners and resized element corners */
/** show guideline on snap to hug with small x marks positioned on child corners and resized element corners */
function collectGuideline(
  edgePosition: EdgePosition,
  direction: 'horizontal' | 'vertical',
  frameA: CanvasRectangle,
  frameB: CanvasRectangle,
): GuidelineWithSnappingVectorAndPointsOfRelevance {
  if (direction === 'horizontal') {
    const guidelinePositionX = edgePosition.x === 0 ? frameA.x : frameA.x + frameA.width
    return {
      snappingVector: zeroCanvasPoint,
      guideline: {
        type: 'XAxisGuideline',
        x: guidelinePositionX,
        yTop: Math.min(frameA.y, frameB.y),
        yBottom: Math.max(frameA.y + frameA.height, frameB.y + frameB.height),
      },
      pointsOfRelevance: [
        ...getHorizontalGuidelineMarkerPoints(guidelinePositionX, frameA),
        ...getHorizontalGuidelineMarkerPoints(guidelinePositionX, frameB),
      ],
    } as GuidelineWithSnappingVectorAndPointsOfRelevance
  } else {
    const guidelinePositionY = edgePosition.y === 0 ? frameA.y : frameA.y + frameA.height
    return {
      snappingVector: zeroCanvasPoint,
      guideline: {
        type: 'YAxisGuideline',
        y: guidelinePositionY,
        xLeft: Math.min(frameA.x, frameB.x),
        xRight: Math.max(frameA.x + frameA.width, frameB.x + frameB.width),
      },
      pointsOfRelevance: [
        ...getVerticalGuidelineMarkerPoints(guidelinePositionY, frameA),
        ...getVerticalGuidelineMarkerPoints(guidelinePositionY, frameB),
      ],
    } as GuidelineWithSnappingVectorAndPointsOfRelevance
  }
}

function getHorizontalGuidelineMarkerPoints(
  positionX: number,
  frame: CanvasRectangle,
): Array<CanvasPoint> {
  return [
    canvasPoint({
      x: positionX,
      y: frame.y,
    }),
    canvasPoint({
      x: positionX,
      y: frame.y + frame.height,
    }),
  ]
}

function getVerticalGuidelineMarkerPoints(
  positionY: number,
  frame: CanvasRectangle,
): Array<CanvasPoint> {
  return [
    canvasPoint({
      x: frame.x,
      y: positionY,
    }),
    canvasPoint({
      x: frame.x + frame.width,
      y: positionY,
    }),
  ]
}
