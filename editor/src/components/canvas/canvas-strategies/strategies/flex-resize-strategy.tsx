import { styleStringInArray } from '../../../../utils/common-constants'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  nullIfInfinity,
  zeroCanvasPoint,
} from '../../../../core/shared/math-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import {
  EdgePosition,
  EdgePositionBottom,
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
  EdgePositionTopLeft,
  EdgePositionTopRight,
} from '../../canvas-types'
import { SnappingThreshold } from '../../canvas-utils'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../../commands/adjust-css-length-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  InteractionLifecycle,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { honoursPropsSize } from './absolute-utils'
import {
  getLockedAspectRatio,
  isAnySelectedElementAspectRatioLocked,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
} from './resize-helpers'
import { cssKeyword, FlexDirection } from '../../../inspector/common/css-utils'
import { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import { detectFillHugFixedState, MaxContent } from '../../../inspector/inspector-common'
import * as EP from '../../../../core/shared/element-path'
import { deleteProperties } from '../../commands/delete-properties-command'
import { getElementDimensions } from './flex-resize-helpers'
import { setCssLengthProperty, setExplicitCssValue } from '../../commands/set-css-length-command'
import { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../guideline'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'

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

  return {
    id: FLEX_RESIZE_STRATEGY_ID,
    name: 'Flex Resize',
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: selectedElements },
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

          const makeResizeCommand = (
            name: 'width' | 'height' | 'flexBasis',
            elementDimension: number | null | undefined,
            original: number,
            resized: number,
            parent: number | undefined,
            parentFlexDirection: FlexDirection | null,
          ): AdjustCssLengthProperty[] => {
            return [
              adjustCssLengthProperty(
                'always',
                selectedElement,
                stylePropPathMappingFn(name, styleStringInArray),
                elementDimension != null ? resized - original : resized,
                parent,
                parentFlexDirection,
                'create-if-not-existing',
              ),
            ]
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
                  interactionSession.latestMetadata,
                )
              : null

          const snapToHug =
            lockedAspectRatio == null
              ? shouldSnapTohug(
                  edgePosition,
                  resizedBounds,
                  elementParentFlexDirection,
                  metadata,
                  canvasState.startingMetadata,
                )
              : null

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
                setProperty(
                  'always',
                  selectedElement,
                  stylePropPathMappingFn('flexGrow', styleStringInArray),
                  1,
                ),
              )
              resizeCommands.push(
                deleteProperties('always', selectedElement, [
                  stylePropPathMappingFn(widthPropToUse, styleStringInArray),
                ]),
              )
              if (snapToParentEdge.guideline != null) {
                resizeCommands.push(
                  setSnappingGuidelines('mid-interaction', [snapToParentEdge.guideline]),
                )
              }
            } else if (
              snapToHug != null &&
              snapToHug.snapDirection === 'horizontal' &&
              snapToHug.snap
            ) {
              resizeCommands.push(
                setCssLengthProperty(
                  'always',
                  selectedElement,
                  stylePropPathMappingFn(widthPropToUse, styleStringInArray),
                  setExplicitCssValue(cssKeyword(MaxContent)),
                  elementParentFlexDirection,
                ),
              )
              if (snapToHug.guideline != null) {
                resizeCommands.push(setSnappingGuidelines('mid-interaction', [snapToHug.guideline]))
              }
            } else {
              resizeCommands.push(
                ...makeResizeCommand(
                  widthPropToUse,
                  elementDimensionsProps?.[widthPropToUse],
                  originalBounds.width,
                  resizedBounds.width,
                  elementParentBounds?.width,
                  elementParentFlexDirection,
                ),
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
                setProperty(
                  'always',
                  selectedElement,
                  stylePropPathMappingFn('flexGrow', styleStringInArray),
                  1,
                ),
              )
              resizeCommands.push(
                deleteProperties('always', selectedElement, [
                  stylePropPathMappingFn(heightPropToUse, styleStringInArray),
                ]),
              )
              if (snapToParentEdge.guideline != null) {
                resizeCommands.push(
                  setSnappingGuidelines('mid-interaction', [snapToParentEdge.guideline]),
                )
              }
            } else if (
              snapToHug != null &&
              snapToHug.snapDirection === 'vertical' &&
              snapToHug.snap
            ) {
              resizeCommands.push(
                setCssLengthProperty(
                  'always',
                  selectedElement,
                  stylePropPathMappingFn(heightPropToUse, styleStringInArray),
                  setExplicitCssValue(cssKeyword(MaxContent)),
                  elementParentFlexDirection,
                ),
              )
              if (snapToHug.guideline != null) {
                resizeCommands.push(setSnappingGuidelines('mid-interaction', [snapToHug.guideline]))
              }
            } else {
              resizeCommands.push(
                ...makeResizeCommand(
                  heightPropToUse,
                  elementDimensionsProps?.[heightPropToUse],
                  originalBounds.height,
                  resizedBounds.height,
                  elementParentBounds?.height,
                  elementParentFlexDirection,
                ),
              )
            }
          }
          if (snapToParentEdge != null && !snapToParentEdge.snap) {
            resizeCommands.push(
              deleteProperties('always', selectedElement, [
                stylePropPathMappingFn('flexGrow', styleStringInArray),
              ]),
            )
          }

          return strategyApplicationResult([
            ...resizeCommands,
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
            setElementsToRerenderCommand(selectedElements),
          ])
        } else {
          return strategyApplicationResult([
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
          ])
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

function shouldSnapToParentEdge(
  edgePosition: EdgePosition,
  resizedBounds: CanvasRectangle,
  parentBounds: CanvasRectangle | null,
  parentFlexDirection: FlexDirection | null,
  element: ElementInstanceMetadata,
  startingMetadata: ElementInstanceMetadataMap,
  latestMetadata: ElementInstanceMetadataMap,
): {
  snapDirection: 'horizontal' | 'vertical'
  snap: boolean
  guideline: GuidelineWithSnappingVectorAndPointsOfRelevance | null
} | null {
  const parentPadding = element.specialSizeMeasurements.parentPadding
  const parentJustifyContent = element.specialSizeMeasurements.parentJustifyContent
  const parentGap = element.specialSizeMeasurements.parentFlexGap

  const flexSiblingsWithoutSelected = MetadataUtils.getSiblingsUnordered(
    startingMetadata,
    element.elementPath,
  )
    .filter((sibling) => !EP.pathsEqual(sibling.elementPath, element.elementPath))
    .filter(MetadataUtils.elementParticipatesInAutoLayout)

  const anySiblingFillSized = flexSiblingsWithoutSelected.some((sibling) => {
    const fillHugFixedState = detectFillHugFixedState(
      parentFlexDirection === 'row' ? 'horizontal' : 'vertical',
      startingMetadata,
      sibling.elementPath,
    )
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
    element.elementPath,
  ).findIndex((sibling) => EP.pathsEqual(element.elementPath, sibling.elementPath))
  const isFirstSibling = siblingIndex === 0
  const isLastSibling = siblingIndex === flexSiblingsWithoutSelected.length

  const parentInnerBounds = canvasRectangle({
    x: parentBounds.x + (parentPadding.left ?? 0),
    y: parentBounds.y + (parentPadding.top ?? 0),
    width: parentBounds.width - ((parentPadding.left ?? 0) + (parentPadding.right ?? 0)),
    height: parentBounds.height - ((parentPadding.top ?? 0) + (parentPadding.bottom ?? 0)),
  })

  const direction = parentFlexDirection === 'row' ? 'horizontal' : 'vertical'

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
    const shouldSnap = (() => {
      if (direction === 'horizontal') {
        // positive free space is calculated with sibling frames, gap and paddings
        const siblingsWidth = siblingFrames.reduce((working, frame) => {
          return frame != null && isFiniteRectangle(frame) ? frame.width + working : working
        }, 0)
        const siblingSize = siblingsWidth + siblingFrames.length * parentGap
        return (
          siblingSize < parentInnerBounds.width && // there is open space in the layout
          Math.abs(siblingSize + resizedBounds.width - parentInnerBounds.width) <= SnappingThreshold
        )
      } else {
        // positive free space is calculated with sibling frames, gap and paddings
        const siblingsHeight = siblingFrames.reduce((working, frame) => {
          return frame != null && isFiniteRectangle(frame) ? frame.height + working : working
        }, 0)
        const siblingSize = siblingsHeight + siblingFrames.length * parentGap
        return (
          siblingSize < parentInnerBounds.height && // there is open space in the layout
          Math.abs(siblingSize + resizedBounds.height - parentInnerBounds.height) <=
            SnappingThreshold
        )
      }
    })()
    const guideline = shouldSnap
      ? collectGuideline(edgePosition, direction, parentBounds, resizedBounds)
      : null
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

function shouldSnapTohug(
  edgePosition: EdgePosition,
  resizedBounds: CanvasRectangle,
  elementParentFlexDirection: FlexDirection | null,
  element: ElementInstanceMetadata,
  startingMetadata: ElementInstanceMetadataMap,
): {
  snapDirection: 'horizontal' | 'vertical'
  snap: boolean
  guideline: GuidelineWithSnappingVectorAndPointsOfRelevance | null
} | null {
  if (MetadataUtils.isFlexLayoutedContainer(element)) {
    const elementFlexDirection = element.specialSizeMeasurements.flexDirection
    const children = MetadataUtils.getChildrenUnordered(startingMetadata, element.elementPath)
    const areAllChildrenFixed = children.every((child) => {
      const fillHugFixedState = detectFillHugFixedState(
        elementParentFlexDirection === 'row' ? 'horizontal' : 'vertical',
        startingMetadata,
        child.elementPath,
      )
      return fillHugFixedState?.type === 'fixed'
    })
    if (!areAllChildrenFixed) {
      return null
    }

    const direction = elementParentFlexDirection === 'row' ? 'horizontal' : 'vertical'
    const resizeDirection = dimensionToSetForEdgePosition(edgePosition)

    if (
      (direction === 'horizontal' && resizeDirection.width) ||
      (direction === 'vertical' && resizeDirection.height)
    ) {
      const shouldSnap = (() => {
        const childrenFrames = children
          .filter(MetadataUtils.elementParticipatesInAutoLayout)
          .map((child) => nullIfInfinity(child.globalFrame))
        const gap = element.specialSizeMeasurements.gap ?? 0

        if (direction === 'horizontal') {
          const paddingSize =
            (element.specialSizeMeasurements.padding.left ?? 0) +
            (element.specialSizeMeasurements.padding.right ?? 0)
          let childrenSize = 0
          if (elementParentFlexDirection === elementFlexDirection) {
            // the element is in a row and it has children in a row it snaps on to all children + gaps + paddings
            const childrenWidth = childrenFrames.reduce(
              (size, child) => size + (child?.width ?? 0),
              0,
            )
            childrenSize = childrenWidth + gap * children.length + paddingSize
          } else {
            // when the element is in row and it has children in a column only the widest child is needed and paddings
            const maxChildWidth = Math.max(...childrenFrames.map((child) => child?.width ?? 0))
            childrenSize = maxChildWidth + paddingSize
          }

          return Math.abs(resizedBounds.width - childrenSize) <= SnappingThreshold
        } else {
          const paddingSize =
            (element.specialSizeMeasurements.padding.top ?? 0) +
            (element.specialSizeMeasurements.padding.bottom ?? 0)
          let childrenSize = 0
          if (elementParentFlexDirection === elementFlexDirection) {
            // the element is in a column and it has children in a column it snaps on to all children + gaps + paddings
            const childrenHeight = childrenFrames.reduce(
              (size, child) => size + (child?.height ?? 0),
              0,
            )
            childrenSize = childrenHeight + gap * children.length + paddingSize
          } else {
            // when the element is in column and it has children in a row only the tallest child element is needed and paddings
            const maxChildHeight = Math.max(...childrenFrames.map((child) => child?.height ?? 0))
            childrenSize = maxChildHeight + paddingSize
          }
          return Math.abs(resizedBounds.height - childrenSize) <= SnappingThreshold
        }
      })()

      const guideline = shouldSnap
        ? collectChildGuideline(edgePosition, direction, element, startingMetadata, resizedBounds)
        : null

      return {
        snapDirection: direction,
        snap: shouldSnap,
        guideline: guideline,
      }
    }
  }

  return null
}

/** show guideline on snap with small x marks on child corners and resized element corners */
function collectChildGuideline(
  edgePosition: EdgePosition,
  direction: 'horizontal' | 'vertical',
  element: ElementInstanceMetadata,
  metadata: ElementInstanceMetadataMap,
  resizedBounds: CanvasRectangle,
): GuidelineWithSnappingVectorAndPointsOfRelevance | null {
  const children = MetadataUtils.getChildrenOrdered(metadata, element.elementPath).filter(
    MetadataUtils.elementParticipatesInAutoLayout,
  )
  let childIndex = 0
  if (direction === 'horizontal') {
    childIndex = edgePosition.x === 0 ? 0 : children.length - 1
  } else {
    childIndex = edgePosition.y === 0 ? 0 : children.length - 1
  }

  const childFrame = children.length > 0 ? nullIfInfinity(children[childIndex].globalFrame) : null
  if (childFrame != null) {
    return collectGuideline(edgePosition, direction, childFrame, resizedBounds)
  }
  return null
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
