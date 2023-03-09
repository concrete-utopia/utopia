import { styleStringInArray } from '../../../../utils/common-constants'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import {
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
              ? shouldSnapTohug(edgePosition, resizedBounds, metadata, canvasState.startingMetadata)
              : null

          const dimensionToUpdate =
            lockedAspectRatio != null
              ? { width: true, height: true }
              : dimensionToSetForEdgePosition(edgePosition)

          let resizeCommands: Array<CanvasCommand> = []
          if (snapToParentEdge != null && snapToParentEdge.snap && elementParentBounds != null) {
            const parentGuideline = collectParentGuideline(
              edgePosition,
              snapToParentEdge.snapDirection,
              elementParentBounds,
              resizedBounds,
            )
            resizeCommands.push(setSnappingGuidelines('mid-interaction', [parentGuideline]))
          }
          if (snapToHug != null && snapToHug.snap) {
            const childGuideline = collectChildGuideline(
              edgePosition,
              snapToHug.snapDirection,
              metadata,
              interactionSession.latestMetadata,
              resizedBounds,
            )
            if (childGuideline != null) {
              resizeCommands.push(setSnappingGuidelines('mid-interaction', [childGuideline]))
            }
          }
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

  function isDraggingEdge(targetEdgePosition: EdgePosition): boolean {
    return edgePosition.x === targetEdgePosition.x && edgePosition.y === targetEdgePosition.y
  }
  if (parentFlexDirection === 'row') {
    // positive free space is calculated with sibling frames, gap and paddings
    const siblingsWidth = siblingFrames.reduce((working, frame) => {
      return frame != null && isFiniteRectangle(frame) ? frame.width + working : working
    }, 0)
    const siblingSize = siblingsWidth + siblingFrames.length * parentGap
    const shouldSnap =
      siblingSize < parentInnerBounds.width && // there is open space in the layout
      Math.abs(siblingSize + resizedBounds.width - parentInnerBounds.width) <= SnappingThreshold

    const isLastEdge =
      (isDraggingEdge(EdgePositionRight) ||
        isDraggingEdge(EdgePositionTopRight) ||
        isDraggingEdge(EdgePositionBottomRight)) &&
      isLastSibling

    const isFirstEdge =
      (isDraggingEdge(EdgePositionLeft) ||
        isDraggingEdge(EdgePositionTopLeft) ||
        isDraggingEdge(EdgePositionBottomLeft)) &&
      isFirstSibling

    const isElementDraggedOnOuterEdgeOrCorner =
      ((parentJustifyContent == null || parentJustifyContent === 'flex-start') && isLastEdge) ||
      (parentJustifyContent === 'center' && (isLastEdge || isFirstEdge)) ||
      (parentJustifyContent === 'flex-end' && isFirstEdge)

    if (isElementDraggedOnOuterEdgeOrCorner) {
      return {
        snapDirection: 'horizontal',
        snap: shouldSnap,
      }
    }
  } else if (parentFlexDirection === 'column') {
    // positive free space is calculated with sibling frames, gap and paddings
    const siblingsHeight = siblingFrames.reduce((working, frame) => {
      return frame != null && isFiniteRectangle(frame) ? frame.height + working : working
    }, 0)
    const siblingSize = siblingsHeight + siblingFrames.length * parentGap
    const shouldSnap =
      siblingSize < parentInnerBounds.height && // there is open space in the layout
      Math.abs(siblingSize + resizedBounds.height - parentInnerBounds.height) <= SnappingThreshold

    const isLastEdge =
      (isDraggingEdge(EdgePositionBottom) ||
        isDraggingEdge(EdgePositionBottomLeft) ||
        isDraggingEdge(EdgePositionBottomRight)) &&
      isLastSibling

    const isFirstEdge =
      (isDraggingEdge(EdgePositionTop) ||
        isDraggingEdge(EdgePositionTopLeft) ||
        isDraggingEdge(EdgePositionTopRight)) &&
      isFirstSibling

    const isElementDraggedOnOuterEdgeOrCorner =
      ((parentJustifyContent == null || parentJustifyContent === 'flex-start') && isLastEdge) ||
      (parentJustifyContent === 'center' && (isLastEdge || isFirstEdge)) ||
      (parentJustifyContent === 'flex-end' && isFirstEdge)

    if (isElementDraggedOnOuterEdgeOrCorner) {
      return {
        snapDirection: 'vertical',
        snap: shouldSnap,
      }
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
  element: ElementInstanceMetadata,
  startingMetadata: ElementInstanceMetadataMap,
): {
  snapDirection: 'horizontal' | 'vertical'
  snap: boolean
} | null {
  if (MetadataUtils.isFlexLayoutedContainer(element)) {
    const flexDirection = element.specialSizeMeasurements.flexDirection
    const children = MetadataUtils.getChildrenUnordered(startingMetadata, element.elementPath)
    const areAllChildrenFixed = children.some((child) => {
      const fillHugFixedState = detectFillHugFixedState(
        flexDirection === 'row' ? 'horizontal' : 'vertical',
        startingMetadata,
        child.elementPath,
      )
      return fillHugFixedState?.type === 'fixed'
    })
    if (!areAllChildrenFixed) {
      return null
    }
    const childrenFrames = children
      .filter(MetadataUtils.elementParticipatesInAutoLayout)
      .map((child) => nullIfInfinity(child.globalFrame))
    const resizeDirection = dimensionToSetForEdgePosition(edgePosition)
    const gap = element.specialSizeMeasurements.gap ?? 0
    if (flexDirection === 'row' && resizeDirection.width) {
      const childrenWidth = childrenFrames.reduce((size, child) => size + (child?.width ?? 0), 0)
      const childrenSize =
        childrenWidth +
        gap * children.length +
        (element.specialSizeMeasurements.padding.left ?? 0) +
        (element.specialSizeMeasurements.padding.right ?? 0)
      return {
        snapDirection: 'horizontal',
        snap: Math.abs(resizedBounds.width - childrenSize) <= SnappingThreshold,
      }
    } else if (flexDirection === 'column' && resizeDirection.height) {
      const childrenHeight = childrenFrames.reduce((size, child) => size + (child?.height ?? 0), 0)
      const childrenSize =
        childrenHeight +
        gap * children.length +
        (element.specialSizeMeasurements.padding.top ?? 0) +
        (element.specialSizeMeasurements.padding.bottom ?? 0)
      return {
        snapDirection: 'vertical',
        snap: Math.abs(resizedBounds.height - childrenSize) <= SnappingThreshold,
      }
    }
  }
  return null
}

function collectParentGuideline(
  edgePosition: EdgePosition,
  direction: 'horizontal' | 'vertical',
  parentBounds: CanvasRectangle,
  resizedBounds: CanvasRectangle,
): GuidelineWithSnappingVectorAndPointsOfRelevance {
  if (direction === 'horizontal') {
    const guidelinePositionX =
      edgePosition.x === 0 ? parentBounds.x : parentBounds.x + parentBounds.width
    return {
      snappingVector: zeroCanvasPoint,
      guideline: {
        type: 'XAxisGuideline',
        x: guidelinePositionX,
        yTop: Math.min(parentBounds.y, resizedBounds.y),
        yBottom: Math.max(
          parentBounds.y + parentBounds.height,
          resizedBounds.y + resizedBounds.height,
        ),
      },
      pointsOfRelevance: [
        {
          x: guidelinePositionX,
          y: parentBounds.y,
        },
        {
          x: guidelinePositionX,
          y: parentBounds.y + parentBounds.height,
        },
        {
          x: guidelinePositionX,
          y: resizedBounds.y,
        },
        {
          x: guidelinePositionX,
          y: resizedBounds.y + resizedBounds.height,
        },
      ],
    } as GuidelineWithSnappingVectorAndPointsOfRelevance
  } else {
    const guidelinePositionY =
      edgePosition.y === 0 ? parentBounds.y : parentBounds.y + parentBounds.height
    return {
      snappingVector: zeroCanvasPoint,
      guideline: {
        type: 'YAxisGuideline',
        y: guidelinePositionY,
        xLeft: Math.min(parentBounds.x, resizedBounds.x),
        xRight: Math.max(
          parentBounds.x + parentBounds.width,
          resizedBounds.x + resizedBounds.width,
        ),
      },
      pointsOfRelevance: [
        {
          x: parentBounds.x,
          y: guidelinePositionY,
        },
        {
          x: parentBounds.x + parentBounds.width,
          y: guidelinePositionY,
        },
        {
          x: resizedBounds.x,
          y: guidelinePositionY,
        },
        {
          x: resizedBounds.x + resizedBounds.width,
          y: guidelinePositionY,
        },
      ],
    } as GuidelineWithSnappingVectorAndPointsOfRelevance
  }
}

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
  if (direction === 'horizontal') {
    const childIndex = edgePosition.x === 0 ? 0 : children.length - 1
    const childFrame = children.length > 0 ? nullIfInfinity(children[childIndex].globalFrame) : null
    if (childFrame == null) {
      return null
    }
    const guidelinePositionX = edgePosition.x === 0 ? childFrame.x : childFrame.x + childFrame.width
    return {
      snappingVector: zeroCanvasPoint,
      guideline: {
        type: 'XAxisGuideline',
        x: guidelinePositionX,
        yTop: Math.min(childFrame.y, resizedBounds.y),
        yBottom: Math.max(childFrame.y + childFrame.height, resizedBounds.y + resizedBounds.height),
      },
      pointsOfRelevance: [
        {
          x: guidelinePositionX,
          y: childFrame.y,
        },
        {
          x: guidelinePositionX,
          y: childFrame.y + childFrame.height,
        },
        {
          x: guidelinePositionX,
          y: resizedBounds.y,
        },
        {
          x: guidelinePositionX,
          y: resizedBounds.y + resizedBounds.height,
        },
      ],
    } as GuidelineWithSnappingVectorAndPointsOfRelevance
  } else {
    const childIndex = edgePosition.y === 0 ? 0 : children.length - 1
    const childFrame = children.length > 0 ? nullIfInfinity(children[childIndex].globalFrame) : null
    if (childFrame == null) {
      return null
    }
    const guidelinePositionY = edgePosition.y === 0 ? childFrame.y : childFrame.y + childFrame.width
    return {
      snappingVector: zeroCanvasPoint,
      guideline: {
        type: 'YAxisGuideline',
        y: guidelinePositionY,
        xLeft: Math.min(childFrame.x, resizedBounds.x),
        xRight: Math.max(childFrame.x + childFrame.width, resizedBounds.x + resizedBounds.width),
      },
      pointsOfRelevance: [
        {
          x: childFrame.x,
          y: guidelinePositionY,
        },
        {
          x: childFrame.x + childFrame.width,
          y: guidelinePositionY,
        },
        {
          x: resizedBounds.x,
          y: guidelinePositionY,
        },
        {
          x: resizedBounds.x + resizedBounds.width,
          y: guidelinePositionY,
        },
      ],
    } as GuidelineWithSnappingVectorAndPointsOfRelevance
  }
}
