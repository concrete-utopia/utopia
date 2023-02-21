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
import { FlexDirection } from '../../../inspector/common/css-utils'
import { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import { detectFillHugFixedState } from '../../../inspector/inspector-common'
import * as EP from '../../../../core/shared/element-path'
import { deleteProperties } from '../../commands/delete-properties-command'
import { getElementDimensions } from './flex-resize-helpers'

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

  const widthPropToUse =
    (elementParentFlexDirection === 'row' || elementParentFlexDirection === 'row-reverse') &&
    elementDimensionsProps?.flexBasis != null
      ? 'flexBasis'
      : 'width'
  const heightPropToUse =
    (elementParentFlexDirection === 'column' || elementParentFlexDirection === 'column-reverse') &&
    elementDimensionsProps?.flexBasis != null
      ? 'flexBasis'
      : 'height'

  const elementDimensions =
    elementDimensionsProps == null
      ? null
      : {
          width: elementDimensionsProps[widthPropToUse],
          height: elementDimensionsProps[heightPropToUse],
        }

  const hasDimensions =
    elementDimensions != null &&
    (elementDimensions.width != null || elementDimensions.height != null)
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
      (hasDimensions || !hasSizedParent)
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

          const resizedBounds = resizeBoundingBox(
            originalBounds,
            drag,
            edgePosition,
            lockedAspectRatio,
            'non-center-based',
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

  if (anySiblingFillSized) {
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
    if (
      // in a row layout with left aligned elements dragging on the right edge/corner snaps to parent edge
      (parentJustifyContent == null || parentJustifyContent === 'flex-start') &&
      (isDraggingEdge(EdgePositionRight) ||
        isDraggingEdge(EdgePositionTopRight) ||
        isDraggingEdge(EdgePositionBottomRight))
    ) {
      return {
        snapDirection: 'horizontal',
        snap:
          resizedBounds.x + resizedBounds.width + SnappingThreshold >
          parentInnerBounds.x + parentInnerBounds.width,
      }
    } else if (parentJustifyContent === 'center' && edgePosition.x !== 0.5) {
      // in a row layout with center aligned elements dragging on the left or right edge/corner snaps to parent edge
      // positive free space is calculated with sibling frames, gap and paddings
      const siblingsWidth = siblingFrames.reduce((working, frame) => {
        return frame != null && isFiniteRectangle(frame) ? frame.width + working : working
      }, 0)
      const siblingsAndDraggedFrame =
        siblingsWidth + resizedBounds.width + siblingFrames.length * parentGap
      return {
        snapDirection: 'horizontal',
        snap: siblingsAndDraggedFrame + SnappingThreshold > parentInnerBounds.width,
      }
    } else if (
      // in a row layout with right aligned elements dragging on the left edge/corner snaps to parent edge
      parentJustifyContent === 'flex-end' &&
      (isDraggingEdge(EdgePositionLeft) ||
        isDraggingEdge(EdgePositionTopLeft) ||
        isDraggingEdge(EdgePositionBottomLeft))
    ) {
      return {
        snapDirection: 'horizontal',
        snap: parentInnerBounds.x > resizedBounds.x + SnappingThreshold,
      }
    }
  } else if (parentFlexDirection === 'column') {
    // in a column layout with top aligned elements dragging on the bottom edge/corner snaps to parent edge
    if (
      (parentJustifyContent == null || parentJustifyContent === 'flex-start') &&
      (isDraggingEdge(EdgePositionBottom) ||
        isDraggingEdge(EdgePositionBottomLeft) ||
        isDraggingEdge(EdgePositionBottomRight))
    ) {
      const parentBottomEdge = parentInnerBounds.y + parentInnerBounds.height
      const elementBottomEdge = resizedBounds.y + resizedBounds.height
      return {
        snapDirection: 'vertical',
        snap: elementBottomEdge + SnappingThreshold > parentBottomEdge,
      }
    } else if (parentJustifyContent === 'center' && edgePosition.y !== 0.5) {
      // in a column layout with center aligned elements dragging on the top or bottom edge/corner snaps to parent edge
      // positive free space is calculated with sibling frames, gap and paddings
      const siblingsHeight = siblingFrames.reduce((working, frame) => {
        return frame != null && isFiniteRectangle(frame) ? frame.height + working : working
      }, 0)
      const siblingsAndDraggedFrame =
        siblingsHeight + resizedBounds.height + siblingFrames.length * parentGap
      return {
        snapDirection: 'vertical',
        snap: siblingsAndDraggedFrame + SnappingThreshold > parentInnerBounds.height,
      }
    } else if (
      // in a column layout with bottom aligned elements dragging on the bottom edge/corner snaps to parent edge
      parentJustifyContent === 'flex-end' &&
      (isDraggingEdge(EdgePositionTop) ||
        isDraggingEdge(EdgePositionTopLeft) ||
        isDraggingEdge(EdgePositionTopRight))
    ) {
      return {
        snapDirection: 'vertical',
        snap: parentBounds.y > resizedBounds.y + SnappingThreshold,
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
