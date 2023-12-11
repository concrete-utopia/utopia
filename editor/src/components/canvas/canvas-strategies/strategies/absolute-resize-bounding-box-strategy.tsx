import type { LayoutPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  canvasRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  roundRectangleToNearestWhole,
  transformFrameUsingBoundingBox,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../editor/store/editor-state'
import {
  getJSXElementFromProjectContents,
  trueUpGroupElementChanged,
} from '../../../editor/store/editor-state'
import {
  detectFillHugFixedState,
  getConstraintsIncludingImplicitForElement,
  isHuggingFixedHugFill,
} from '../../../inspector/inspector-common'
import type { EdgePosition } from '../../canvas-types'
import { EdgePositionLeft, EdgePositionTop, EdgePositionTopLeft } from '../../canvas-types'
import { isEdgePositionEqualTo } from '../../canvas-utils'
import { pushIntendedBoundsAndUpdateGroups } from '../../commands/push-intended-bounds-and-update-groups-command'
import { queueTrueUpElement } from '../../commands/queue-true-up-command'
import { activeFrameTargetRect, setActiveFrames } from '../../commands/set-active-frames-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { gatherParentAndSiblingTargets } from '../../controls/guideline-helpers'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import {
  getDescriptiveStrategyLabelWithRetargetedPaths,
  onlyFitWhenDraggingThisControl,
} from '../canvas-strategies'
import type { CanvasStrategy, InteractionCanvasState } from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import {
  getChildGroupsForNonGroupParents,
  retargetStrategyToChildrenOfFragmentLikeElements,
  treatElementAsFragmentLike,
} from './fragment-like-helpers'
import { treatElementAsGroupLike } from './group-helpers'
import {
  getLockedAspectRatio,
  isAnySelectedElementAspectRatioLocked,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
  supportsAbsoluteResize,
} from './resize-helpers'
import type { EnsureFramePointsExist } from './resize-strategy-helpers'
import { createResizeCommandsFromFrame } from './resize-strategy-helpers'
import { childrenBoundsToSnapTo, snapBoundingBox } from './shared-absolute-resize-strategy-helpers'
import { flattenSelection, getMultiselectBounds } from './shared-move-strategies-helpers'

export function absoluteResizeBoundingBoxStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const originalTargets = flattenSelection(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )

  const { pathsWereReplaced, paths } = retargetStrategyToChildrenOfFragmentLikeElements(canvasState)

  const retargetedTargets = flattenSelection(paths)

  if (
    retargetedTargets.length === 0 ||
    !retargetedTargets.every((element) => {
      return supportsAbsoluteResize(canvasState.startingMetadata, element, canvasState)
    })
  ) {
    return null
  }

  return {
    id: 'ABSOLUTE_RESIZE_BOUNDING_BOX',
    name: 'Resize',
    descriptiveLabel: getDescriptiveStrategyLabelWithRetargetedPaths(
      'Resizing Elements',
      pathsWereReplaced,
    ),
    icon: {
      category: 'modalities',
      type: 'resize',
    },
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: originalTargets, pathsWereReplaced: pathsWereReplaced },
        key: 'absolute-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
      controlWithProps({
        control: ZeroSizeResizeControlWrapper,
        props: { targets: originalTargets },
        key: 'zero-size-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: originalTargets },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: originalTargets },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'RESIZE_HANDLE', 1),
    apply: () => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.activeControl.type === 'RESIZE_HANDLE'
      ) {
        const childGroups = getChildGroupsForNonGroupParents(
          canvasState.startingMetadata,
          retargetedTargets,
        )

        const edgePosition = interactionSession.activeControl.edgePosition
        if (interactionSession.interactionData.drag != null) {
          const drag = interactionSession.interactionData.drag
          const originalBoundingBox = getMultiselectBounds(
            canvasState.startingMetadata,
            retargetedTargets,
          )
          const anySelectedElementAspectRatioLocked = isAnySelectedElementAspectRatioLocked(
            canvasState.startingMetadata,
            retargetedTargets,
          )
          if (originalBoundingBox != null) {
            const lockedAspectRatio = getLockedAspectRatio(
              interactionSession,
              interactionSession.interactionData.modifiers,
              originalBoundingBox,
              anySelectedElementAspectRatioLocked,
            )
            const centerBased = interactionSession.interactionData.modifiers.alt
              ? 'center-based'
              : 'non-center-based'
            const newBoundingBox = resizeBoundingBox(
              originalBoundingBox,
              drag,
              edgePosition,
              lockedAspectRatio,
              centerBased,
            )
            const parentAndSiblings: ElementPath[] = gatherParentAndSiblingTargets(
              canvasState.startingMetadata,
              canvasState.startingAllElementProps,
              canvasState.startingElementPathTree,
              originalTargets,
            )
            const childrenToSnapTo = childrenBoundsToSnapTo(
              edgePosition,
              originalTargets,
              canvasState.startingMetadata,
              canvasState.startingAllElementProps,
              canvasState.startingElementPathTree,
            )
            const snapTargets = [...childrenToSnapTo, ...parentAndSiblings]
            const { snappedBoundingBox, guidelinesWithSnappingVector } = snapBoundingBox(
              snapTargets,
              originalTargets,
              canvasState.startingMetadata,
              edgePosition,
              newBoundingBox,
              canvasState.scale,
              lockedAspectRatio,
              centerBased,
              canvasState.startingAllElementProps,
              canvasState.startingElementPathTree,
            )

            const commandsForSelectedElements = retargetedTargets.flatMap((selectedElement) => {
              const element = getJSXElementFromProjectContents(
                selectedElement,
                canvasState.projectContents,
              )
              const originalFrame = MetadataUtils.getFrameInCanvasCoords(
                selectedElement,
                canvasState.startingMetadata,
              )

              if (element == null || originalFrame == null || isInfinityRectangle(originalFrame)) {
                return []
              }

              const elementIsGroup = treatElementAsGroupLike(
                canvasState.startingMetadata,
                selectedElement,
              )

              // If there are constrained descendants of the selected elements, adjust the
              // resized frame to respect the min/max dimensions that come from them.
              const newFrame = applyConstraintsAdjustmentsToFrame(
                canvasState.startingMetadata,
                canvasState.startingAllElementProps,
                canvasState.startingElementPathTree,
                selectedElement,
                originalFrame,
                edgePosition,
                roundRectangleToNearestWhole(
                  transformFrameUsingBoundingBox(
                    snappedBoundingBox,
                    originalBoundingBox,
                    originalFrame,
                  ),
                ),
              )

              const metadata = MetadataUtils.findElementByElementPath(
                canvasState.startingMetadata,
                selectedElement,
              )
              const elementParentBounds =
                metadata?.specialSizeMeasurements.immediateParentBounds ?? null

              const elementParentFlexDirection =
                metadata?.specialSizeMeasurements.parentFlexDirection ?? null

              const ensureFramePointsExist: EnsureFramePointsExist =
                !elementIsGroup ||
                (isEdgePositionEqualTo(edgePosition, EdgePositionLeft) && originalFrame.x === 0) ||
                (isEdgePositionEqualTo(edgePosition, EdgePositionTop) && originalFrame.y === 0) ||
                (isEdgePositionEqualTo(edgePosition, EdgePositionTopLeft) &&
                  (originalFrame.x === 0 || originalFrame.y === 0))
                  ? 'ensure-two-frame-points-per-dimension-exists'
                  : 'only-offset-frame-points-are-needed'

              return [
                ...createResizeCommandsFromFrame(
                  element,
                  selectedElement,
                  newFrame,
                  originalFrame,
                  elementParentBounds,
                  elementParentFlexDirection,
                  edgePosition,
                  ensureFramePointsExist,
                ),
                pushIntendedBoundsAndUpdateGroups(
                  [{ target: selectedElement, frame: newFrame }],
                  'starting-metadata',
                ),
                queueTrueUpElement(childGroups.map(trueUpGroupElementChanged)),
                setActiveFrames([
                  {
                    action: 'resize',
                    target: activeFrameTargetRect(newFrame),
                    source: originalBoundingBox,
                  },
                ]),
              ]
            })

            return strategyApplicationResult([
              ...commandsForSelectedElements,
              setSnappingGuidelines('mid-interaction', guidelinesWithSnappingVector),
              updateHighlightedViews('mid-interaction', []),
              setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
              setElementsToRerenderCommand(retargetedTargets),
            ])
          }
        } else {
          return strategyApplicationResult([
            setCursorCommand(pickCursorFromEdgePosition(edgePosition)),
            updateHighlightedViews('mid-interaction', []),
          ])
        }
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

/**
 * Returns adjusted version of the newFrame that respects any constraints that
 * may be set on its descendants.
 */
function applyConstraintsAdjustmentsToFrame(
  jsxMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  target: ElementPath,
  originalFrame: CanvasRectangle,
  edgePosition: EdgePosition,
  newFrame: CanvasRectangle,
): CanvasRectangle {
  const resizeAffectsChildren =
    treatElementAsGroupLike(jsxMetadata, target) ||
    treatElementAsFragmentLike(jsxMetadata, allElementProps, pathTrees, target)
  if (!resizeAffectsChildren) {
    // this can become anything that can affect the layout of its children
    return newFrame
  }

  const { constrainedSizes, lockedWidth, lockedHeight } = getConstrainedSizes(
    jsxMetadata,
    allElementProps,
    target,
    originalFrame,
  )
  if (constrainedSizes.length <= 0) {
    return newFrame
  }

  const { offset: x, size: width } = getConstrainedFramesAdjustments(
    lockedWidth,
    'width',
    constrainedSizes,
    edgePosition,
    originalFrame,
    newFrame,
  )
  const { offset: y, size: height } = getConstrainedFramesAdjustments(
    lockedHeight,
    'height',
    constrainedSizes,
    edgePosition,
    originalFrame,
    newFrame,
  )

  return canvasRectangle({ x, y, width, height })
}

/**
 * Returns a pair of offset and size for the given dimension of the newFrame rectangle,
 * which respect the given constraints.
 */
function getConstrainedFramesAdjustments(
  locked: boolean,
  dimension: 'width' | 'height',
  constrainedSizes: Array<NullableSize>,
  edgePosition: EdgePosition,
  originalFrame: CanvasRectangle,
  newFrame: CanvasRectangle,
): {
  offset: number
  size: number
} {
  const axis = dimension === 'width' ? 'x' : 'y'
  const currentOffset = newFrame[axis]
  const originalOffset = originalFrame[axis]
  const originalDimension = originalFrame[dimension]
  const currentDimension = newFrame[dimension]
  const isLeftEdge = edgePosition[axis] === 0
  const isTopEdge = edgePosition[axis] === 1

  if (locked) {
    // If it's locked, return the original bounds.
    return {
      offset: originalOffset,
      size: originalDimension,
    }
  }

  // The maximum value of the given constraints, used as the upper bound for the adjustment.
  const maxDimension = constrainedSizes.reduce((size, frame) => {
    const frameDimension = frame[dimension]
    return frameDimension != null ? Math.max(frameDimension, size) : size
  }, -Infinity)

  function getOffset() {
    if (currentDimension <= maxDimension) {
      if (isLeftEdge) {
        return Math.max(originalOffset, originalOffset + originalDimension - maxDimension)
      }
      if (isTopEdge) {
        return originalOffset
      }
    }
    return currentOffset
  }

  return {
    offset: getOffset(),
    size: Math.max(maxDimension, currentDimension),
  }
}

function isDimensionConstrained(
  jsxMetadata: ElementInstanceMetadataMap,
  path: ElementPath,
  constraintsArray: LayoutPinnedProp[],
  dimension: 'width' | 'height',
): boolean {
  return (
    constraintsArray.includes(dimension) ||
    isHuggingFixedHugFill(
      detectFillHugFixedState(dimension === 'width' ? 'horizontal' : 'vertical', jsxMetadata, path)
        .fixedHugFill?.type,
    ) // hug is treated as a constrained dimension
  )
}

type NullableSize = {
  width: number | null
  height: number | null
}

function getConstrainedSizes(
  jsxMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  path: ElementPath,
  originalFrame: CanvasRectangle,
): { constrainedSizes: Array<NullableSize>; lockedWidth: boolean; lockedHeight: boolean } {
  let lockedWidth = false
  let lockedHeight = false
  let constrainedSizes: Array<NullableSize> = []

  const descendants = Object.values(jsxMetadata).filter((element) =>
    EP.isDescendantOf(element.elementPath, path),
  )
  for (const element of descendants) {
    const constraintsArray = getConstraintsIncludingImplicitForElement(
      jsxMetadata,
      allElementProps,
      element.elementPath,
      'only-explicit-constraints', // if we set this to include-implicit-constraints, we can probably delete isDimensionConstrained
    )
    const constraints = {
      width: isDimensionConstrained(jsxMetadata, element.elementPath, constraintsArray, 'width'),
      height: isDimensionConstrained(jsxMetadata, element.elementPath, constraintsArray, 'height'),
      top: constraintsArray.includes('top'),
      bottom: constraintsArray.includes('bottom'),
      left: constraintsArray.includes('left'),
      right: constraintsArray.includes('right'),
    }

    lockedWidth ||= constraints.width && constraints.left && constraints.right
    lockedHeight ||= constraints.height && constraints.top && constraints.bottom

    const isConstrained =
      constraints.top ||
      constraints.bottom ||
      constraints.height ||
      constraints.left ||
      constraints.right ||
      constraints.width

    if (
      isConstrained &&
      element.localFrame != null &&
      isFiniteRectangle(element.localFrame) &&
      element.globalFrame != null &&
      isFiniteRectangle(element.globalFrame)
    ) {
      const constrainedParentFrame = MetadataUtils.getFrameInCanvasCoords(
        EP.parentPath(element.elementPath),
        jsxMetadata,
      )
      const offsetDiffX =
        constrainedParentFrame != null && isFiniteRectangle(constrainedParentFrame)
          ? constrainedParentFrame.x - originalFrame.x
          : 0
      const offsetDiffY =
        constrainedParentFrame != null && isFiniteRectangle(constrainedParentFrame)
          ? constrainedParentFrame.y - originalFrame.y
          : 0

      constrainedSizes.push({
        width: getBoundDimension(
          constraints.left,
          constraints.right,
          constraints.width,
          originalFrame.width,
          element.localFrame.x,
          element.localFrame.width + offsetDiffX,
        ),
        height: getBoundDimension(
          constraints.top,
          constraints.bottom,
          constraints.height,
          originalFrame.height,
          element.localFrame.y,
          element.localFrame.height + offsetDiffY,
        ),
      })
    }
  }

  return {
    constrainedSizes,
    lockedWidth,
    lockedHeight,
  }
}

function getBoundDimension(
  minBound: boolean,
  maxBound: boolean,
  dimensionBound: boolean,
  originalDimension: number,
  frameOffset: number,
  frameDimension: number,
): number | null {
  if (minBound && maxBound) {
    return originalDimension
  } else if (minBound && dimensionBound) {
    return frameOffset + frameDimension
  } else if (minBound) {
    return frameOffset
  } else if (maxBound && dimensionBound) {
    return originalDimension - frameOffset
  } else if (maxBound) {
    return originalDimension - frameDimension
  } else if (dimensionBound) {
    return frameDimension
  } else {
    return null
  }
}
