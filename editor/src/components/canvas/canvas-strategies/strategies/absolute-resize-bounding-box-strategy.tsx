import { styleStringInArray } from '../../../../utils/common-constants'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isRight, right } from '../../../../core/shared/either'
import { ElementInstanceMetadataMap, JSXElement } from '../../../../core/shared/element-template'
import {
  CanvasRectangle,
  canvasRectangleToLocalRectangle,
  isInfinityRectangle,
  rectangleDifference,
  roundRectangleToNearestWhole,
  roundTo,
  SimpleRectangle,
  transformFrameUsingBoundingBox,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { AllElementProps, getElementFromProjectContents } from '../../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { EdgePosition } from '../../canvas-types'
import {
  AdjustCssLengthProperties,
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
  LengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import { pushIntendedBoundsAndUpdateGroups } from '../../commands/push-intended-bounds-and-update-groups-command'
import {
  SetCssLengthProperty,
  setCssLengthProperty,
  setValueKeepingOriginalUnit,
} from '../../commands/set-css-length-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../../commands/update-highlighted-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { AbsoluteResizeControl } from '../../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../../controls/zero-sized-element-controls'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import {
  AbsolutePin,
  isAnySelectedElementAspectRatioLocked,
  ensureAtLeastTwoPinsForEdgePosition,
  getLockedAspectRatio,
  pickCursorFromEdgePosition,
  resizeBoundingBox,
  supportsAbsoluteResize,
  onlyEnsureOffsetPinsExist,
} from './resize-helpers'
import { runLegacyAbsoluteResizeSnapping } from './shared-absolute-resize-strategy-helpers'
import { flattenSelection, getMultiselectBounds } from './shared-move-strategies-helpers'
import { FlexDirection } from '../../../inspector/common/css-utils'
import { retargetStrategyToChildrenOfFragmentLikeElements } from './fragment-like-helpers'
import { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { treatElementAsGroupLike } from './group-helpers'

export function absoluteResizeBoundingBoxStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const originalTargets = flattenSelection(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )
  const retargetedTargets = flattenSelection(
    retargetStrategyToChildrenOfFragmentLikeElements(canvasState),
  )

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
    controlsToRender: [
      controlWithProps({
        control: AbsoluteResizeControl,
        props: { targets: originalTargets },
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
            const { snappedBoundingBox, guidelinesWithSnappingVector } = snapBoundingBox(
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
              const element = getElementFromProjectContents(
                selectedElement,
                canvasState.projectContents,
                canvasState.openFile,
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
                canvasState.startingElementPathTree,
                selectedElement,
              )

              const newFrame = roundRectangleToNearestWhole(
                transformFrameUsingBoundingBox(
                  snappedBoundingBox,
                  originalBoundingBox,
                  originalFrame,
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

              return [
                ...createResizeCommandsFromFrame(
                  element,
                  selectedElement,
                  newFrame,
                  originalFrame,
                  elementParentBounds,
                  elementParentFlexDirection,
                  edgePosition,
                  elementIsGroup
                    ? 'only-offset-pins-are-needed'
                    : 'ensure-two-pins-per-dimension-exists',
                ),
                pushIntendedBoundsAndUpdateGroups([{ target: selectedElement, frame: newFrame }]),
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

function createResizeCommandsFromFrame(
  element: JSXElement,
  selectedElement: ElementPath,
  newFrame: CanvasRectangle,
  originalFrame: CanvasRectangle,
  elementParentBounds: CanvasRectangle | null,
  elementParentFlexDirection: FlexDirection | null,
  edgePosition: EdgePosition,
  ensurePinsExist: 'ensure-two-pins-per-dimension-exists' | 'only-offset-pins-are-needed',
): (AdjustCssLengthProperties | SetCssLengthProperty)[] {
  const pins: Array<AbsolutePin> =
    ensurePinsExist === 'ensure-two-pins-per-dimension-exists'
      ? ensureAtLeastTwoPinsForEdgePosition(right(element.props), edgePosition)
      : onlyEnsureOffsetPinsExist(right(element.props), edgePosition)

  let propertiesToAdjust: Array<LengthPropertyToAdjust> = []
  let commands: Array<AdjustCssLengthProperties | SetCssLengthProperty> = []

  for (const pin of pins) {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const value = getLayoutProperty(pin, right(element.props), styleStringInArray)
    const rectangleDiff = rectangleDifference(originalFrame, newFrame)
    const delta = allPinsFromFrame(rectangleDiff)[pin]
    const roundedDelta = roundTo(delta, 0)
    const pinDirection = pin === 'right' || pin === 'bottom' ? -1 : 1
    if (roundedDelta !== 0) {
      if (isRight(value) && value.value != null) {
        propertiesToAdjust.push(
          lengthPropertyToAdjust(
            stylePropPathMappingFn(pin, styleStringInArray),
            roundedDelta * pinDirection,
            horizontal ? elementParentBounds?.width : elementParentBounds?.height,
            'create-if-not-existing',
          ),
        )
      } else {
        // If this element has a parent, we need to take that parent's bounds into account
        const frameToUse =
          elementParentBounds == null
            ? newFrame
            : canvasRectangleToLocalRectangle(newFrame, elementParentBounds)
        const valueToSet = allPinsFromFrame(frameToUse)[pin]
        commands.push(
          setCssLengthProperty(
            'always',
            selectedElement,
            stylePropPathMappingFn(pin, styleStringInArray),
            setValueKeepingOriginalUnit(
              roundTo(valueToSet, 0),
              horizontal ? elementParentBounds?.width : elementParentBounds?.height,
            ),
            elementParentFlexDirection,
          ),
        )
      }
    }
  }

  commands.push(
    adjustCssLengthProperties(
      'always',
      selectedElement,
      elementParentFlexDirection,
      propertiesToAdjust,
    ),
  )
  return commands
}

function allPinsFromFrame(frame: SimpleRectangle): { [key: string]: number } {
  return {
    left: frame.x,
    top: frame.y,
    width: frame.width,
    height: frame.height,
    right: frame.x + frame.width,
    bottom: frame.y + frame.height,
  }
}

function snapBoundingBox(
  selectedElements: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  edgePosition: EdgePosition,
  resizedBounds: CanvasRectangle,
  canvasScale: number,
  lockedAspectRatio: number | null,
  centerBased: 'center-based' | 'non-center-based',
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
) {
  const { snappedBoundingBox, guidelinesWithSnappingVector } = runLegacyAbsoluteResizeSnapping(
    selectedElements,
    jsxMetadata,
    edgePosition,
    resizedBounds,
    canvasScale,
    lockedAspectRatio,
    centerBased,
    allElementProps,
    pathTrees,
  )

  return {
    snappedBoundingBox,
    guidelinesWithSnappingVector,
  }
}
