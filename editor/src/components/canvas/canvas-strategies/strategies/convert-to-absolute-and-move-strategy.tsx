import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import {
  framePointForPinnedProp,
  LayoutPinnedProp,
} from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import {
  asLocal,
  boundingRectangleArray,
  CanvasPoint,
  CanvasRectangle,
  canvasRectangleToLocalRectangle,
  CanvasVector,
  isFiniteRectangle,
  LocalPoint,
  LocalRectangle,
  nullIfInfinity,
  offsetPoint,
  offsetRect,
  rectContainsPoint,
  zeroCanvasPoint,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { fastForEach } from '../../../../core/shared/utils'
import { getElementFromProjectContents } from '../../../editor/store/editor-state'
import { FullFrame, getFullFrame } from '../../../frame'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { CanvasFrameAndTarget } from '../../canvas-types'
import { CanvasCommand } from '../../commands/commands'
import { convertToAbsolute } from '../../commands/convert-to-absolute-command'
import {
  SetCssLengthProperty,
  setCssLengthProperty,
  setValueKeepingOriginalUnit,
} from '../../commands/set-css-length-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { honoursPropsPosition } from './absolute-utils'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { getReparentOutcome, pathToReparent } from './reparent-utils'
import { applyMoveCommon, flattenSelection } from './shared-move-strategies-helpers'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { styleStringInArray } from '../../../../utils/common-constants'
import {
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  retargetStrategyToChildrenOfFragmentLikeElements,
  retargetStrategyToTopMostFragmentLikeElement,
} from './fragment-like-helpers'
import { AutoLayoutSiblingsOutline } from '../../controls/autolayout-siblings-outline'
import { memoize } from '../../../../core/shared/memoize'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import { ElementPathTrees } from '../../../../core/shared/element-path-tree'

export const ConvertToAbsoluteAndMoveStrategyID = 'CONVERT_TO_ABSOLUTE_AND_MOVE_STRATEGY'

export function convertToAbsoluteAndMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const originalTargets = retargetStrategyToTopMostFragmentLikeElement(canvasState) // this needs a better variable name
  const retargetedTargets = retargetStrategyToChildrenOfFragmentLikeElements(canvasState)

  if (
    !retargetedTargets.every((element) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        canvasState.startingMetadata,
        element,
      )
      return (
        elementMetadata?.specialSizeMeasurements.position !== 'absolute' &&
        honoursPropsPosition(canvasState, element)
      )
    })
  ) {
    return null
  }

  const autoLayoutSiblings = getAutoLayoutSiblings(
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
    originalTargets,
  )
  const hasAutoLayoutSiblings = autoLayoutSiblings.length > 1
  const autoLayoutSiblingsBounds = getAutoLayoutSiblingsBounds(
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
    originalTargets,
  )

  const autoLayoutSiblingsControl = hasAutoLayoutSiblings
    ? [
        controlWithProps({
          control: AutoLayoutSiblingsOutline,
          props: { bounds: autoLayoutSiblingsBounds },
          key: 'autolayout-siblings-outline',
          show: 'always-visible',
        }),
      ]
    : []

  return {
    id: ConvertToAbsoluteAndMoveStrategyID,
    name: 'Move (Abs)',
    controlsToRender: [
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
      ...autoLayoutSiblingsControl,
    ], // Uses existing hooks in select-mode-hooks.tsx
    fitness: getFitness(
      interactionSession,
      hasAutoLayoutSiblings,
      autoLayoutSiblingsBounds,
      originalTargets.length > 1,
    ),
    apply: () => {
      if (
        interactionSession != null &&
        interactionSession.interactionData.type === 'DRAG' &&
        interactionSession.interactionData.drag != null
      ) {
        const getConversionAndMoveCommands = (
          snappedDragVector: CanvasPoint,
        ): {
          commands: Array<CanvasCommand>
          intendedBounds: Array<CanvasFrameAndTarget>
        } => {
          return getEscapeHatchCommands(
            getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
            canvasState.startingMetadata,
            canvasState,
            snappedDragVector,
          )
        }
        const absoluteMoveApplyResult = applyMoveCommon(
          retargetedTargets,
          getTargetPathsFromInteractionTarget(canvasState.interactionTarget), // TODO eventually make this handle fragmentLike elements
          canvasState,
          interactionSession,
          getConversionAndMoveCommands,
        )

        const strategyIndicatorCommand = wildcardPatch('mid-interaction', {
          canvas: {
            controls: {
              dragToMoveIndicatorFlags: {
                $set: {
                  showIndicator: true,
                  dragType: 'absolute',
                  reparent: 'none',
                  ancestor: false,
                },
              },
            },
          },
        })

        return strategyApplicationResult([
          ...absoluteMoveApplyResult.commands,
          strategyIndicatorCommand,
        ])
      }
      // Fallback for when the checks above are not satisfied.
      return emptyStrategyApplicationResult
    },
  }
}

const BaseWeight = 0.5
const DragConversionWeight = 1.5

function getFitness(
  interactionSession: InteractionSession | null,
  hasAutoLayoutSiblings: boolean,
  autoLayoutSiblingsBounds: CanvasRectangle | null,
  multipleTargets: boolean,
): number {
  if (
    interactionSession != null &&
    interactionSession.interactionData.type === 'DRAG' &&
    interactionSession.activeControl.type === 'BOUNDING_AREA'
  ) {
    if (interactionSession.interactionData.spacePressed) {
      // If space is pressed, this should happening!
      return 100
    }

    if (interactionSession.interactionData.drag == null) {
      return BaseWeight
    }

    if (!hasAutoLayoutSiblings) {
      if (multipleTargets) {
        // multi-selection should require a spacebar press to activate
        return BaseWeight
      }
      return DragConversionWeight
    }

    const pointOnCanvas = offsetPoint(
      interactionSession.interactionData.dragStart,
      interactionSession.interactionData.drag,
    )

    const isInsideBoundingBoxOfSiblings =
      autoLayoutSiblingsBounds != null && rectContainsPoint(autoLayoutSiblingsBounds, pointOnCanvas)

    return isInsideBoundingBoxOfSiblings ? BaseWeight : DragConversionWeight
  }

  return 0
}

const getAutoLayoutSiblingsBounds = memoize(getAutoLayoutSiblingsBoundsInner, { maxSize: 1 })

function getAutoLayoutSiblingsBoundsInner(
  jsxMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  targets: Array<ElementPath>,
): CanvasRectangle | null {
  const autoLayoutSiblings = getAutoLayoutSiblings(jsxMetadata, pathTrees, targets)
  const autoLayoutSiblingsFrames = autoLayoutSiblings.map((e) => nullIfInfinity(e.globalFrame))
  return boundingRectangleArray(autoLayoutSiblingsFrames)
}

const getAutoLayoutSiblings = memoize(getAutoLayoutSiblingsInner, { maxSize: 1 })

function getAutoLayoutSiblingsInner(
  jsxMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  targets: Array<ElementPath>,
): Array<ElementInstanceMetadata> {
  if (!targets.every((t) => EP.isSiblingOf(targets[0], t))) {
    // this function only makes sense if the targets are siblings
    return []
  }
  return MetadataUtils.getSiblingsParticipatingInAutolayoutOrdered(
    jsxMetadata,
    pathTrees,
    targets[0],
  )
}

export function getEscapeHatchCommands(
  _selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  canvasState: InteractionCanvasState,
  dragDelta: CanvasVector | null,
): {
  commands: Array<CanvasCommand>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const selectedElements = flattenSelection(_selectedElements)
  if (selectedElements.length === 0) {
    return { commands: [], intendedBounds: [] }
  }
  let commands: Array<CanvasCommand> = []
  let intendedBounds: Array<CanvasFrameAndTarget> = []

  /**
   * Multiselect reparents the selection to a common ancestor
   * starting from the inner elements
   */
  const commonAncestor = EP.getCommonParent(selectedElements, false)
  const sortedElements = EP.getOrderedPathsByDepth(selectedElements)

  const elementsToConvertToAbsolute = replaceFragmentLikePathsWithTheirChildrenRecursive(
    metadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    sortedElements,
  )

  /**
   * It's possible to have descendants where the layout is defined by an ancestor
   * these are offset here as the new layout parents will be the selected elements
   */
  const descendantsInNewContainingBlock = moveDescendantsToNewContainingBlock(
    metadata,
    elementsToConvertToAbsolute,
    canvasState,
  )
  commands.push(...descendantsInNewContainingBlock)

  elementsToConvertToAbsolute.forEach((path) => {
    const elementResult = collectSetLayoutPropCommands(
      path,
      metadata,
      canvasState,
      dragDelta,
      commonAncestor,
    )

    intendedBounds.push(...elementResult.intendedBounds)
    commands.push(...elementResult.commands)
  })
  sortedElements.forEach((path) => {
    const reparentResult = collectReparentCommands(path, canvasState, commonAncestor)
    commands.push(...reparentResult)
  })
  commands.push(
    updateSelectedViews(
      'always',
      selectedElements.map((path) => {
        return commonAncestor != null ? EP.appendToPath(commonAncestor, EP.toUid(path)) : path
      }),
    ),
  )
  return { commands, intendedBounds }
}

function collectSetLayoutPropCommands(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  canvasState: InteractionCanvasState,
  dragDelta: CanvasVector | null,
  targetParent: ElementPath | null,
): {
  commands: Array<CanvasCommand>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const globalFrame = MetadataUtils.getFrameInCanvasCoords(path, metadata)
  if (globalFrame != null && isFiniteRectangle(globalFrame)) {
    const newLocalFrame = MetadataUtils.getFrameRelativeToTargetContainingBlock(
      targetParent ?? EP.parentPath(path),
      metadata,
      globalFrame,
    )

    const intendedBounds: Array<CanvasFrameAndTarget> = (() => {
      if (globalFrame == null) {
        return []
      } else {
        const updatedGlobalFrame = offsetRect(globalFrame, dragDelta ?? zeroCanvasRect)
        return [{ frame: updatedGlobalFrame, target: path }]
      }
    })()

    if (newLocalFrame != null) {
      let commands: Array<CanvasCommand> = [convertToAbsolute('always', path)]
      const updatePinsCommands = createUpdatePinsCommands(
        path,
        metadata,
        canvasState,
        dragDelta,
        newLocalFrame,
      )
      commands.push(...updatePinsCommands)

      return { commands: commands, intendedBounds: intendedBounds }
    }
  }

  return { commands: [], intendedBounds: [] }
}

function collectReparentCommands(
  path: ElementPath,
  canvasState: InteractionCanvasState,
  targetParent: ElementPath | null,
): Array<CanvasCommand> {
  const currentParentPath = EP.parentPath(path)
  const shouldReparent = targetParent != null && !EP.pathsEqual(targetParent, currentParentPath)

  if (!shouldReparent) {
    return []
  }
  const outcomeResult = getReparentOutcome(
    canvasState.builtInDependencies,
    canvasState.projectContents,
    canvasState.nodeModules,
    canvasState.openFile,
    pathToReparent(path),
    childInsertionPath(targetParent),
    'always',
    null,
  )
  if (outcomeResult == null) {
    return []
  }
  return outcomeResult.commands
}

function filterPinsToSet(
  path: ElementPath,
  canvasState: InteractionCanvasState,
): Array<LayoutPinnedProp> {
  const element = getElementFromProjectContents(
    path,
    canvasState.projectContents,
    canvasState.openFile,
  )
  if (element == null) {
    return ['top', 'left', 'width', 'height']
  } else {
    const horizontalProps = (['left', 'right', 'width'] as Array<LayoutPinnedProp>).filter((p) => {
      const prop = getLayoutProperty(p, right(element.props), styleStringInArray)
      return isRight(prop) && prop.value != null
    })
    const verticalProps = (['top', 'bottom', 'height'] as Array<LayoutPinnedProp>).filter((p) => {
      const prop = getLayoutProperty(p, right(element.props), styleStringInArray)
      return isRight(prop) && prop.value != null
    })

    let pinsToSet: Array<LayoutPinnedProp> = []
    if (horizontalProps.length === 0) {
      pinsToSet.push('left', 'width')
    } else if (horizontalProps.length === 1) {
      if (horizontalProps[0] !== 'width') {
        pinsToSet.push(...horizontalProps, 'width')
      } else {
        pinsToSet.push('left', 'width')
      }
    } else {
      pinsToSet.push(...horizontalProps)
    }
    if (verticalProps.length === 0) {
      pinsToSet.push('top', 'height')
    } else if (verticalProps.length === 1) {
      if (verticalProps[0] !== 'height') {
        pinsToSet.push(...verticalProps, 'height')
      } else {
        pinsToSet.push('top', 'height')
      }
    } else {
      pinsToSet.push(...verticalProps)
    }
    return pinsToSet
  }
}

function pinValueToSet(
  pin: LayoutPinnedProp,
  fullFrame: FullFrame,
  parentFrame: CanvasRectangle | null,
) {
  if (pin === 'right') {
    return (parentFrame?.width ?? 0) - fullFrame[pin]
  } else if (pin === 'bottom') {
    return (parentFrame?.height ?? 0) - fullFrame[pin]
  } else {
    return fullFrame[pin]
  }
}

function findAbsoluteDescendantsToMove(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): Array<ElementPath> {
  /**
   * Collecting all absolute descendants that have their containing block outside of the converted element.
   * The containing block element is the layout parent that provides the bounds for the top/left/bottom/right of the position absolute element.
   * The absolute conversion creates a new containing block, these child elements are moved to keep their position in place.
   * Not all absolute elements will change their containing block here: for example relative positioned descendant with absolute children, the relative positioned element defines the containing block.
   * And component children are changed only when the selected element is inside a focused component.
   */
  return mapDropNulls((element) => {
    const path = element.elementPath
    const nearestSelectedAncestor = findNearestSelectedAncestor(path, selectedElements)
    if (
      MetadataUtils.isPositionAbsolute(element) &&
      nearestSelectedAncestor != null &&
      EP.isFromSameInstanceAs(path, nearestSelectedAncestor) &&
      !EP.isRootElementOfInstance(path)
    ) {
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
      const containingBlockPath = elementMetadata?.specialSizeMeasurements.closestOffsetParentPath
      /**
       * With the conversion the nearest selected ancestor will receive absolute position,
       * checking if the containing block element is somewhere outside of the selection.
       */
      if (
        containingBlockPath != null &&
        EP.isDescendantOf(nearestSelectedAncestor, containingBlockPath)
      ) {
        return path
      } else {
        return null
      }
    } else {
      return null
    }
  }, Object.values(metadata))
}

function moveDescendantsToNewContainingBlock(
  metadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
  canvasState: InteractionCanvasState,
): Array<CanvasCommand> {
  const absoluteDescendants = findAbsoluteDescendantsToMove(selectedElements, metadata)
  return absoluteDescendants.flatMap((path) => {
    const canvasFrame = MetadataUtils.getFrameInCanvasCoords(path, metadata)

    const nearestSelectedAncestor = findNearestSelectedAncestor(path, selectedElements)
    if (nearestSelectedAncestor != null) {
      const nearestSelectedAncestorFrame = MetadataUtils.getFrameInCanvasCoords(
        nearestSelectedAncestor,
        metadata,
      )
      if (
        canvasFrame != null &&
        nearestSelectedAncestorFrame != null &&
        isFiniteRectangle(canvasFrame) &&
        isFiniteRectangle(nearestSelectedAncestorFrame)
      ) {
        /**
         * after conversion selected elements define the containing block,
         * descendants are offset to the new layout ancestor
         */
        const newLocalFrame = canvasRectangleToLocalRectangle(
          canvasFrame,
          nearestSelectedAncestorFrame,
        )
        return createUpdatePinsCommands(path, metadata, canvasState, zeroCanvasPoint, newLocalFrame)
      }
    }
    return []
  })
}

function findNearestSelectedAncestor(
  target: ElementPath,
  selectedElements: Array<ElementPath>,
): ElementPath | null {
  return (
    EP.getOrderedPathsByDepth(selectedElements).find((selection) =>
      EP.isDescendantOf(target, selection),
    ) ?? null
  )
}

function createUpdatePinsCommands(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  canvasState: InteractionCanvasState,
  dragDelta: CanvasVector | null,
  frame: LocalRectangle,
) {
  const specialSizeMeasurements = MetadataUtils.findElementByElementPath(
    metadata,
    path,
  )?.specialSizeMeasurements
  const parentFrame = specialSizeMeasurements?.immediateParentBounds ?? null
  const parentFlexDirection = specialSizeMeasurements?.parentFlexDirection ?? null
  const frameWithoutMargin = getFrameWithoutMargin(frame, specialSizeMeasurements)
  const updatedFrame = offsetRect(frameWithoutMargin, asLocal(dragDelta ?? zeroCanvasRect))
  const fullFrame = getFullFrame(updatedFrame)
  const pinsToSet = filterPinsToSet(path, canvasState)

  let commands: Array<SetCssLengthProperty> = []
  fastForEach(pinsToSet, (framePin) => {
    const pinValue = pinValueToSet(framePin, fullFrame, parentFrame)
    commands.push(
      setCssLengthProperty(
        'always',
        path,
        stylePropPathMappingFn(framePin, styleStringInArray),
        setValueKeepingOriginalUnit(
          pinValue,
          isHorizontalPoint(framePointForPinnedProp(framePin))
            ? parentFrame?.width
            : parentFrame?.height,
        ),
        parentFlexDirection,
      ),
    )
  })
  return commands
}

function getFrameWithoutMargin(
  frame: LocalRectangle,
  specialSizeMeasurements: SpecialSizeMeasurements | undefined,
) {
  // TODO fix bottom and right margins
  const margin = specialSizeMeasurements?.margin
  const marginPoint: LocalPoint = {
    x: -(margin?.left ?? 0),
    y: -(margin?.top ?? 0),
  } as LocalPoint
  return offsetRect(frame, marginPoint)
}
