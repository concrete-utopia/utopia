import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp, LayoutPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { flatMapArray, mapDropNulls, stripNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  asLocal,
  CanvasPoint,
  CanvasRectangle,
  canvasRectangleToLocalRectangle,
  CanvasVector,
  LocalPoint,
  LocalRectangle,
  offsetPoint,
  offsetRect,
  rectContainsPoint,
  zeroCanvasPoint,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import { FullFrame, getFullFrame } from '../../frame'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { CanvasFrameAndTarget, CSSCursor } from '../canvas-types'
import { CanvasCommand } from '../commands/commands'
import { ConvertToAbsolute, convertToAbsolute } from '../commands/convert-to-absolute-command'
import { ReparentElement, reparentElement } from '../commands/reparent-element-command'
import { SetCssLengthProperty, setCssLengthProperty } from '../commands/set-css-length-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { showOutlineHighlight } from '../commands/show-outline-highlight-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { AnimationTimer, PieTimerControl } from '../controls/select-mode/pie-timer'
import { ZeroSizeResizeControlWrapper } from '../controls/zero-sized-element-controls'
import { applyAbsoluteMoveCommon } from './absolute-move-strategy'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
} from './canvas-strategy-types'
import { DragInteractionData, InteractionSession, StrategyState } from './interaction-state'

export const escapeHatchStrategy: CanvasStrategy = {
  id: 'ESCAPE_HATCH_STRATEGY',
  name: 'Absolute Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length > 0) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
        return (
          elementMetadata?.specialSizeMeasurements.position === 'static' ||
          MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
            element,
            metadata,
          )
        )
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    {
      control: ZeroSizeResizeControlWrapper,
      key: 'zero-size-resize-control',
      show: 'always-visible',
    },
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
    {
      control: PieTimerControl,
      key: 'pie-timer-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
  ],
  fitness: (canvasState, interactionState, strategyState) => {
    if (
      escapeHatchStrategy.isApplicable(
        canvasState,
        interactionState,
        strategyState.startingMetadata,
        strategyState.startingAllElementProps,
      ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
    ) {
      if (escapeHatchAllowed(canvasState, interactionState.interactionData, strategyState)) {
        return 2
      } else {
        return 0.5
      }
    } else {
      return 0
    }
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (interactionState.interactionData.type === 'DRAG') {
      let shouldEscapeHatch = false
      let escapeHatchActivated = strategyState.customStrategyState.escapeHatchActivated ?? false
      let dragThresholdPassed = interactionState.interactionData.drag != null
      if (dragThresholdPassed) {
        if (interactionState.interactionData.modifiers.cmd) {
          shouldEscapeHatch = true
        } else if (
          escapeHatchActivated ||
          interactionState.interactionData.globalTime - interactionState.lastInteractionTime >
            AnimationTimer
        ) {
          shouldEscapeHatch = true
          escapeHatchActivated = true
        }
      }

      if (shouldEscapeHatch) {
        const getConversionAndMoveCommands = (
          snappedDragVector: CanvasPoint,
        ): {
          commands: Array<CanvasCommand>
          intendedBounds: Array<CanvasFrameAndTarget>
        } => {
          return getEscapeHatchCommands(
            canvasState.selectedElements,
            strategyState.startingMetadata,
            canvasState,
            snappedDragVector,
          )
        }
        const absoluteMoveApplyResult = applyAbsoluteMoveCommon(
          canvasState,
          interactionState,
          strategyState,
          getConversionAndMoveCommands,
        )

        return {
          commands: absoluteMoveApplyResult.commands,
          customState: {
            ...strategyState.customStrategyState,
            escapeHatchActivated,
          },
        }
      } else {
        return {
          commands: [setCursorCommand('transient', CSSCursor.Move)],
          customState: null,
        }
      }
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

export function getEscapeHatchCommands(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  canvasState: InteractionCanvasState,
  dragDelta: CanvasVector | null,
): {
  commands: Array<CanvasCommand>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  const moveAndPositionCommands = collectMoveCommandsForSelectedElements(
    selectedElements,
    metadata,
    canvasState,
    dragDelta,
  )
  return moveAndPositionCommands
}

function collectMoveCommandsForSelectedElements(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  canvasState: InteractionCanvasState,
  dragDelta: CanvasVector | null,
): {
  commands: Array<CanvasCommand>
  intendedBounds: Array<CanvasFrameAndTarget>
} {
  let commands: Array<CanvasCommand> = []
  let intendedBounds: Array<CanvasFrameAndTarget> = []

  const commonAncestor = EP.getCommonParent(selectedElements, false)
  const sortedElements = EP.getOrderedPathsByDepth(selectedElements) // inner elements should be reparented first

  const descendantsInNewContainingBlock = moveDescendantsToNewContainingBlock(
    metadata,
    selectedElements,
    canvasState,
  )
  commands.push(...descendantsInNewContainingBlock)

  sortedElements.forEach((path) => {
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
  commands.push(
    updateSelectedViews(
      'permanent',
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
  const currentParentPath = EP.parentPath(path)
  const shouldReparent = targetParent != null && !EP.pathsEqual(targetParent, currentParentPath)
  const globalFrame = MetadataUtils.getFrameInCanvasCoords(path, metadata)
  if (globalFrame != null) {
    const newLocalFrame = MetadataUtils.getFrameRelativeToTargetContainingBlock(
      shouldReparent ? targetParent : currentParentPath,
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

    let commands: Array<CanvasCommand> = [convertToAbsolute('permanent', path)]
    const updatePinsCommands = createUpdatePinsCommands(
      path,
      metadata,
      canvasState,
      dragDelta,
      newLocalFrame,
    )
    commands.push(...updatePinsCommands)
    if (shouldReparent) {
      commands.push(reparentElement('permanent', path, targetParent))
    }
    return { commands: commands, intendedBounds: intendedBounds }
  } else {
    return { commands: [], intendedBounds: [] }
  }
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
      const prop = getLayoutProperty(p, right(element.props), ['style'])
      return isRight(prop) && prop.value != null
    })
    const verticalProps = (['top', 'bottom', 'height'] as Array<LayoutPinnedProp>).filter((p) => {
      const prop = getLayoutProperty(p, right(element.props), ['style'])
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

function escapeHatchAllowed(
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  strategyState: StrategyState,
): boolean {
  if (interactionData.modifiers.cmd) {
    return true
  }
  // flex children with siblings switches to escape hatch when the cursor reaches the parent bounds
  // for flow elements and flex child without siblings the conversion automatically starts on drag
  if (strategyState.customStrategyState.escapeHatchActivated) {
    return true
  }
  const selectedElementsHaveSiblingsAndFlex = canvasState.selectedElements.some((path) => {
    return (
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        path,
        strategyState.startingMetadata,
      ) && MetadataUtils.getSiblings(strategyState.startingMetadata, path).length > 1
    )
  })
  if (selectedElementsHaveSiblingsAndFlex) {
    const cursorPosition = offsetPoint(
      interactionData.dragStart,
      interactionData.drag ?? zeroCanvasPoint,
    )
    const parentBounds = mapDropNulls((path) => {
      return MetadataUtils.findElementByElementPath(strategyState.startingMetadata, path)
        ?.specialSizeMeasurements.immediateParentBounds
    }, canvasState.selectedElements)
    return parentBounds.some((frame) => !rectContainsPoint(frame, cursorPosition))
  } else {
    return true
  }
}

function collectHighlightCommand(
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  strategyState: StrategyState,
): CanvasCommand {
  const siblingFrames = stripNulls(
    canvasState.selectedElements.flatMap((path) => {
      return MetadataUtils.getSiblings(strategyState.startingMetadata, path)
        .filter((sibling) =>
          canvasState.selectedElements.every(
            (selected) => !EP.pathsEqual(selected, sibling.elementPath),
          ),
        )
        .map((element) =>
          MetadataUtils.getFrameInCanvasCoords(element.elementPath, strategyState.startingMetadata),
        )
    }),
  )

  const draggedFrames = mapDropNulls((path) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(path, strategyState.startingMetadata)
    if (frame != null) {
      return offsetRect(frame, interactionData.drag ?? zeroCanvasPoint)
    } else {
      return null
    }
  }, canvasState.selectedElements)
  return showOutlineHighlight('transient', [...siblingFrames, ...draggedFrames])
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
      const containingBlockPath = MetadataUtils.findContainingBlock(metadata, path)
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
      if (canvasFrame != null && nearestSelectedAncestorFrame != null) {
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
  return EP.getOrderedPathsByDepth(selectedElements).filter((selection) =>
    EP.isDescendantOf(target, selection),
  )[0]
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
  const margin = specialSizeMeasurements?.margin
  const marginPoint: LocalPoint = {
    x: -(margin?.left ?? 0),
    y: -(margin?.top ?? 0),
  } as LocalPoint
  const frameWithoutMargin = offsetRect(frame, marginPoint)
  const updatedFrame = offsetRect(frameWithoutMargin, asLocal(dragDelta ?? zeroCanvasRect))
  const fullFrame = getFullFrame(updatedFrame)
  const pinsToSet = filterPinsToSet(path, canvasState)

  let commands: Array<SetCssLengthProperty> = []
  fastForEach(pinsToSet, (framePin) => {
    const pinValue = pinValueToSet(framePin, fullFrame, parentFrame)
    commands.push(
      setCssLengthProperty(
        'permanent',
        path,
        stylePropPathMappingFn(framePin, ['style']),
        pinValue,
        isHorizontalPoint(framePointForPinnedProp(framePin))
          ? parentFrame?.width
          : parentFrame?.height,
      ),
    )
  })
  return commands
}
