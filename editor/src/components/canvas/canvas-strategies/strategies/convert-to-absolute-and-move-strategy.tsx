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
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import {
  asLocal,
  CanvasPoint,
  CanvasRectangle,
  canvasRectangleToLocalRectangle,
  CanvasVector,
  LocalPoint,
  LocalRectangle,
  offsetRect,
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
import { applyMoveCommon, getDragTargets } from './shared-move-strategies-helpers'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { styleStringInArray } from '../../../../utils/common-constants'

export function convertToAbsoluteAndMoveStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const filteredSelectedElements = getDragTargets(selectedElements)
  if (
    filteredSelectedElements.length === 0 ||
    !filteredSelectedElements.every((element) => {
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

  return {
    id: 'CONVERT_TO_ABSOLUTE_AND_MOVE_STRATEGY',
    name: 'Move (Abs)',
    controlsToRender: [
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: filteredSelectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: filteredSelectedElements },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ], // Uses existing hooks in select-mode-hooks.tsx
    fitness:
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA'
        ? interactionSession.interactionData.spacePressed
          ? 5
          : 0.5
        : 0,
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

export function getEscapeHatchCommands(
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

  /**
   * Multiselect reparents the selection to a common ancestor
   * starting from the inner elements
   */
  const commonAncestor = EP.getCommonParent(selectedElements, false)
  const sortedElements = EP.getOrderedPathsByDepth(selectedElements)

  /**
   * It's possible to have descendants where the layout is defined by an ancestor
   * these are offset here as the new layout parents will be the selected elements
   */
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

    let commands: Array<CanvasCommand> = [convertToAbsolute('always', path)]
    const updatePinsCommands = createUpdatePinsCommands(
      path,
      metadata,
      canvasState,
      dragDelta,
      newLocalFrame,
    )
    commands.push(...updatePinsCommands)
    if (shouldReparent) {
      const outcomeResult = getReparentOutcome(
        canvasState.builtInDependencies,
        canvasState.projectContents,
        canvasState.nodeModules,
        canvasState.openFile,
        pathToReparent(path),
        targetParent,
        'always',
      )
      if (outcomeResult != null) {
        commands.push(...outcomeResult.commands)
      }
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
