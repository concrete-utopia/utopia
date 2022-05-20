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
  CanvasRectangle,
  CanvasVector,
  LocalPoint,
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
import { CanvasCommand } from '../commands/commands'
import { convertToAbsolute } from '../commands/convert-to-absolute-command'
import { setCssLengthProperty } from '../commands/set-css-length-command'
import { showOutlineHighlight } from '../commands/show-outline-highlight-command'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { AnimationTimer, PieTimerControl } from '../controls/select-mode/pie-timer'
import { getTransientMoveCommands, snapDragAbsoluteMove } from './absolute-move-strategy'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
} from './canvas-strategy-types'
import { DragInteractionData, StrategyState } from './interaction-state'

export const escapeHatchStrategy: CanvasStrategy = {
  id: 'ESCAPE_HATCH_STRATEGY',
  name: 'Escape Hatch',
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
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
    {
      control: PieTimerControl,
      key: 'pie-timer-control',
      show: 'visible-only-while-active',
    },
  ],
  fitness: (canvasState, interactionState, strategyState) => {
    return escapeHatchStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA' &&
      escapeHatchAllowed(canvasState, interactionState.interactionData, strategyState)
      ? 2
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (interactionState.interactionData.type === 'DRAG') {
      let escapeHatchActivated = strategyState.customStrategyState.escapeHatchActivated ?? false
      if (
        interactionState.interactionData.globalTime - interactionState.lastInteractionTime >
        AnimationTimer
      ) {
        escapeHatchActivated = true
      }
      if (escapeHatchActivated) {
        const { snappedDragVector, guidelinesWithSnappingVector } = snapDragAbsoluteMove(
          canvasState,
          interactionState.interactionData,
          strategyState,
        )

        const conversionCommands = getEscapeHatchCommands(
          canvasState.selectedElements,
          strategyState.startingMetadata,
          canvasState,
          snappedDragVector,
        )

        const highlightCommand = collectHighlightCommand(
          canvasState,
          interactionState.interactionData,
          strategyState,
        )
        const moveTransientCommands = getTransientMoveCommands(guidelinesWithSnappingVector)
        return {
          commands: [...conversionCommands, highlightCommand, ...moveTransientCommands],
          customState: {
            ...strategyState.customStrategyState,
            escapeHatchActivated,
          },
        }
      } else {
        return {
          commands: [],
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
): Array<CanvasCommand> {
  const moveAndPositionCommands = collectMoveCommandsForSelectedElements(
    selectedElements,
    metadata,
    canvasState,
    dragDelta,
  )
  const siblingCommands = collectSiblingCommands(selectedElements, metadata, canvasState)
  return [...moveAndPositionCommands, ...siblingCommands]
}

function collectMoveCommandsForSelectedElements(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  canvasState: InteractionCanvasState,
  dragDelta: CanvasVector | null,
): Array<CanvasCommand> {
  return flatMapArray(
    (path) => collectSetLayoutPropCommands(path, metadata, canvasState, dragDelta),
    selectedElements,
  )
}

function collectSiblingCommands(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  canvasState: InteractionCanvasState,
): Array<CanvasCommand> {
  const siblings = selectedElements
    .flatMap((path) => {
      return MetadataUtils.getSiblings(metadata, path).map((element) => element.elementPath)
    })
    .filter((sibling) => selectedElements.every((path) => !EP.pathsEqual(path, sibling)))

  return flatMapArray(
    (path) => collectSetLayoutPropCommands(path, metadata, canvasState, null),
    siblings,
  )
}

function collectSetLayoutPropCommands(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  canvasState: InteractionCanvasState,
  dragDelta: CanvasVector | null,
): Array<CanvasCommand> {
  const frame = MetadataUtils.getFrame(path, metadata)
  if (frame != null) {
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

    let commands: Array<CanvasCommand> = [convertToAbsolute('permanent', path)]
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
  } else {
    return []
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
