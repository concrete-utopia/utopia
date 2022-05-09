import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { flatMapArray, mapDropNulls, stripNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  asLocal,
  CanvasVector,
  LocalPoint,
  offsetRect,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { setLocalFrame, SetLocalFrameCommand } from '../commands/bounding-box-move-command'
import { CanvasCommand } from '../commands/commands'
import { convertToAbsolute } from '../commands/convert-to-absolute-command'
import { CanvasStrategy } from './canvas-strategy-types'

export const escapeHatchStrategy: CanvasStrategy = {
  id: 'ESCAPE_HATCH_STRATEGY',
  name: 'Escape Hatch',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length > 0) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
        return elementMetadata?.specialSizeMeasurements.position === 'static'
      })
    } else {
      return false
    }
  },
  controlsToRender: [],
  fitness: (canvasState, interactionState, sessionState) => {
    return escapeHatchStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (interactionState.interactionData.type === 'DRAG') {
      // if (interactionState.globalTime - interactionState.lastInteractionTime > AnimationTimer) {
      //   return [switchToAbsolute('permanent', canvasState.selectedElements)]
      // }
      const dragDelta = interactionState.interactionData.drag
      const moveAndPositionCommands = collectMoveCommandsForSelectedElements(
        dragDelta,
        canvasState.selectedElements,
        strategyState.startingMetadata,
      )
      const siblingCommands = collectSiblingCommands(
        canvasState.selectedElements,
        strategyState.startingMetadata,
      )
      return [...moveAndPositionCommands, ...siblingCommands]
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}

function collectMoveCommandsForSelectedElements(
  dragDelta: CanvasVector | null,
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): Array<CanvasCommand> {
  return flatMapArray((path) => {
    const frame = MetadataUtils.getFrame(path, metadata)
    if (frame != null) {
      const margin = MetadataUtils.findElementByElementPath(metadata, path)?.specialSizeMeasurements
        .margin
      const marginPoint: LocalPoint = {
        x: -(margin?.left ?? 0),
        y: -(margin?.top ?? 0),
      } as LocalPoint
      const frameWithoutMargin = offsetRect(frame, marginPoint)
      const updatedFrame = offsetRect(frameWithoutMargin, asLocal(dragDelta ?? zeroCanvasRect))
      return [convertToAbsolute('permanent', path), setLocalFrame('permanent', path, updatedFrame)]
    } else {
      return []
    }
  }, selectedElements)
}

function collectSiblingCommands(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): Array<CanvasCommand> {
  const siblings = selectedElements
    .flatMap((path) => {
      return [...MetadataUtils.getSiblings(metadata, path).map((element) => element.elementPath)]
    })
    .filter((sibling) => selectedElements.every((path) => !EP.pathsEqual(path, sibling)))

  return flatMapArray((path) => {
    const frame = MetadataUtils.getFrame(path, metadata)
    if (frame != null) {
      const margin = MetadataUtils.findElementByElementPath(metadata, path)?.specialSizeMeasurements
        .margin
      const marginPoint: LocalPoint = {
        x: -(margin?.left ?? 0),
        y: -(margin?.top ?? 0),
      } as LocalPoint
      const frameWithoutMargin = offsetRect(frame, marginPoint)
      return [
        convertToAbsolute('permanent', path),
        setLocalFrame('permanent', path, frameWithoutMargin),
      ]
    } else {
      return []
    }
  }, siblings)
}
