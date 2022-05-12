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
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { CanvasCommand } from '../commands/commands'
import { convertToAbsolute } from '../commands/convert-to-absolute-command'
import { setCssLengthProperty } from '../commands/set-css-length-command'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { CanvasStrategy } from './canvas-strategy-types'

export const AnimationTimer = 2000
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
  controlsToRender: [
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      // show: 'visible-only-while-active',
      show: 'always-visible',
    },
  ],
  fitness: (canvasState, interactionState, strategyState) => {
    return escapeHatchStrategy.isApplicable(
      canvasState,
      interactionState,
      strategyState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (interactionState.interactionData.type === 'DRAG') {
      // TODO if the element has siblings the escape hatch is triggered when pulled outside of the parent bounds
      // without siblings it's automatically converted
      if (interactionState.globalTime - interactionState.lastInteractionTime > AnimationTimer) {
        const moveAndPositionCommands = collectMoveCommandsForSelectedElements(
          canvasState.selectedElements,
          strategyState.startingMetadata,
          interactionState.interactionData.drag,
        )
        const siblingCommands = collectSiblingCommands(
          canvasState.selectedElements,
          strategyState.startingMetadata,
        )
        return [...moveAndPositionCommands, ...siblingCommands]
      }
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}

function collectMoveCommandsForSelectedElements(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  dragDelta: CanvasVector | null,
): Array<CanvasCommand> {
  return flatMapArray(
    (path) => collectSetLayoutPropCommands(path, metadata, dragDelta),
    selectedElements,
  )
}

function collectSiblingCommands(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): Array<CanvasCommand> {
  const siblings = selectedElements
    .flatMap((path) => {
      return MetadataUtils.getSiblings(metadata, path).map((element) => element.elementPath)
    })
    .filter((sibling) => selectedElements.every((path) => !EP.pathsEqual(path, sibling)))

  return flatMapArray((path) => collectSetLayoutPropCommands(path, metadata, null), siblings)
}

function collectSetLayoutPropCommands(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  dragDelta: CanvasVector | null,
) {
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
    return [
      convertToAbsolute('permanent', path),
      setCssLengthProperty(
        'permanent',
        path,
        stylePropPathMappingFn('left', ['style']),
        updatedFrame.x,
      ),
      setCssLengthProperty(
        'permanent',
        path,
        stylePropPathMappingFn('top', ['style']),
        updatedFrame.y,
      ),
      setCssLengthProperty(
        'permanent',
        path,
        stylePropPathMappingFn('width', ['style']),
        updatedFrame.width,
      ),
      setCssLengthProperty(
        'permanent',
        path,
        stylePropPathMappingFn('height', ['style']),
        updatedFrame.height,
      ),
    ]
  } else {
    return []
  }
}
