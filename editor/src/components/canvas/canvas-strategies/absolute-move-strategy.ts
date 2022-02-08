import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { CanvasStrategy } from '../../../interactions_proposal'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { adjustNumberProperty, setProperty, wildcardPatch } from '../commands/commands'

export const absoluteMoveStrategy: CanvasStrategy = {
  name: 'Absolute Move',
  strategyGroups: new Set(),
  isApplicable: (canvasState, _interactionState, pathMappings) => {
    if (canvasState.selectedElements.length === 1) {
      let metadata = MetadataUtils.findElementByElementPath(
        canvasState.metadata,
        canvasState.selectedElements[0],
      )
      if (metadata == null) {
        const selectedElement = canvasState.selectedElements[0]
        const pathMapping = pathMappings.find(
          (mapping) => mapping.from === selectedElement || mapping.to === selectedElement,
        )
        if (pathMapping == null) {
          return false
        }
        const mappedElementPath =
          pathMapping.from === selectedElement ? pathMapping.to : pathMapping.from
        metadata = MetadataUtils.findElementByElementPath(canvasState.metadata, mappedElementPath)
      }

      return metadata?.specialSizeMeasurements.position === 'absolute'
    } else {
      return false
    }
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState, pathMappings) => {
    return absoluteMoveStrategy.isApplicable(canvasState, interactionState, pathMappings) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      const drag = interactionState.interactionData.drag
      return canvasState.selectedElements.flatMap((selectedElement) => [
        adjustNumberProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn('left', ['style']),
          drag.x,
        ),
        adjustNumberProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn('top', ['style']),
          drag.y,
        ),
        wildcardPatch('transient', {
          highlightedViews: {
            $set: [],
          },
        }),
      ])
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}
