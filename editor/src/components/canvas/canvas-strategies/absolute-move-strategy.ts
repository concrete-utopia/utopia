import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  offsetPoint,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { runLegacyAbsoluteMoveSnapping } from '../controls/guideline-helpers'
import { ConstrainedDragAxis, Guideline, GuidelineWithSnappingVector } from '../guideline'
import { CanvasStrategy } from './canvas-strategy-types'
import { getAbsoluteMoveCommandsForSelectedElement } from './shared-absolute-move-strategy-helpers'

export const absoluteMoveStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_MOVE',
  name: 'Absolute Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length > 0) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
      })
    } else {
      return false
    }
  },
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return absoluteMoveStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
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
      const { snappedDragVector, guidelinesWithSnappingVector } = snapDrag(
        drag,
        null, // TODO constrain drag axis!
        sessionState.startingMetadata,
        canvasState.selectedElements,
        canvasState.scale,
      )
      const commandsForSelectedElements = canvasState.selectedElements.flatMap((selectedElement) =>
        getAbsoluteMoveCommandsForSelectedElement(
          selectedElement,
          snappedDragVector,
          canvasState,
          sessionState,
        ),
      )
      return [
        ...commandsForSelectedElements,
        updateHighlightedViews('transient', []),
        setSnappingGuidelines('transient', guidelinesWithSnappingVector),
      ]
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}

function snapDrag(
  drag: CanvasPoint,
  constrainedDragAxis: ConstrainedDragAxis | null,
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
  canvasScale: number,
): {
  snappedDragVector: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVector>
} {
  const multiselectBounds = getMultiselectBounds(jsxMetadata, selectedElements)

  // This is the entry point to extend the list of snapping strategies, if we want to add more

  const { snappedDragVector, guidelinesWithSnappingVector } = runLegacyAbsoluteMoveSnapping(
    drag,
    constrainedDragAxis,
    jsxMetadata,
    selectedElements,
    canvasScale,
    multiselectBounds,
  )

  return { snappedDragVector, guidelinesWithSnappingVector }
}

function getMultiselectBounds(
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
): CanvasRectangle | null {
  const frames = mapDropNulls((element) => {
    return MetadataUtils.getFrameInCanvasCoords(element, jsxMetadata)
  }, selectedElements)

  return boundingRectangleArray(frames)
}
