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
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { runLegacySnapping } from '../controls/guideline-helpers'
import { ConstrainedDragAxis, Guideline } from '../guideline'
import { CanvasStrategy } from './canvas-strategy-types'

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
      const { snappedDragVector, guidelines } = snapDrag(
        drag,
        null, // TODO constrain drag axis!
        sessionState.startingMetadata,
        canvasState.selectedElements,
        canvasState.scale,
      )
      const commandsForSelectedElements = canvasState.selectedElements.flatMap(
        (selectedElement) => {
          const element: JSXElement | null = getElementFromProjectContents(
            selectedElement,
            canvasState.projectContents,
            canvasState.openFile,
          )
          const elementParentBounds =
            MetadataUtils.findElementByElementPath(
              sessionState.startingMetadata, // TODO should this be using the current metadata?
              selectedElement,
            )?.specialSizeMeasurements.immediateParentBounds ?? null

          if (element == null) {
            return []
          }

          return createMoveCommandsForElement(
            element,
            selectedElement,
            snappedDragVector,
            elementParentBounds,
          )
        },
      )
      return [
        ...commandsForSelectedElements,
        wildcardPatch('transient', {
          highlightedViews: {
            $set: [],
          },
          canvas: {
            controls: {
              snappingGuidelines: {
                $set: guidelines, // these will be used by the guideline controls
              },
            },
          },
        }),
      ]
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}

function createMoveCommandsForElement(
  element: JSXElement,
  selectedElement: ElementPath,
  drag: CanvasVector,
  elementParentBounds: CanvasRectangle | null,
): AdjustCssLengthProperty[] {
  return mapDropNulls(
    (pin) => {
      const horizontal = isHorizontalPoint(
        // TODO avoid using the loaded FramePoint enum
        framePointForPinnedProp(pin),
      )
      const negative = pin === 'right' || pin === 'bottom'
      const value = getLayoutProperty(pin, right(element.props), ['style'])
      if (isRight(value) && value.value != null) {
        // TODO what to do about missing properties?
        return adjustCssLengthProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn(pin, ['style']),
          (horizontal ? drag.x : drag.y) * (negative ? -1 : 1),
          horizontal ? elementParentBounds?.width : elementParentBounds?.height,
          true,
        )
      } else {
        return null
      }
    },
    ['top', 'bottom', 'left', 'right'] as const,
  )
}

function snapDrag(
  drag: CanvasPoint,
  constrainedDragAxis: ConstrainedDragAxis | null,
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
  canvasScale: number,
): { snappedDragVector: CanvasPoint; guidelines: Array<Guideline> } {
  const multiselectBounds = getMultiselectBounds(jsxMetadata, selectedElements)

  // This is the entry point to extend the list of snapping strategies, if we want to add more

  const { snappedDragVector, guidelines } = runLegacySnapping(
    drag,
    constrainedDragAxis,
    jsxMetadata,
    selectedElements,
    canvasScale,
    multiselectBounds,
  )

  return { snappedDragVector, guidelines: guidelines }
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
