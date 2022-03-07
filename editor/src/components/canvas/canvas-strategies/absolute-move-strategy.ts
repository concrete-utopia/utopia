import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { JSXElement } from '../../../core/shared/element-template'
import { CanvasRectangle, CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { adjustNumberProperty } from '../commands/adjust-number-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { CanvasStrategy } from './canvas-strategy-types'

export const absoluteMoveStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_MOVE',
  name: 'Absolute Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        metadata,
        canvasState.selectedElements[0],
      )

      return elementMetadata?.specialSizeMeasurements.position === 'absolute'
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
      const commandsForSelectedElements = canvasState.selectedElements.flatMap(
        (selectedElement) => {
          const element: JSXElement | null = withUnderlyingTarget(
            selectedElement,
            canvasState.projectContents,
            {},
            canvasState.openFile,
            null,
            (_, e) => e,
          )
          const elementParentBounds =
            MetadataUtils.findElementByElementPath(
              sessionState.startingMetadata, // TODO should this be using the current metadata?
              selectedElement,
            )?.specialSizeMeasurements.immediateParentBounds ?? null

          if (element == null) {
            return []
          }

          return createMoveCommandsForElement(element, selectedElement, drag, elementParentBounds)
        },
      )
      return [
        ...commandsForSelectedElements,
        wildcardPatch('transient', {
          highlightedViews: {
            $set: [],
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
