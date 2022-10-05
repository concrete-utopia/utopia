import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../controls/select-mode/flex-reparent-target-indicator'
import {
  CanvasStrategy,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'
import { applyFlexReparent, getFitnessForReparentStrategy } from './reparent-strategy-helpers'
import { getDragTargets } from './shared-move-strategies-helpers'

export function absoluteReparentToFlexStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const filteredSelectedElements = getDragTargets(selectedElements)
  if (
    filteredSelectedElements.length === 1 &&
    interactionSession != null &&
    interactionSession.interactionData.type === 'DRAG'
  ) {
    const elementMetadata = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      filteredSelectedElements[0],
    )

    if (elementMetadata?.specialSizeMeasurements.position === 'absolute') {
      return {
        id: 'ABSOLUTE_REPARENT_TO_FLEX',
        name: 'Reparent (Flex)',
        controlsToRender: [
          {
            control: DragOutlineControl,
            key: 'ghost-outline-control',
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
          {
            control: FlexReparentTargetIndicator,
            key: 'flex-reparent-target-indicator',
            show: 'visible-only-while-active',
          },
        ],
        fitness: getFitnessForReparentStrategy(
          'ABSOLUTE_REPARENT_TO_FLEX',
          canvasState,
          interactionSession,
          'use-strict-bounds',
        ),
        apply: () => {
          return applyFlexReparent('strip-absolute-props', canvasState, interactionSession)
        },
      }
    }
  }

  return null
}
