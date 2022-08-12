import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { FlexReparentTargetIndicator } from '../controls/select-mode/flex-reparent-target-indicator'
import { CanvasStrategy } from './canvas-strategy-types'
import { applyFlexReparent, findReparentStrategy } from './reparent-strategy-helpers'

export const flexReparentToFlexStrategy: CanvasStrategy = {
  id: 'FLEX_REPARENT_TO_FLEX',
  name: 'Flex Reparent to Flex',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length == 1) {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        canvasState.selectedElements[0],
        metadata,
      )
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
  fitness: (canvasState, interactionState, strategyState) => {
    // All 4 reparent strategies use the same fitness function findReparentStrategy
    const reparentStrategy = findReparentStrategy(
      canvasState,
      interactionState,
      strategyState,
    ).strategy
    if (reparentStrategy === 'FLEX_REPARENT_TO_FLEX') {
      return 3
    }
    return 0
  },
  apply: (canvasState, interactionSession, strategyState) => {
    return applyFlexReparent('do-not-strip-props', canvasState, interactionSession, strategyState)
  },
}
