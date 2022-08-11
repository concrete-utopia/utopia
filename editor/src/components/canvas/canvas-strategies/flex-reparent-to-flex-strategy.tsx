import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { offsetPoint } from '../../../core/shared/math-utils'
import { CSSCursor } from '../canvas-types'
import { CanvasCommand } from '../commands/commands'
import { reorderElement } from '../commands/reorder-element-command'
import { reparentElement } from '../commands/reparent-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { getReorderIndex } from './flex-reorder-strategy'
import { ifAllowedToReparent } from './reparent-helpers'
import {
  applyFlexReparent,
  findReparentStrategy,
  getReparentTargetForFlexElement,
} from './reparent-strategy-helpers'
import { getReparentCommands } from './reparent-utils'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

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
