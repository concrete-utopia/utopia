import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { CanvasPoint, CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { InteractionCanvasState, InteractionState } from './interaction-state'
import type { EdgePosition } from '../canvas-types'
import { CanvasCommand } from '../commands/commands'
import { SessionStateState } from './interaction-state'

export type StrategyApplicationResult = Array<CanvasCommand>

export interface ControlWithKey {
  control: React.FC
  key: string
  show:
    | 'always-visible'
    | 'visible-only-while-active'
    | 'visible-except-when-other-strategy-is-active'
}

export interface CanvasStrategy {
  name: string // We'd need to do something to guarantee uniqueness here if using this for the commands' reason

  strategyGroups: Set<string>

  isApplicable: (
    canvasState: InteractionCanvasState,
    interactionState: InteractionState | null,
    metadata: ElementInstanceMetadataMap,
  ) => boolean
  // Determines if we should show the controls that this strategy renders
  // Maybe this can just be rolled into controlsToRender?

  controlsToRender: Array<ControlWithKey>
  // The controls to render when this strategy is applicable, regardless of if it is currently active

  fitness: (
    canvasState: InteractionCanvasState,
    interactionState: InteractionState,
    sessionState: SessionStateState,
  ) => number
  // As before, for determining the relative ordering of applicable strategies during an interaction, and therefore which one to apply

  apply: (
    canvasState: InteractionCanvasState,
    interactionState: InteractionState,
    sessionState: SessionStateState,
  ) => StrategyApplicationResult
  // Returns the commands that inform how the model and the editor should be updated
}

interface BoundingArea {
  type: 'BOUNDING_AREA'
  target: ElementPath
}

interface ResizeHandle {
  type: 'RESIZE_HANDLE'
  edgePosition: EdgePosition
}

interface FlexGapHandle {
  type: 'FLEX_GAP_HANDLE'
}

interface KeyboardCatcherControl {
  type: 'KEYBOARD_CATCHER_CONTROL'
}

export type CanvasControlType = BoundingArea | ResizeHandle | FlexGapHandle | KeyboardCatcherControl
