import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { InteractionCanvasState, InteractionState } from './interaction-state'
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

  // Determines if we should show the controls that this strategy renders
  isApplicable: (
    canvasState: InteractionCanvasState,
    interactionState: InteractionState | null,
    metadata: ElementInstanceMetadataMap,
  ) => boolean

  // The controls to render when this strategy is applicable, regardless of if it is currently active
  controlsToRender: Array<ControlWithKey>

  // As before, for determining the relative ordering of applicable strategies during an interaction, and therefore which one to apply
  fitness: (
    canvasState: InteractionCanvasState,
    interactionState: InteractionState,
    sessionState: SessionStateState,
  ) => number

  // Returns the commands that inform how the model and the editor should be updated
  apply: (
    canvasState: InteractionCanvasState,
    interactionState: InteractionState,
    sessionState: SessionStateState,
  ) => StrategyApplicationResult
}
