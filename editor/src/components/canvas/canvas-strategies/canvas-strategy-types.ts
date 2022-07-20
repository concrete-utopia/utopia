import { AllElementProps } from 'src/components/editor/store/editor-state'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'
import { CanvasCommand } from '../commands/commands'
import { InteractionSession, StrategyState } from './interaction-state'

// TODO: fill this in, maybe make it an ADT for different strategies
export interface CustomStrategyState {
  escapeHatchActivated: boolean
  lastReorderIdx: number | null
  duplicatedElementNewUids: { [elementPath: string]: string }
}

export function defaultCustomStrategyState(): CustomStrategyState {
  return {
    escapeHatchActivated: false,
    lastReorderIdx: null,
    duplicatedElementNewUids: {},
  }
}

export interface StrategyApplicationResult {
  commands: Array<CanvasCommand>
  customState: CustomStrategyState | null // null means the previous custom strategy state should be kept
}

export const emptyStrategyApplicationResult = {
  commands: [],
  customState: null,
}

export interface ControlWithKey {
  control: React.FC<React.PropsWithChildren<unknown>>
  key: string
  show:
    | 'always-visible'
    | 'visible-only-while-active'
    | 'visible-except-when-other-strategy-is-active'
}

export interface InteractionCanvasState {
  selectedElements: Array<ElementPath>
  projectContents: ProjectContentTreeRoot
  openFile: string | null | undefined
  scale: number
  canvasOffset: CanvasVector
}

export type CanvasStrategyId =
  | 'ABSOLUTE_MOVE'
  | 'ABSOLUTE_REPARENT'
  | 'ABSOLUTE_DUPLICATE'
  | 'ABSOLUTE_RESIZE_BOUNDING_BOX'
  | 'KEYBOARD_ABSOLUTE_MOVE'
  | 'KEYBOARD_ABSOLUTE_RESIZE'
  | 'ESCAPE_HATCH_STRATEGY'
  | 'FLEX_REORDER'
  | 'ABSOLUTE_REPARENT_TO_FLEX'

export interface CanvasStrategy {
  id: CanvasStrategyId // We'd need to do something to guarantee uniqueness here if using this for the commands' reason
  name: string

  // Determines if we should show the controls that this strategy renders
  isApplicable: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
  ) => boolean

  // The controls to render when this strategy is applicable, regardless of if it is currently active
  controlsToRender: Array<ControlWithKey>

  // As before, for determining the relative ordering of applicable strategies during an interaction, and therefore which one to apply
  fitness: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyState,
  ) => number

  // Returns the commands that inform how the model and the editor should be updated
  apply: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyState,
  ) => StrategyApplicationResult
}
