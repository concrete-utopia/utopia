import { AllElementProps } from '../../../components/editor/store/editor-state'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath, NodeModules } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'
import { InsertionSubject } from '../../editor/editor-modes'
import { CanvasCommand } from '../commands/commands'
import {
  InteractionSession,
  StrategyApplicationStatus,
  StrategyState,
  StrategyStateNew,
} from './interaction-state'

// TODO: fill this in, maybe make it an ADT for different strategies
export interface CustomStrategyState {
  escapeHatchActivated: boolean
  lastReorderIdx: number | null
  duplicatedElementNewUids: { [elementPath: string]: string }
  previousReorderTargetSiblingUnderMouse: ElementPath | null
}

export type CustomStrategyStatePatch = Partial<CustomStrategyState>

export function defaultCustomStrategyState(): CustomStrategyState {
  return {
    escapeHatchActivated: false,
    lastReorderIdx: null,
    duplicatedElementNewUids: {},
    previousReorderTargetSiblingUnderMouse: null,
  }
}

export interface StrategyApplicationResult {
  commands: Array<CanvasCommand>
  customStatePatch: CustomStrategyStatePatch
  status: StrategyApplicationStatus
}

export const emptyStrategyApplicationResult: StrategyApplicationResult = {
  commands: [],
  customStatePatch: {},
  status: 'success',
}

export function strategyApplicationResult(
  commands: Array<CanvasCommand>,
  customStatePatch: CustomStrategyStatePatch = {},
  status: StrategyApplicationStatus = 'success',
): StrategyApplicationResult {
  return {
    commands: commands,
    customStatePatch: customStatePatch,
    status: status,
  }
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
  interactionTarget: InteractionTarget
  projectContents: ProjectContentTreeRoot
  nodeModules: NodeModules
  builtInDependencies: BuiltInDependencies
  openFile: string | null | undefined
  scale: number
  canvasOffset: CanvasVector
  startingMetadata: ElementInstanceMetadataMap
  startingAllElementProps: AllElementProps
}

export type InteractionTarget = TargetPaths | InsertionSubjects

interface TargetPaths {
  type: 'TARGET_PATHS'
  elements: Array<ElementPath>
}

export function targetPaths(elements: Array<ElementPath>): TargetPaths {
  return {
    type: 'TARGET_PATHS',
    elements: elements,
  }
}

interface InsertionSubjects {
  type: 'INSERTION_SUBJECTS'
  subjects: Array<InsertionSubject>
}

export function insertionSubjects(subjects: Array<InsertionSubject>): InsertionSubjects {
  return {
    type: 'INSERTION_SUBJECTS',
    subjects: subjects,
  }
}

export function getTargetPathsFromInteractionTarget(target: InteractionTarget): Array<ElementPath> {
  if (target.type === 'TARGET_PATHS') {
    return target.elements
  }
  return []
}

export function getInsertionSubjectsFromInteractionTarget(
  target: InteractionTarget,
): Array<InsertionSubject> {
  if (target.type === 'INSERTION_SUBJECTS') {
    return target.subjects
  }
  return []
}

export type CanvasStrategyId =
  | 'ABSOLUTE_MOVE'
  | 'ABSOLUTE_REPARENT'
  | 'FORCED_ABSOLUTE_REPARENT'
  | 'ABSOLUTE_DUPLICATE'
  | 'ABSOLUTE_RESIZE_BOUNDING_BOX'
  | 'KEYBOARD_ABSOLUTE_MOVE'
  | 'KEYBOARD_ABSOLUTE_RESIZE'
  | 'CONVERT_TO_ABSOLUTE_AND_MOVE_STRATEGY'
  | 'FLEX_REORDER'
  | 'ABSOLUTE_REPARENT_TO_FLEX'
  | 'FLEX_REPARENT_TO_ABSOLUTE'
  | 'FORCED_FLEX_REPARENT_TO_ABSOLUTE'
  | 'FLEX_REPARENT_TO_FLEX'
  | 'DRAG_TO_INSERT'
  | 'FLOW_REORDER'
  | 'FLOW_REORDER_SLIDER'
  | 'LOOK_FOR_APPLICABLE_PARENT_ID'
  | 'DRAW_TO_INSERT'
  | 'FLEX_RESIZE_BASIC'

export type InteractionLifecycle = 'mid-interaction' | 'end-interaction'

export interface CanvasStrategy {
  id: CanvasStrategyId // We'd need to do something to guarantee uniqueness here if using this for the commands' reason

  name: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyStateNew,
  ) => string

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
    strategyState: StrategyStateNew,
  ) => number

  // Returns the commands that inform how the model and the editor should be updated
  apply: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyStateNew,
    strategyLifecycle: InteractionLifecycle,
  ) => StrategyApplicationResult
}

export const ControlDelay = 600
