import React from 'react'
import { AllElementProps } from '../../editor/store/editor-state'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath, NodeModules } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'
import { InsertionSubject } from '../../editor/editor-modes'
import { CanvasCommand } from '../commands/commands'
import { InteractionSession, StrategyApplicationStatus } from './interaction-state'

// TODO: fill this in, maybe make it an ADT for different strategies
export interface CustomStrategyState {
  escapeHatchActivated: boolean
  lastReorderIdx: number | null
  duplicatedElementNewUids: { [elementPath: string]: string }
}

export type CustomStrategyStatePatch = Partial<CustomStrategyState>

export function defaultCustomStrategyState(): CustomStrategyState {
  return {
    escapeHatchActivated: false,
    lastReorderIdx: null,
    duplicatedElementNewUids: {},
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

export interface MoveStrategy {
  strategy: CanvasStrategy
  dragType: 'absolute' | 'static'
}

export interface ControlForStrategy<P> {
  type: 'ControlForStrategy'
  control: React.FC<P>
}

export function controlForStrategyMemoized<P>(control: React.FC<P>): ControlForStrategy<P> {
  return { type: 'ControlForStrategy', control: React.memo<any>(control) }
}

export type WhenToShowControl =
  | 'always-visible'
  | 'visible-only-while-active'
  | 'visible-except-when-other-strategy-is-active'

export interface ControlWithProps<P> {
  control: ControlForStrategy<P>
  props: P
  key: string
  show: WhenToShowControl
}

export function controlWithProps<P>(value: ControlWithProps<P>): ControlWithProps<P> {
  return value
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

export function isTargetPaths(target: InteractionTarget): target is TargetPaths {
  return target.type === 'TARGET_PATHS'
}

export interface InsertionSubjects {
  type: 'INSERTION_SUBJECTS'
  subjects: Array<InsertionSubject>
}

export function insertionSubjects(subjects: Array<InsertionSubject>): InsertionSubjects {
  return {
    type: 'INSERTION_SUBJECTS',
    subjects: subjects,
  }
}

export function isInsertionSubjects(target: InteractionTarget): target is InsertionSubjects {
  return target.type === 'INSERTION_SUBJECTS'
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

export type TestStrategyId =
  | 'WORST_STRATEGY'
  | 'UNFIT_STRATEGY'
  | 'TEST_STRATEGY'
  | 'EMPTY_TEST_STRATEGY'
  | 'AVERAGE_STRATEGY'
  | 'BEST_STRATEGY'

export type MoveOrReorderStrategyId =
  | 'ABSOLUTE_DUPLICATE'
  | 'KEYBOARD_ABSOLUTE_MOVE'
  | 'KEYBOARD_REORDER'
  | 'CONVERT_TO_ABSOLUTE_AND_MOVE_STRATEGY'
  | 'REORDER_SLIDER'

export type ReparentStrategy = 'REPARENT_AS_ABSOLUTE' | 'REPARENT_AS_STATIC'

export type LeafStrategyId =
  | 'FLEX_RESIZE'
  | 'ABSOLUTE_RESIZE_BOUNDING_BOX'
  | 'KEYBOARD_ABSOLUTE_RESIZE'
  | 'FLEX_RESIZE_BASIC'
  | 'FLEX_RESIZE'
  | 'BASIC_RESIZE'
  | 'DO_NOTHING'
  | 'SET_PADDING_STRATEGY'
  | 'SET_FLEX_GAP_STRATEGY'
  | 'SET_BORDER_RADIUS_STRATEGY'
  | 'ABSOLUTE_MOVE'
  | 'RELATIVE_MOVE'
  | 'SET_FONT_WEIGHT'
  | 'SET_FONT_SIZE'
  | 'SET_OPACITY'
  | 'FLOW_REORDER'
  | 'FLEX_REORDER'
  | 'FLEX_REPARENT_TO_ABSOLUTE'
  | 'ABSOLUTE_REPARENT'
  | 'REPARENT_TO_FLEX'
  | 'REPARENT_TO_FLOW'
  | 'DRAW_TO_INSERT_TEXT'
  | 'DRAG_TO_INSERT_ABSOLUTE'
  | 'DRAG_TO_INSERT_FLOW'
  | 'DRAG_TO_INSERT_FLEX'

export type SimpleStrategyId = MoveOrReorderStrategyId | LeafStrategyId | TestStrategyId

export type AncestorMetaStrategyID = `${SimpleStrategyId}_ANCESTOR_${number}`

export type CanvasStrategyId = SimpleStrategyId // | AncestorMetaStrategyID

export type InteractionLifecycle = 'mid-interaction' | 'end-interaction'

export interface CanvasStrategy<Id extends CanvasStrategyId> {
  id: Id // We'd need to do something to guarantee uniqueness here if using this for the commands' reason
  name: string

  // The controls to render when this strategy is applicable, regardless of if it is currently active
  controlsToRender: Array<ControlWithProps<any>>

  // For determining the relative ordering of applicable strategies during an interaction, and therefore which one to apply
  fitness: number

  // Returns the commands that inform how the model and the editor should be updated
  apply: (strategyLifecycle: InteractionLifecycle) => StrategyApplicationResult
}

export const ControlDelay = 600
