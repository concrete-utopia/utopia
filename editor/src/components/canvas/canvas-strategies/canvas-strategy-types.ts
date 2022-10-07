import React from 'react'
import { AllElementProps } from '../../../components/editor/store/editor-state'
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
  aspectRatioLock: number | null
}

export type CustomStrategyStatePatch = Partial<CustomStrategyState>

export function defaultCustomStrategyState(): CustomStrategyState {
  return {
    escapeHatchActivated: false,
    lastReorderIdx: null,
    duplicatedElementNewUids: {},
    aspectRatioLock: null,
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

export type CanvasStrategyId = string

export type InteractionLifecycle = 'mid-interaction' | 'end-interaction'

export interface CanvasStrategy {
  id: CanvasStrategyId // We'd need to do something to guarantee uniqueness here if using this for the commands' reason

  name: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    customStrategyState: CustomStrategyState,
  ) => string

  // Determines if we should show the controls that this strategy renders
  isApplicable: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
  ) => boolean

  // The controls to render when this strategy is applicable, regardless of if it is currently active
  controlsToRender: Array<ControlWithProps<unknown>>

  // As before, for determining the relative ordering of applicable strategies during an interaction, and therefore which one to apply
  fitness: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    customStrategyState: CustomStrategyState,
  ) => number

  // Returns the commands that inform how the model and the editor should be updated
  apply: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    customStrategyState: CustomStrategyState,
    strategyLifecycle: InteractionLifecycle,
  ) => StrategyApplicationResult
}

export const ControlDelay = 600
