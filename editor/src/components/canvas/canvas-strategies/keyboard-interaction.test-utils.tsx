import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { canvasRectangle } from '../../../core/shared/math-utils'
import { KeyCharacter } from '../../../utils/keyboard'
import { Modifiers } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { CanvasStrategy } from './canvas-strategy-types'
import {
  createInteractionViaKeyboard,
  InteractionSession,
  StrategyState,
} from './interaction-state'

export function pressKeys(
  editorState: EditorState,
  strategy: CanvasStrategy,
  keys: Array<KeyCharacter>,
  modifiers: Modifiers,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createInteractionViaKeyboard(
      keys,
      modifiers,
      null as any, // the strategy does not use this
    ),
    metadata: null as any, // the strategy does not use this
  }

  const strategyResult = strategy.apply(
    pickCanvasStateFromEditorState(editorState),
    interactionSession,
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      accumulatedPatches: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: {
        'scene-aaa/app-entity:aaa/bbb': {
          elementPath: elementPath([
            ['scene-aaa', 'app-entity'],
            ['aaa', 'bbb'],
          ]),
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
      },
    } as StrategyState,
  )

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    [],
    strategyResult,
    'permanent',
  ).editorState

  return finalEditor
}

export const shiftModifier: Modifiers = {
  alt: false,
  cmd: false,
  ctrl: false,
  shift: true,
}

export const cmdModifier: Modifiers = {
  alt: false,
  cmd: true,
  ctrl: false,
  shift: false,
}

export const shiftCmdModifier: Modifiers = {
  alt: false,
  cmd: true,
  ctrl: false,
  shift: true,
}
