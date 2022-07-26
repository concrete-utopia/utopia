import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { canvasRectangle } from '../../../core/shared/math-utils'
import { KeyCharacter } from '../../../utils/keyboard'
import { Modifiers } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { CanvasStrategy, defaultCustomStrategyState } from './canvas-strategy-types'
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
  const metadata: ElementInstanceMetadataMap = {
    'scene-aaa/app-entity:aaa/bbb': {
      elementPath: elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      specialSizeMeasurements: {
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
  }
  const interactionSession: InteractionSession = {
    ...createInteractionViaKeyboard(
      keys,
      modifiers,
      null as any, // the strategy does not use this
    ),
    metadata: metadata,
    allElementProps: null as any,
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
      startingMetadata: metadata,
      startingAllElementProps: { 'scene-aaa/app-entity:aaa/bbb': {} },
      customStrategyState: defaultCustomStrategyState(),
    } as StrategyState,
    'mid-interaction',
  )

  expect(strategyResult.customState).toBeNull()

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    [],
    strategyResult.commands,
    'permanent',
  ).editorState

  return finalEditor
}
