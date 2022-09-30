import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
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
import {
  pickCanvasStateFromEditorState,
  pickCanvasStateFromEditorStateWithMetadata,
} from './canvas-strategies'
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
    latestMetadata: metadata,
    latestAllElementProps: null as any,
    startingTargetParentsToFilterOut: null,
  }

  const strategyResult = strategy.apply(
    pickCanvasStateFromEditorStateWithMetadata(
      editorState,
      createBuiltInDependenciesList(null),
      metadata,
    ),
    interactionSession,
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      accumulatedPatches: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      status: 'success',
      startingMetadata: metadata,
      startingAllElementProps: { 'scene-aaa/app-entity:aaa/bbb': {} },
      customStrategyState: defaultCustomStrategyState(),
    } as StrategyState,
    'end-interaction',
  )

  expect(strategyResult.customStatePatch).toEqual({})
  expect(strategyResult.status).toEqual('success')

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    [],
    strategyResult.commands,
    'end-interaction',
  ).editorState

  return finalEditor
}
