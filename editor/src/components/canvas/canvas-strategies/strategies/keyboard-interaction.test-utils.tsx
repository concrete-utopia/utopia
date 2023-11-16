import { createBuiltInDependenciesList } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { right } from '../../../../core/shared/either'
import { elementPath } from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import { jsxElement, jsxElementName } from '../../../../core/shared/element-template'
import { canvasRectangle } from '../../../../core/shared/math-utils'
import type { KeyCharacter } from '../../../../utils/keyboard'
import type { Modifiers } from '../../../../utils/modifiers'
import type { EditorState } from '../../../editor/store/editor-state'
import { foldAndApplyCommands } from '../../commands/commands'
import { pickCanvasStateFromEditorStateWithMetadata } from '../canvas-strategies'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { defaultCustomStrategyState } from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { createInteractionViaKeyboard } from '../interaction-state'

export function pressKeys(
  editorState: EditorState,
  strategyFactoryFunction: (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    customStrategyState: CustomStrategyState,
  ) => CanvasStrategy | null,
  keys: Array<KeyCharacter>,
  modifiers: Modifiers,
): EditorState {
  const metadata: ElementInstanceMetadataMap = {
    'scene-aaa/app-entity:aaa/bbb': {
      elementPath: elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
      element: right(jsxElement(jsxElementName('View', []), 'bbb', [], [])),
      specialSizeMeasurements: {
        position: 'absolute',
        immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      } as SpecialSizeMeasurements,
      globalFrame: { x: 50, y: 50, width: 250, height: 300 },
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
    latestElementPathTree: null as any,
    latestVariablesInScope: null as any,
  }

  const strategy = strategyFactoryFunction(
    pickCanvasStateFromEditorStateWithMetadata(
      editorState,
      createBuiltInDependenciesList(null),
      metadata,
    ),
    interactionSession,
    defaultCustomStrategyState(),
  )

  if (strategy == null) {
    throw new Error(`keyboard-interaction.test-utils error: strategyFactoryFunction returned null`)
  }

  const strategyResult = strategy.apply('end-interaction')

  expect(strategyResult.customStatePatch).toEqual({})
  expect(strategyResult.status).toEqual('success')

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    strategyResult.commands,
    'end-interaction',
  ).editorState

  return finalEditor
}
