import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { CanvasPoint, canvasPoint, canvasRectangle } from '../../../core/shared/math-utils'
import { emptyModifiers } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { flexReorderStrategy } from './flex-reorder-strategy'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'

function reorderElement(
  editorState: EditorState,
  flexDirection: string,
  dragStart: CanvasPoint,
  drag: CanvasPoint,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      dragStart,
      emptyModifiers,
      null as any, // the strategy does not use this
      drag,
    ),
    metadata: null as any, // the strategy does not use this
  }

  const strategyResult = flexReorderStrategy.apply(
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
        'scene-aaa/app-entity:app-outer-div': {
          elementPath: elementPath([['scene-aaa', 'app-entity'], ['app-outer-div']]),
          globalFrame: canvasRectangle({ x: 0, y: 0, width: 375, height: 50 }),
          specialSizeMeasurements: {
            flexDirection: flexDirection,
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:app-outer-div/child-0': {
          elementPath: elementPath([
            ['scene-aaa', 'app-entity'],
            ['app-outer-div', 'child-0'],
          ]),
          globalFrame: canvasRectangle({ x: 0, y: 0, width: 50, height: 50 }),
          specialSizeMeasurements: {
            parentLayoutSystem: 'flex',
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:app-outer-div/child-1': {
          elementPath: elementPath([
            ['scene-aaa', 'app-entity'],
            ['app-outer-div', 'child-1'],
          ]),
          globalFrame: canvasRectangle({ x: 60, y: 0, width: 50, height: 50 }),
          specialSizeMeasurements: {
            parentLayoutSystem: 'flex',
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:app-outer-div/child-2': {
          elementPath: elementPath([
            ['scene-aaa', 'app-entity'],
            ['app-outer-div', 'child-2'],
          ]),
          globalFrame: canvasRectangle({ x: 120, y: 0, width: 50, height: 50 }),
          specialSizeMeasurements: {
            parentLayoutSystem: 'flex',
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

describe('Flex Reorder Strategy', () => {
  it('works with normal direction', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['app-outer-div', 'child-1'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='app-outer-div'
        style={{ display: 'flex', gap: 10 }}
      >
        <div
          data-uid='child-0'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'green',
          }}
        />
        <div
          data-uid='child-1'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'blue',
          }}
        />
        <div
          data-uid='child-2'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'purple',
          }}
        />
      </div>`),
      [targetElement],
    )

    const finalEditor = reorderElement(
      initialEditor,
      'row',
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 52, y: 0 }),
    )

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='app-outer-div'
        style={{ display: 'flex', gap: 10 }}
      >
        <div
          data-uid='child-0'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'green',
          }}
        />
        <div
          data-uid='child-2'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'purple',
          }}
        />
        <div
          data-uid='child-1'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'blue',
          }}
        />
      </div>`),
    )
  })
  it('works with reverse direction', async () => {
    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['app-outer-div', 'child-1'],
    ])

    const initialEditor = getEditorStateWithSelectedViews(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='app-outer-div'
        style={{ display: 'flex', gap: 10 }}
      >
        <div
          data-uid='child-0'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'green',
          }}
        />
        <div
          data-uid='child-1'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'blue',
          }}
        />
        <div
          data-uid='child-2'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'purple',
          }}
        />
      </div>`),
      [targetElement],
    )

    const finalEditor = reorderElement(
      initialEditor,
      'row-reverse',
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 52, y: 0 }),
    )

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='app-outer-div'
        style={{ display: 'flex', gap: 10 }}
      >
        <div
          data-uid='child-1'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'blue',
          }}
        />
        <div
          data-uid='child-0'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'green',
          }}
        />
        <div
          data-uid='child-2'
          style={{
            width: 50,
            height: 50,
            backgroundColor: 'purple',
          }}
        />
      </div>`),
    )
  })
})
