import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
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
import {
  pickCanvasStateFromEditorState,
  pickCanvasStateFromEditorStateWithMetadata,
} from './canvas-strategies'
import { defaultCustomStrategyState } from './canvas-strategy-types'
import { flexReorderStrategy } from './flex-reorder-strategy'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'

function getDefaultMetadata(flexDirection: string): ElementInstanceMetadataMap {
  return {
    'scene-aaa/app-entity:app-outer-div': {
      elementPath: elementPath([['scene-aaa', 'app-entity'], ['app-outer-div']]),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 375, height: 50 }),
      specialSizeMeasurements: {
        flexDirection: flexDirection,
        layoutSystemForChildren: 'flex',
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
        parentFlexDirection: flexDirection,
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
        parentFlexDirection: flexDirection,
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
        parentFlexDirection: flexDirection,
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
  }
}

function getMetadataWithAbsoluteChild(flexDirection: string): ElementInstanceMetadataMap {
  return {
    'scene-aaa/app-entity:app-outer-div': {
      elementPath: elementPath([['scene-aaa', 'app-entity'], ['app-outer-div']]),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 375, height: 50 }),
      specialSizeMeasurements: {
        flexDirection: flexDirection,
        layoutSystemForChildren: 'flex',
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'scene-aaa/app-entity:app-outer-div/absolute-child': {
      elementPath: elementPath([
        ['scene-aaa', 'app-entity'],
        ['app-outer-div', 'absolute-child'],
      ]),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 50, height: 50 }),
      specialSizeMeasurements: {
        parentLayoutSystem: 'flex',
        parentFlexDirection: flexDirection,
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
        parentFlexDirection: flexDirection,
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
        parentFlexDirection: flexDirection,
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
        parentFlexDirection: flexDirection,
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
  }
}

function getReverseMetadata(flexDirection: string): ElementInstanceMetadataMap {
  return {
    'scene-aaa/app-entity:app-outer-div': {
      elementPath: elementPath([['scene-aaa', 'app-entity'], ['app-outer-div']]),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 375, height: 50 }),
      specialSizeMeasurements: {
        flexDirection: flexDirection,
        layoutSystemForChildren: 'flex',
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'scene-aaa/app-entity:app-outer-div/child-0': {
      elementPath: elementPath([
        ['scene-aaa', 'app-entity'],
        ['app-outer-div', 'child-0'],
      ]),
      globalFrame: canvasRectangle({ x: 120, y: 0, width: 50, height: 50 }),
      specialSizeMeasurements: {
        parentLayoutSystem: 'flex',
        parentFlexDirection: flexDirection,
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
        parentFlexDirection: flexDirection,
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
    'scene-aaa/app-entity:app-outer-div/child-2': {
      elementPath: elementPath([
        ['scene-aaa', 'app-entity'],
        ['app-outer-div', 'child-2'],
      ]),
      globalFrame: canvasRectangle({ x: 0, y: 0, width: 50, height: 50 }),
      specialSizeMeasurements: {
        parentLayoutSystem: 'flex',
        parentFlexDirection: flexDirection,
      } as SpecialSizeMeasurements,
    } as ElementInstanceMetadata,
  }
}

function reorderElement(
  editorState: EditorState,
  dragStart: CanvasPoint,
  drag: CanvasPoint,
  metadata: ElementInstanceMetadataMap,
  newIndex?: number,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      dragStart,
      emptyModifiers,
      null as any, // the strategy does not use this
      drag,
    ),
    latestMetadata: null as any, // the strategy does not use this
    latestAllElementProps: null as any, // the strategy does not use this
    startingTargetParentsToFilterOut: null,
  }

  const strategyResult = flexReorderStrategy.apply(
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
      customStrategyState: defaultCustomStrategyState(),
    } as StrategyState,
    'end-interaction',
  )

  expect(strategyResult.customStatePatch?.lastReorderIdx).toEqual(newIndex)

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

describe('Flex Reorder Strategy', () => {
  it('does not activate when drag threshold is not reached', async () => {
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
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 1, y: 1 }),
      getDefaultMetadata('row'),
    )

    expect(finalEditor).toEqual(initialEditor)
  })
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
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 52, y: 0 }),
      getDefaultMetadata('row'),
      2,
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
  it('excludes absolute siblings', async () => {
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
          data-uid='absolute-child'
          style={{
            position: 'absolute',
            top: 100,
            left: 50,
            width: 50,
            height: 50,
            backgroundColor: 'yellow',
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
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 52, y: 0 }),
      getMetadataWithAbsoluteChild('row'),
      3,
    )

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='app-outer-div'
        style={{ display: 'flex', gap: 10 }}
      >
        <div
          data-uid='absolute-child'
          style={{
            position: 'absolute',
            top: 100,
            left: 50,
            width: 50,
            height: 50,
            backgroundColor: 'yellow',
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
        style={{ display: 'flex', gap: 10, flexDirection: 'row-reverse' }}
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
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 52, y: 0 }),
      getReverseMetadata('row-reverse'),
      0,
    )

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='app-outer-div'
        style={{
          display: 'flex',
          gap: 10,
          flexDirection: 'row-reverse',
        }}
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
