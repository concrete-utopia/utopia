import { createBuiltInDependenciesList } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { elementPath } from '../../../../core/shared/element-path'
import { CanvasPoint, canvasPoint } from '../../../../core/shared/math-utils'
import { emptyModifiers } from '../../../../utils/modifiers'
import { EditorState } from '../../../editor/store/editor-state'
import { foldAndApplyCommands } from '../../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  testPrintCodeFromEditorState,
} from '../../ui-jsx.test-utils'
import { pickCanvasStateFromEditorStateWithMetadata } from '../canvas-strategies'
import { defaultCustomStrategyState } from '../canvas-strategy-types'
import { flexReorderStrategy } from './flex-reorder-strategy'
import { boundingArea, InteractionSession } from '../interaction-state'
import { createMouseInteractionForTests } from '../interaction-state.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'

function reorderElement(
  editorState: EditorState,
  dragStart: CanvasPoint,
  drag: CanvasPoint,
  newIndex?: number,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(dragStart, emptyModifiers, boundingArea(), drag),
    latestMetadata: null as any, // the strategy does not use this
    latestAllElementProps: null as any, // the strategy does not use this
    startingTargetParentsToFilterOut: null,
  }

  const strategyResult = flexReorderStrategy(
    pickCanvasStateFromEditorStateWithMetadata(
      editorState,
      createBuiltInDependenciesList(null),
      editorState.jsxMetadata,
    ),
    interactionSession,
    defaultCustomStrategyState(),
  )?.apply('end-interaction')

  expect(strategyResult).toBeDefined()
  expect(strategyResult!.customStatePatch?.lastReorderIdx).toEqual(newIndex)

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    strategyResult!.commands,
    'end-interaction',
  ).editorState

  return finalEditor
}

const TestProject = (direction: string) => `
<div
  data-uid='app-outer-div'
  style={{ display: 'flex', gap: 10, flexDirection: '${direction}' }}
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
</div>`

describe('Flex Reorder Strategy', () => {
  it('does not activate when drag threshold is not reached', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject('row')),
      'await-first-dom-report',
    )

    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['app-outer-div', 'child-1'],
    ])

    await renderResult.dispatch([selectComponents([targetElement], false)], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const finalEditor = reorderElement(
      renderResult.getEditorState().editor,
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 1, y: 1 }),
    )

    expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
      makeTestProjectCodeWithSnippet(TestProject('row')),
    )
  })
  it('works with normal direction', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject('row')),
      'await-first-dom-report',
    )

    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['app-outer-div', 'child-1'],
    ])

    await renderResult.dispatch([selectComponents([targetElement], false)], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const finalEditor = reorderElement(
      renderResult.getEditorState().editor,
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 52, y: 0 }),
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
    const renderResult = await renderTestEditorWithCode(
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
      'await-first-dom-report',
    )

    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['app-outer-div', 'child-1'],
    ])

    await renderResult.dispatch([selectComponents([targetElement], false)], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const finalEditor = reorderElement(
      renderResult.getEditorState().editor,
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 52, y: 0 }),
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
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject('row-reverse')),
      'await-first-dom-report',
    )

    const targetElement = elementPath([
      ['scene-aaa', 'app-entity'],
      ['app-outer-div', 'child-1'],
    ])

    await renderResult.dispatch([selectComponents([targetElement], false)], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const finalEditor = reorderElement(
      renderResult.getEditorState().editor,
      canvasPoint({ x: 89, y: 27 }),
      canvasPoint({ x: 52, y: 0 }),
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
