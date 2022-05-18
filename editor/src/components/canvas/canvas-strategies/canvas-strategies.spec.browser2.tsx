import { elementPath } from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { canvasPoint } from '../../../core/shared/math-utils'
import { emptyModifiers } from '../../../utils/modifiers'
import {
  findCanvasStrategy,
  pickCanvasStateFromEditorState,
  RegisteredCanvasStrategies,
} from './canvas-strategies'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'
import { act } from '@testing-library/react'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'

const baseStrategyState = (metadata: ElementInstanceMetadataMap) =>
  ({
    currentStrategy: null as any, // the strategy does not use this
    currentStrategyFitness: null as any, // the strategy does not use this
    currentStrategyCommands: null as any, // the strategy does not use this
    accumulatedPatches: null as any, // the strategy does not use this
    commandDescriptions: null as any, // the strategy does not use this
    sortedApplicableStrategies: null as any, // the strategy does not use this
    startingMetadata: metadata,
    customStrategyState: {
      escapeHatchActivated: false,
      lastReorderIdx: null,
    },
  } as StrategyState)

describe('Strategy Fitness', () => {
  it('fits Escape Hatch Strategy when dragging a flow element', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...(props.style || {}) }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', width: 250, height: 300 }}
          data-uid='bbb'
        />
      </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    const interactionSession: InteractionSession = {
      ...createMouseInteractionForTests(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: 15, y: 15 }),
      ),
      metadata: renderResult.getEditorState().editor.jsxMetadata,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(renderResult.getEditorState().editor.jsxMetadata),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ESCAPE_HATCH_STRATEGY')
  })
  it('fits Escape Hatch Strategy when dragging a flex element without siblings', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...(props.style || {}), display: 'flex' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', width: 250, height: 300 }}
          data-uid='bbb'
        />
      </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    const interactionSession: InteractionSession = {
      ...createMouseInteractionForTests(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: 15, y: 15 }),
      ),
      metadata: renderResult.getEditorState().editor.jsxMetadata,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(renderResult.getEditorState().editor.jsxMetadata),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ESCAPE_HATCH_STRATEGY')
  })
  it('fits Flex Reorder Strategy when dragging a flex element with siblings', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...(props.style || {}), display: 'flex' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', width: 100, height: 200 }}
          data-uid='bbb'
        />
        <div
          style={{ backgroundColor: '#0091FFAA', width: 100, height: 50 }}
          data-uid='ccc'
        />
      </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    const interactionSession: InteractionSession = {
      ...createMouseInteractionForTests(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: 15, y: 15 }),
      ),
      metadata: renderResult.getEditorState().editor.jsxMetadata,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(renderResult.getEditorState().editor.jsxMetadata),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('FLEX_REORDER')
  })
  it('fits Escape Hatch Strategy when dragging a flex element with siblings the cursor is outside of the parent', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...(props.style || {}), display: 'flex' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', width: 100, height: 200 }}
          data-uid='bbb'
        />
        <div
          style={{ backgroundColor: '#0091FFAA', width: 100, height: 50 }}
          data-uid='ccc'
        />
      </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    const interactionSession: InteractionSession = {
      ...createMouseInteractionForTests(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: -15, y: -15 }),
      ),
      metadata: renderResult.getEditorState().editor.jsxMetadata,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(renderResult.getEditorState().editor.jsxMetadata),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ESCAPE_HATCH_STRATEGY')
  })
})
