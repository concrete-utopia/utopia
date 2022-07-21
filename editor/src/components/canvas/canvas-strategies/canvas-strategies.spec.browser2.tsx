import { elementPath } from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { canvasPoint, Rectangle, SimpleRectangle } from '../../../core/shared/math-utils'
import { cmdModifier, emptyModifiers } from '../../../utils/modifiers'
import {
  findCanvasStrategy,
  pickCanvasStateFromEditorState,
  RegisteredCanvasStrategies,
} from './canvas-strategies'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'
import { act } from '@testing-library/react'
import {
  EditorRenderResult,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'
import CanvasActions from '../canvas-actions'
import { AllElementProps } from '../../editor/store/editor-state'

const baseStrategyState = (
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
) =>
  ({
    currentStrategy: null as any, // the strategy does not use this
    currentStrategyFitness: null as any, // the strategy does not use this
    currentStrategyCommands: null as any, // the strategy does not use this
    accumulatedPatches: null as any, // the strategy does not use this
    commandDescriptions: null as any, // the strategy does not use this
    sortedApplicableStrategies: null as any, // the strategy does not use this
    startingMetadata: metadata,
    startingAllElementProps: allElementProps,
    customStrategyState: {
      escapeHatchActivated: false,
      lastReorderIdx: null,
    },
  } as StrategyState)

interface StyleRectangle {
  left: string
  top: string
  width: string
  height: string
}

async function getGuidelineDimensions(
  renderResult: EditorRenderResult,
  testId: string,
): Promise<StyleRectangle> {
  const guideline = await renderResult.renderedDOM.findByTestId(testId)
  return {
    left: guideline.style.left,
    top: guideline.style.top,
    width: guideline.style.width,
    height: guideline.style.height,
  }
}

async function getGuidelineRenderResult(scale: number) {
  const targetElement = elementPath([
    ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
    ['div-parent', 'first-div'],
  ])

  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(`
      <div
        data-uid='div-parent'
        data-testid='div-parent'>
        <div
          data-uid='first-div'
          data-testid='first-div'
          style={{
            position: 'absolute',
            left: 50.5,
            top: 146.5,
            width: 50,
            height: 50,
            backgroundColor: 'red',
          }}
        />
        <div
          data-uid='second-div'
          data-testid='second-div'
          style={{
            position: 'absolute',
            left: 110.5,
            top: 206.5,
            width: 7,
            height: 9,
            backgroundColor: 'blue',
          }}
        />
      </div>
      `),
    'await-first-dom-report',
  )

  await act(async () => {
    const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
    await renderResult.dispatch(
      [selectComponents([targetElement], false), CanvasActions.zoom(scale)],
      false,
    )
    await dispatchDone
  })

  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      canvasPoint({ x: 60, y: 150 }),
      emptyModifiers,
      { type: 'BOUNDING_AREA', target: targetElement },
      canvasPoint({ x: 10, y: 10 }),
    ),
    metadata: renderResult.getEditorState().editor.jsxMetadata,
    allElementProps: renderResult.getEditorState().editor.allElementProps,
  }

  await act(async () => {
    const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
    await renderResult.dispatch([CanvasActions.createInteractionSession(interactionSession)], false)
    await dispatchDone
  })
  return renderResult
}

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
      allElementProps: renderResult.getEditorState().editor.allElementProps,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.allElementProps,
      ),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ESCAPE_HATCH_STRATEGY')
  })
  xit('fits Escape Hatch Strategy when dragging a flex element without siblings', async () => {
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
      allElementProps: renderResult.getEditorState().editor.allElementProps,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.allElementProps,
      ),
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
      allElementProps: renderResult.getEditorState().editor.allElementProps,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.allElementProps,
      ),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('FLEX_REORDER')
  })
  it('fits Flex Reparent to Absolute Strategy when cmd-dragging a flex element with siblings the cursor is outside of the parent', async () => {
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
        cmdModifier,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: -15, y: -15 }),
      ),
      metadata: renderResult.getEditorState().editor.jsxMetadata,
      allElementProps: renderResult.getEditorState().editor.allElementProps,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.allElementProps,
      ),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('FLEX_REPARENT_TO_ABSOLUTE')
  })
  it('fits Absolute Resize Strategy when resizing an absolute element without modifiers', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...(props.style || {}), display: 'flex' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', top: 10, left: 10, width: 100, height: 120 }}
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
        { type: 'RESIZE_HANDLE', edgePosition: { x: 1, y: 0.5 } },
        canvasPoint({ x: -15, y: -15 }),
      ),
      metadata: renderResult.getEditorState().editor.jsxMetadata,
      allElementProps: renderResult.getEditorState().editor.allElementProps,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.allElementProps,
      ),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ABSOLUTE_RESIZE_BOUNDING_BOX')
  })
  it('fits Absolute Resize Strategy when resizing an absolute element with cmd pressed', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...(props.style || {}), display: 'flex' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', top: 10, left: 10, width: 100, height: 120 }}
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
        cmdModifier,
        { type: 'RESIZE_HANDLE', edgePosition: { x: 1, y: 0.5 } },
        canvasPoint({ x: -15, y: -15 }),
      ),
      metadata: renderResult.getEditorState().editor.jsxMetadata,
      allElementProps: renderResult.getEditorState().editor.allElementProps,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(renderResult.getEditorState().editor),
      interactionSession,
      baseStrategyState(
        renderResult.getEditorState().editor.jsxMetadata,
        renderResult.getEditorState().editor.allElementProps,
      ),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ABSOLUTE_RESIZE_BOUNDING_BOX')
  })
})

describe('Snapping guidelines for absolutely moved element', () => {
  it('should line up appropriately with a scale of 1', async () => {
    const renderResult = await getGuidelineRenderResult(1)

    expect(await getGuidelineDimensions(renderResult, 'guideline-0')).toEqual({
      left: '110px',
      top: '156px',
      width: '0px',
      height: '59px',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-1')).toEqual({
      left: '60px',
      top: '206px',
      width: '57px',
      height: '0px',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-2')).toEqual({
      left: '',
      top: '',
      width: '',
      height: '',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-3')).toEqual({
      left: '',
      top: '',
      width: '',
      height: '',
    })

    expect(renderResult.getEditorState().editor.canvas.controls.snappingGuidelines).toEqual([
      {
        activateSnap: true,
        guideline: {
          type: 'XAxisGuideline',
          x: 110.5,
          yBottom: 215.5,
          yTop: 156.5,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
      },
      {
        activateSnap: true,
        guideline: {
          type: 'YAxisGuideline',
          xLeft: 60.5,
          xRight: 117.5,
          y: 206.5,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
      },
    ])
  })

  it('should line up appropriately with a scale of 4', async () => {
    const renderResult = await getGuidelineRenderResult(4)

    expect(await getGuidelineDimensions(renderResult, 'guideline-0')).toEqual({
      left: '110.375px',
      top: '156.375px',
      width: '0px',
      height: '59px',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-1')).toEqual({
      left: '60.375px',
      top: '206.375px',
      width: '57px',
      height: '0px',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-2')).toEqual({
      left: '',
      top: '',
      width: '',
      height: '',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-3')).toEqual({
      left: '',
      top: '',
      width: '',
      height: '',
    })

    expect(renderResult.getEditorState().editor.canvas.controls.snappingGuidelines).toEqual([
      {
        activateSnap: true,
        guideline: {
          type: 'XAxisGuideline',
          x: 110.5,
          yBottom: 215.5,
          yTop: 156.5,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
      },
      {
        activateSnap: true,
        guideline: {
          type: 'YAxisGuideline',
          xLeft: 60.5,
          xRight: 117.5,
          y: 206.5,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
      },
    ])
  })

  it('should line up appropriately with a scale of 0.25', async () => {
    const renderResult = await getGuidelineRenderResult(0.25)

    expect(await getGuidelineDimensions(renderResult, 'guideline-0')).toEqual({
      left: '108px',
      top: '154px',
      width: '0px',
      height: '60px',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-1')).toEqual({
      left: '58px',
      top: '204px',
      width: '58px',
      height: '0px',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-2')).toEqual({
      left: '',
      top: '',
      width: '',
      height: '',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-3')).toEqual({
      left: '',
      top: '',
      width: '',
      height: '',
    })

    expect(renderResult.getEditorState().editor.canvas.controls.snappingGuidelines).toEqual([
      {
        activateSnap: true,
        guideline: {
          type: 'XAxisGuideline',
          x: 110,
          yBottom: 216,
          yTop: 156,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
      },
      {
        activateSnap: true,
        guideline: {
          type: 'YAxisGuideline',
          xLeft: 60,
          xRight: 118,
          y: 206,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
      },
    ])
  })
})
