import { act } from '@testing-library/react'
import { elementPath } from '../../../core/shared/element-path'
import type { WindowPoint } from '../../../core/shared/math-utils'
import { canvasPoint, offsetPoint, windowPoint } from '../../../core/shared/math-utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import type { Modifiers } from '../../../utils/modifiers'
import { cmdModifier, emptyModifiers } from '../../../utils/modifiers'
import { selectComponents } from '../../editor/actions/action-creators'
import CanvasActions from '../canvas-actions'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { mouseDownAtPoint, mouseMoveToPoint } from '../event-helpers.test-utils'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import {
  findCanvasStrategy,
  pickCanvasStateFromEditorState,
  RegisteredCanvasStrategies,
} from './canvas-strategies'
import { defaultCustomStrategyState } from './canvas-strategy-types'
import type { InteractionSession } from './interaction-state'
import { boundingArea } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'
import { NonResizableControlTestId } from '../controls/select-mode/non-resizable-control'

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
        style={{
          width: 300,
          height: 300,
        }}
        data-uid='div-parent'
        data-testid='div-parent'>
        <div
          data-uid='first-div'
          data-testid='first-div'
          style={{
            position: 'absolute',
            left: 50,
            top: 146,
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
            left: 110,
            top: 206,
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
      boundingArea(),
      canvasPoint({ x: 10, y: 10 }),
    ),
    latestMetadata: renderResult.getEditorState().editor.jsxMetadata,
    latestAllElementProps: renderResult.getEditorState().editor.allElementProps,
    latestElementPathTree: renderResult.getEditorState().editor.elementPathTree,
    latestVariablesInScope: renderResult.getEditorState().editor.variablesInScope,
  }

  await act(async () => {
    const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
    await renderResult.dispatch([CanvasActions.createInteractionSession(interactionSession)], false)
    await dispatchDone
  })
  return renderResult
}

async function startElementDragNoMouseUp(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
): Promise<void> {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControl = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 20, y: targetElementBounds.y + 20 })
  const endPoint = offsetPoint(startPoint, dragDelta)
  await mouseDownAtPoint(canvasControl, startPoint, { modifiers: modifiers })
  await mouseMoveToPoint(canvasControl, endPoint, {
    modifiers: modifiers,
    eventOptions: { buttons: 1 },
  })
}

describe('Strategy Fitness', () => {
  xit('fits Escape Hatch Strategy when dragging a flow element', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...(props.style || {}) }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300 }}
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
        boundingArea(),
        canvasPoint({ x: 15, y: 15 }),
      ),
      latestMetadata: renderResult.getEditorState().editor.jsxMetadata,
      latestAllElementProps: renderResult.getEditorState().editor.allElementProps,
      latestElementPathTree: renderResult.getEditorState().editor.elementPathTree,
      latestVariablesInScope: renderResult.getEditorState().editor.variablesInScope,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().builtInDependencies,
      ),
      interactionSession,
      defaultCustomStrategyState(),
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
          style={{ backgroundColor: '#aaaaaa33', width: 250, height: 300 }}
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
        boundingArea(),
        canvasPoint({ x: 15, y: 15 }),
      ),
      latestMetadata: renderResult.getEditorState().editor.jsxMetadata,
      latestAllElementProps: renderResult.getEditorState().editor.allElementProps,
      latestElementPathTree: renderResult.getEditorState().editor.elementPathTree,
      latestVariablesInScope: renderResult.getEditorState().editor.variablesInScope,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().builtInDependencies,
      ),
      interactionSession,
      defaultCustomStrategyState(),
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
          style={{ backgroundColor: '#aaaaaa33', width: 100, height: 200 }}
          data-uid='bbb'
          data-testid='bbb'
        />
        <div
          style={{ backgroundColor: '#aaaaaa33', width: 100, height: 50 }}
          data-uid='ccc'
        />
      </div>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch([selectComponents([targetElement], false)], false)

    // we don't want to mouse up to avoid clearInteractionSession
    await startElementDragNoMouseUp(
      renderResult,
      'bbb',
      windowPoint({ x: 15, y: 15 }),
      emptyModifiers,
    )

    const canvasStrategy = renderResult.getEditorState().strategyState.currentStrategy
    expect(canvasStrategy).toEqual('FLEX_REORDER')
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
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', top: 10, left: 10, width: 100, height: 120 }}
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
      latestMetadata: renderResult.getEditorState().editor.jsxMetadata,
      latestAllElementProps: renderResult.getEditorState().editor.allElementProps,
      latestElementPathTree: renderResult.getEditorState().editor.elementPathTree,
      latestVariablesInScope: renderResult.getEditorState().editor.variablesInScope,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().builtInDependencies,
      ),
      interactionSession,
      defaultCustomStrategyState(),
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
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', top: 10, left: 10, width: 100, height: 120 }}
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
      latestMetadata: renderResult.getEditorState().editor.jsxMetadata,
      latestAllElementProps: renderResult.getEditorState().editor.allElementProps,
      latestElementPathTree: renderResult.getEditorState().editor.elementPathTree,
      latestVariablesInScope: renderResult.getEditorState().editor.variablesInScope,
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(
        renderResult.getEditorState().editor,
        renderResult.getEditorState().builtInDependencies,
      ),
      interactionSession,
      defaultCustomStrategyState(),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ABSOLUTE_RESIZE_BOUNDING_BOX')
  })
})

describe('Snapping guidelines for absolutely moved element', () => {
  it('should line up appropriately with a scale of 1', async () => {
    const renderResult = await getGuidelineRenderResult(1)

    expect(await getGuidelineDimensions(renderResult, 'guideline-0')).toEqual({
      left: '109.5px',
      top: '155.5px',
      width: '0px',
      height: '59px',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-1')).toEqual({
      left: '59.5px',
      top: '205.5px',
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
        guideline: {
          type: 'XAxisGuideline',
          x: 110,
          yBottom: 215,
          yTop: 156,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
        pointsOfRelevance: [
          {
            x: 110,
            y: 206,
          },
          {
            x: 110,
            y: 215,
          },
        ],
      },
      {
        guideline: {
          type: 'YAxisGuideline',
          xLeft: 60,
          xRight: 117,
          y: 206,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
        pointsOfRelevance: [
          {
            x: 110,
            y: 206,
          },
          {
            x: 117,
            y: 206,
          },
        ],
      },
    ])
  })

  it('should line up appropriately with a scale of 4', async () => {
    const renderResult = await getGuidelineRenderResult(4)

    expect(await getGuidelineDimensions(renderResult, 'guideline-0')).toEqual({
      left: '109.875px',
      top: '155.875px',
      width: '0px',
      height: '59px',
    })
    expect(await getGuidelineDimensions(renderResult, 'guideline-1')).toEqual({
      left: '59.875px',
      top: '205.875px',
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
        guideline: {
          type: 'XAxisGuideline',
          x: 110,
          yBottom: 215,
          yTop: 156,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
        pointsOfRelevance: [
          {
            x: 110,
            y: 206,
          },
          {
            x: 110,
            y: 215,
          },
        ],
      },
      {
        guideline: {
          type: 'YAxisGuideline',
          xLeft: 60,
          xRight: 117,
          y: 206,
        },
        snappingVector: {
          x: 0,
          y: 0,
        },
        pointsOfRelevance: [
          {
            x: 110,
            y: 206,
          },
          {
            x: 117,
            y: 206,
          },
        ],
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
        pointsOfRelevance: [
          {
            x: 110,
            y: 206,
          },
          {
            x: 110,
            y: 216,
          },
        ],
      },
      {
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
        pointsOfRelevance: [
          {
            x: 110,
            y: 206,
          },
          {
            x: 118,
            y: 206,
          },
        ],
      },
    ])
  })
})

const projectWithNonResizableElement = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const ChildrenHider = (props) => {
  return <div data-uid='33d' style={{ ...props.style }} />
}

export const Button = () => {
  return (
    <div
      data-uid='buttondiv'
      data-testid='buttondiv'
      data-label='buttondiv'
      style={{
        width: 100,
        height: 30,
        backgroundColor: 'pink',
      }}
    >
      BUTTON
    </div>
  )
}

const unmoveableColour = 'orange'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='scene'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 500,
        }}
        data-uid='sceneroot'
      >
        <ChildrenHider
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 150,
            top: 200,
            height: 50,
            width: 50,
          }}
          data-uid='d75'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 111,
            width: 140,
            position: 'absolute',
            left: 197,
            top: 376,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          <Button
            data-uid='button'
            data-testid='button'
            data-label='button'
          />
          <Button
            data-uid='other-button'
            data-testid='other-button'
            data-label='other-button'
          />
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'grey',
          position: 'absolute',
          display: 'flex',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'relative',
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'relative',
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
      </div>
    </Scene>
  </Storyboard>
)
`

describe('special case controls', () => {
  it('non-resizable corner controls show for an element that is not resizable', async () => {
    const targetElement = elementPath([['storyboard', 'scene', 'sceneroot', 'dragme', 'button']])

    const renderResult = await renderTestEditorWithCode(
      projectWithNonResizableElement,
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    async function checkResizableControl(
      controlTestId: string,
      expectedLeft: string,
      expectedTop: string,
    ): Promise<void> {
      const nonResizableControl = await renderResult.renderedDOM.findByTestId(controlTestId)
      const nonResizableControlParent = forceNotNull(
        'Should be able to find the parent.',
        nonResizableControl.parentElement,
      )
      expect(nonResizableControlParent.style.left).toEqual(expectedLeft)
      expect(nonResizableControlParent.style.top).toEqual(expectedTop)
    }

    await checkResizableControl('non-resizable-0-0', '', '')
    await checkResizableControl('non-resizable-1-0', '100px', '')
    await checkResizableControl('non-resizable-0-1', '', '30px')
    await checkResizableControl('non-resizable-1-1', '100px', '30px')
  })

  it('no non-resizable corner controls show for an element that is resizable', async () => {
    const targetElement = elementPath([['storyboard', 'scene', 'sceneroot', 'dragme']])

    const renderResult = await renderTestEditorWithCode(
      projectWithNonResizableElement,
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    const nonResizableControl = renderResult.renderedDOM.queryByTestId(NonResizableControlTestId)
    expect(nonResizableControl).toEqual(null)
  })
})
