import { cmdModifier, emptyModifiers } from '../../../../utils/modifiers'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
  pressKey,
} from '../../event-helpers.test-utils'
import { canvasPoint, windowPoint } from '../../../../core/shared/math-utils'
import { isFeatureEnabled, setFeatureEnabled } from '../../../../utils/feature-switches'

const TestProject = (direction: string) => `
<div
  data-uid='aaa'
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
    data-testid='child-1'
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

const TestProjectWithFragment = (direction: string) => `
<div
  data-uid='aaa'
  style={{ display: 'flex', gap: 10, flexDirection: '${direction}' }}
>
  <div
    data-uid='child-0'
      data-testid='child-0'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'green',
    }}
  />
  <>
    <div
      data-uid='child-1'
      data-testid='child-1'
      style={{
        width: 50,
        height: 50,
        backgroundColor: 'blue',
      }}
    />
    <div
      data-uid='child-2'
      data-testid='child-2'
      style={{
        width: 50,
        height: 50,
        backgroundColor: 'purple',
      }}
    />
  </>
  <div
    data-uid='child-3'
    data-testid='child-3'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'yellow',
    }}
  />
</div>`

const TestProjectAbsoluteSibling = `
<div
  data-uid='aaa'
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
    data-testid='child-1'
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
</div>
`

const TestProjectWithFragmentAbsoluteSibling = `
<div
  data-uid='aaa'
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
  <>
    <div
      data-uid='child-1'
      data-testid='child-1'
      style={{
        width: 50,
        height: 50,
        backgroundColor: 'blue',
      }}
    />
    <div
      data-uid='child-2'
      data-testid='child-2'
      style={{
        width: 50,
        height: 50,
        backgroundColor: 'purple',
      }}
    />
  </>
  <div
    data-uid='child-3'
      data-testid='child-3'
    style={{
      width: 50,
      height: 50,
      backgroundColor: 'purple',
    }}
  />
</div>
`
describe('Flex Reorder Strategy', () => {
  it('does not activate when drag threshold is not reached', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject('row')),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: emptyModifiers })
    await mouseDragFromPointWithDelta(
      canvasControlsLayer,
      startPoint,
      canvasPoint({ x: 1, y: 1 }),
      {
        modifiers: emptyModifiers,
      },
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(TestProject('row')),
    )
  })
  it('works with normal direction', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject('row')),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(
      canvasControlsLayer,
      startPoint,
      canvasPoint({ x: 62, y: 0 }),
      {
        modifiers: emptyModifiers,
        midDragCallback: async () => {
          expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
            'FLEX_REORDER',
          )
          expect(
            renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
          ).toEqual(2)
        },
      },
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{ display: 'flex', gap: 10, flexDirection: 'row' }}
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
          data-testid='child-1'
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
      makeTestProjectCodeWithSnippet(TestProjectAbsoluteSibling),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(
      canvasControlsLayer,
      startPoint,
      canvasPoint({ x: 62, y: 0 }),
      {
        modifiers: emptyModifiers,
        midDragCallback: async () => {
          expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
            'FLEX_REORDER',
          )
          expect(
            renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
          ).toEqual(3)
        },
      },
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
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
          data-testid='child-1'
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

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(
      canvasControlsLayer,
      startPoint,
      canvasPoint({ x: 62, y: 0 }),
      {
        modifiers: emptyModifiers,
        midDragCallback: async () => {
          expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
            'FLEX_REORDER',
          )
          expect(
            renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
          ).toEqual(0)
        },
      },
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          display: 'flex',
          gap: 10,
          flexDirection: 'row-reverse',
        }}
      >
        <div
          data-uid='child-1'
          data-testid='child-1'
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

  xdescribe('projects with fragments, with fragments support enabled', () => {
    let originalFragmentsSupported: boolean = false
    before(() => {
      originalFragmentsSupported = isFeatureEnabled('Fragment support')
      setFeatureEnabled('Fragment support', true)
    })
    after(() => {
      setFeatureEnabled('Fragment support', originalFragmentsSupported)
    })

    it('does not activate when drag threshold is not reached', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(TestProjectWithFragment('row')),
        'await-first-dom-report',
      )

      const targetElement = renderResult.renderedDOM.getByTestId('child-2')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
      await pressKey('Escape')
      await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: emptyModifiers })
      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        startPoint,
        canvasPoint({ x: 1, y: 1 }),
        {
          modifiers: emptyModifiers,
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(TestProjectWithFragment('row')),
      )
    })
    it('works with normal direction', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(TestProjectWithFragment('row')),
        'await-first-dom-report',
      )

      const targetElement = renderResult.renderedDOM.getByTestId('child-2')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
      await pressKey('Escape')
      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        startPoint,
        canvasPoint({ x: 62, y: 0 }),
        {
          modifiers: emptyModifiers,
          midDragCallback: async () => {
            expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
              'FLEX_REORDER',
            )
            expect(
              renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
            ).toEqual(2)
          },
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{ display: 'flex', gap: 10, flexDirection: 'row' }}
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
          data-testid='child-1'
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
        makeTestProjectCodeWithSnippet(TestProjectWithFragmentAbsoluteSibling),
        'await-first-dom-report',
      )

      const targetElement = renderResult.renderedDOM.getByTestId('child-2')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
      await pressKey('Escape')
      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        startPoint,
        canvasPoint({ x: 62, y: 0 }),
        {
          modifiers: emptyModifiers,
          midDragCallback: async () => {
            expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
              'FLEX_REORDER',
            )
            expect(
              renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
            ).toEqual(3)
          },
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
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
          data-testid='child-1'
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
        makeTestProjectCodeWithSnippet(TestProjectWithFragment('row-reverse')),
        'await-first-dom-report',
      )

      const targetElement = renderResult.renderedDOM.getByTestId('child-2')
      const targetElementBounds = targetElement.getBoundingClientRect()
      const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

      const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
      await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
      await pressKey('Escape')
      await mouseDragFromPointWithDelta(
        canvasControlsLayer,
        startPoint,
        canvasPoint({ x: 62, y: 0 }),
        {
          modifiers: emptyModifiers,
          midDragCallback: async () => {
            expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
              'FLEX_REORDER',
            )
            expect(
              renderResult.getEditorState().strategyState.customStrategyState?.lastReorderIdx,
            ).toEqual(0)
          },
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div
        data-uid='aaa'
        style={{
          display: 'flex',
          gap: 10,
          flexDirection: 'row-reverse',
        }}
      >
        <div
          data-uid='child-1'
          data-testid='child-1'
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
})
