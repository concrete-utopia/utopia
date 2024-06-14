import { windowPoint } from '../../../../core/shared/math-utils'
import { cmdModifier, emptyModifiers } from '../../../../utils/modifiers'
import CanvasActions from '../../canvas-actions'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../../event-helpers.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../../ui-jsx.test-utils'

const TestProject1 = `
<div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
  <div
    style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100 }}
    data-uid='child-1'
    data-testid='child-1'
  />
  <div
    style={{ backgroundColor: '#aaaaaa33', width: 200, height: 100, display: 'inline-flex' }}
    data-uid='child-2'
  />
  <div
    style={{ backgroundColor: '#aaaaaa33', width: 200, height: 100, display: 'inline-flex' }}
    data-uid='child-3'
  />
</div>
`
const TestProject2 = `
<div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
  <div
    style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100 }}
    data-uid='child-1'
    data-testid='child-1'
  />
  <div
    style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100 }}
    data-uid='child-2'
  />
  <div
    style={{ backgroundColor: '#aaaaaa33', width: 200, height: 100, position: 'relative' }}
    data-uid='child-3'
  />
</div>
`

describe('Drag To Move Metastrategy', () => {
  it('when no reparent or no base move is fit, the fallback DO_NOTHING strategy is used', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject1),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 10, y: 10 })

    const midDragCallback = async () => {
      expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual('DO_NOTHING')
    }

    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: emptyModifiers,
      midDragCallback: midDragCallback,
    })
  })
  it('when a reorder strategy is active, the fallback DO_NOTHING strategy is not applicable', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject2),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 10, y: 10 })

    const midDragCallback = async () => {
      const strategies = renderResult.getEditorState().strategyState.sortedApplicableStrategies
      const doNothingInSortedStrategies = strategies?.findIndex(
        (strategy) => strategy.name === 'DO_NOTHING',
      )

      expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual('FLOW_REORDER')
      expect(doNothingInSortedStrategies).toEqual(-1)
    }

    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: emptyModifiers,
      midDragCallback: midDragCallback,
    })
  })
})

describe('Drag To Move Strategy Indicator', () => {
  it('when the DO_NOTHING strategy is active, nothing is active in the Strategy Indicator', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject1),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 10, y: 10 })

    const midDragCallback = async () => {
      expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual('DO_NOTHING')

      const { showIndicator, dragType, reparent, ancestor } =
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags

      expect(showIndicator).toEqual(true)
      expect(dragType).toEqual('none')
      expect(reparent).toEqual('none')
      expect(ancestor).toEqual(false)

      const indicator = renderResult.renderedDOM.getByTestId('drag-strategy-indicator')
      expect(indicator).toBeDefined()
    }

    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: emptyModifiers,
      midDragCallback: midDragCallback,
    })
  })
  it('when reparenting an element the Strategy Indicator is visible', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject1),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: -50, y: 0 }) // moving it to the empty canvas

    const midDragCallback = async () => {
      expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
        'FLEX_REPARENT_TO_ABSOLUTE',
      )
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.showIndicator,
      ).toEqual(true)
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.ancestor,
      ).toEqual(false)
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.reparent,
      ).toEqual('different-component')
      const indicator = renderResult.renderedDOM.getByTestId('drag-strategy-indicator')
      expect(indicator).toBeDefined()
    }

    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: emptyModifiers,
      midDragCallback: midDragCallback,
    })
  })
  it('when reordering an element the Strategy Indicator is visible', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProject2),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 10, y: 10 })

    const midDragCallback = async () => {
      expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual('FLOW_REORDER')
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.showIndicator,
      ).toEqual(true)
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.dragType,
      ).toEqual('static')
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.reparent,
      ).toEqual('none')
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.ancestor,
      ).toEqual(false)
      const indicator = renderResult.renderedDOM.getByTestId('drag-strategy-indicator')
      expect(indicator).toBeDefined()
    }

    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: emptyModifiers,
      midDragCallback: midDragCallback,
    })
  })
  it('when absolute move strategy is active the Strategy Indicator is visible', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100, position: 'absolute' }}
          data-uid='child-1'
          data-testid='child-1'
        />
      </div>
      `),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 10, y: 10 })

    const midDragCallback = async () => {
      expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual('ABSOLUTE_MOVE')
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.showIndicator,
      ).toEqual(true)
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.dragType,
      ).toEqual('absolute')
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.reparent,
      ).toEqual('none')
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.ancestor,
      ).toEqual(false)
      const indicator = renderResult.renderedDOM.getByTestId('drag-strategy-indicator')
      expect(indicator).toBeDefined()
    }

    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: emptyModifiers,
      midDragCallback: midDragCallback,
    })
  })
  it('when ancestor move strategy is active the Strategy Indicator is visible', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ width: 100, height: 100, position: 'absolute' }} data-uid='aaa'>
        <div style={{ width: 100, height: 100, position: 'absolute' }} data-uid='bbb'>
          <div
            style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100 }}
            data-uid='child-1'
            data-testid='child-1'
          />
        </div>
      </div>
      `),
      'await-first-dom-report',
    )

    const targetElement = renderResult.renderedDOM.getByTestId('child-1')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 10, y: 10 })

    const midDragCallback = async () => {
      expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
        'ABSOLUTE_MOVE_ANCESTOR_1',
      )
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.showIndicator,
      ).toEqual(true)
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.dragType,
      ).toEqual('absolute')
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.reparent,
      ).toEqual('none')
      expect(
        renderResult.getEditorState().editor.canvas.controls.dragToMoveIndicatorFlags.ancestor,
      ).toEqual(true)
      const indicator = renderResult.renderedDOM.getByTestId('drag-strategy-indicator')
      expect(indicator).toBeDefined()
    }

    await mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
    await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
      modifiers: emptyModifiers,
      midDragCallback: midDragCallback,
    })
  })
})
