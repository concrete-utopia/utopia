import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import {
  CanvasStrategy,
  strategyApplicationResult,
} from '../../canvas-strategies/canvas-strategy-types'
import { CanvasControlsContainerID } from '../new-canvas-controls'
import { applicableStrategy, ApplicableStrategy } from '../../canvas-strategies/canvas-strategies'
import { cmdModifier, emptyModifiers, Modifiers, shiftModifier } from '../../../../utils/modifiers'
import { mouseDownAtPoint, mouseMoveToPoint, pressKey } from '../../event-helpers.test-utils'

const BestStrategy: CanvasStrategy = {
  id: 'BEST_STRATEGY',
  name: 'Best Strategy',
  controlsToRender: [],
  fitness: 10,
  apply: () => strategyApplicationResult([]),
}

const AverageStrategy: CanvasStrategy = {
  id: 'AVERAGE_STRATEGY',
  name: 'Average Strategy',
  controlsToRender: [],
  fitness: 5,
  apply: () => strategyApplicationResult([]),
}

const WorstStrategy: CanvasStrategy = {
  id: 'WORST_STRATEGY',
  name: 'Worst Strategy',
  controlsToRender: [],
  fitness: 1,
  apply: () => strategyApplicationResult([]),
}

const UnfitStrategy: CanvasStrategy = {
  id: 'UNFIT_STRATEGY',
  name: 'Unfit Strategy',
  controlsToRender: [],
  fitness: 0,
  apply: () => strategyApplicationResult([]),
}

// Deliberately not in sorted order
const allStrategies = [() => [AverageStrategy, BestStrategy, UnfitStrategy, WorstStrategy]]

async function renderBasicModel(): Promise<EditorRenderResult> {
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(`
      <div style={{ width: '100%', height: '100%', position: 'relative' }}>
        <div
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
          data-testid='target'
        />
      </div>
    `),
    'await-first-dom-report',
    allStrategies,
  )
  await renderResult.getDispatchFollowUpActionsFinished()

  return renderResult
}

async function startDraggingDefaultTarget(
  renderResult: EditorRenderResult,
  metaKey: boolean,
): Promise<void> {
  const targetElement = await renderResult.renderedDOM.findByTestId('target')
  const targetElementRect = targetElement.getBoundingClientRect()
  const canvasControls = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = {
    x: targetElementRect.x + targetElementRect.width / 2,
    y: targetElementRect.y + targetElementRect.height / 2,
  }
  const endPoint = {
    x: targetElementRect.x + targetElementRect.width / 2 + 20,
    y: targetElementRect.y + targetElementRect.height / 2 + 20,
  }

  // Start dragging
  await mouseDownAtPoint(canvasControls, startPoint, { modifiers: cmdModifier })
  await mouseMoveToPoint(canvasControls, endPoint, {
    modifiers: cmdModifier,
    eventOptions: { buttons: 1 },
  })
}

function applicableStrategyForStrategy(strategy: CanvasStrategy): ApplicableStrategy {
  return applicableStrategy(strategy, strategy.name)
}

async function pressTab() {
  await pressKey('Tab', { modifiers: emptyModifiers })
}

async function pressShiftTab() {
  await pressKey('Tab', { modifiers: shiftModifier })
}

describe('The strategy picker', () => {
  it('Picks the best strategy by default', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Includes all fit strategies', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.sortedApplicableStrategies).toEqual([
      applicableStrategyForStrategy(BestStrategy),
      applicableStrategyForStrategy(AverageStrategy),
      applicableStrategyForStrategy(WorstStrategy),
    ])
  })
  it('Supports tabbing to switch to the next strategy', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
  })
  it('Supports tabbing past the end to return to the first strategy', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressTab()
    await pressTab()
    await pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
  })
  it('Supports shift+tabbing to switch to the previous strategy', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressTab()
    await pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    await pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Supports shift+tabbing past the beginning to switch to the last strategy', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    await pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Supports using numeric keys to pick valid strategies', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressKey('3', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    await pressKey('2', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('1', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Supports using numeric keys to pick valid strategies whilst cmd is held down', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressKey('3', { modifiers: cmdModifier })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    await pressKey('2', { modifiers: cmdModifier })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('1', { modifiers: cmdModifier })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Ignores numeric keys for invalid strategy numbers', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressKey('2', { modifiers: cmdModifier })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('0', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('4', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('9', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('a', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
  })
})
