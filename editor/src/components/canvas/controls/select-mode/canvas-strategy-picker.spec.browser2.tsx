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
  name: () => 'Best Strategy',
  isApplicable: () => true,
  controlsToRender: [],
  fitness: () => 10,
  apply: () => strategyApplicationResult([]),
}

const AverageStrategy: CanvasStrategy = {
  id: 'AVERAGE_STRATEGY',
  name: () => 'Average Strategy',
  isApplicable: () => true,
  controlsToRender: [],
  fitness: () => 5,
  apply: () => strategyApplicationResult([]),
}

const WorstStrategy: CanvasStrategy = {
  id: 'WORST_STRATEGY',
  name: () => 'Worst Strategy',
  isApplicable: () => true,
  controlsToRender: [],
  fitness: () => 1,
  apply: () => strategyApplicationResult([]),
}

const UnfitStrategy: CanvasStrategy = {
  id: 'UNFIT_STRATEGY',
  name: () => 'Unfit Strategy',
  isApplicable: () => false,
  controlsToRender: [],
  fitness: () => 0,
  apply: () => strategyApplicationResult([]),
}

// Deliberately not in sorted order
const allStrategies = [() => [AverageStrategy, BestStrategy, UnfitStrategy, WorstStrategy]]

async function renderBasicModel(): Promise<EditorRenderResult> {
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(`
      <div style={{ width: '100%', height: '100%', position: 'relative' }}>
        <div
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
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
  mouseDownAtPoint(canvasControls, startPoint, { modifiers: cmdModifier })
  mouseMoveToPoint(canvasControls, endPoint, {
    modifiers: cmdModifier,
    eventOptions: { buttons: 1 },
  })
}

function applicableStrategyForStrategy(strategy: CanvasStrategy): ApplicableStrategy {
  return applicableStrategy(strategy, strategy.name({} as any, {} as any, {} as any))
}

function pressTab() {
  pressKey('Tab', { modifiers: emptyModifiers })
}

function pressShiftTab() {
  pressKey('Tab', { modifiers: shiftModifier })
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
    pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
  })
  it('Supports tabbing past the end to return to the first strategy', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    pressTab()
    pressTab()
    pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
  })
  it('Supports shift+tabbing to switch to the previous strategy', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    pressTab()
    pressTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Supports shift+tabbing past the beginning to switch to the last strategy', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressShiftTab()
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Supports using numeric keys to pick valid strategies', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    pressKey('3', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    pressKey('2', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressKey('1', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Supports using numeric keys to pick valid strategies whilst cmd is held down', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    pressKey('3', { modifiers: cmdModifier })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    pressKey('2', { modifiers: cmdModifier })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressKey('1', { modifiers: cmdModifier })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Ignores numeric keys for invalid strategy numbers', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    pressKey('2', { modifiers: cmdModifier })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressKey('0', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressKey('4', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressKey('9', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    pressKey('a', { modifiers: emptyModifiers })
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
  })
})
