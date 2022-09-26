import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import {
  CanvasStrategy,
  CanvasStrategyId,
  strategyApplicationResult,
} from '../../canvas-strategies/canvas-strategy-types'
import { act } from 'react-dom/test-utils'
import { fireEvent } from '@testing-library/react'
import { CanvasControlsContainerID } from '../new-canvas-controls'
import { applicableStrategy, ApplicableStrategy } from '../../canvas-strategies/canvas-strategies'
import { cmdModifier, emptyModifiers, Modifiers, shiftModifier } from '../../../../utils/modifiers'

const BestStrategy: CanvasStrategy = {
  id: 'BEST_STRATEGY' as CanvasStrategyId,
  name: () => 'Best Strategy',
  isApplicable: () => true,
  controlsToRender: [],
  fitness: () => 10,
  apply: () => strategyApplicationResult([]),
}

const AverageStrategy: CanvasStrategy = {
  id: 'AVERAGE_STRATEGY' as CanvasStrategyId,
  name: () => 'Average Strategy',
  isApplicable: () => true,
  controlsToRender: [],
  fitness: () => 5,
  apply: () => strategyApplicationResult([]),
}

const WorstStrategy: CanvasStrategy = {
  id: 'WORST_STRATEGY' as CanvasStrategyId,
  name: () => 'Worst Strategy',
  isApplicable: () => true,
  controlsToRender: [],
  fitness: () => 1,
  apply: () => strategyApplicationResult([]),
}

const UnfitStrategy: CanvasStrategy = {
  id: 'UNFIT_STRATEGY' as CanvasStrategyId,
  name: () => 'Unfit Strategy',
  isApplicable: () => false,
  controlsToRender: [],
  fitness: () => 0,
  apply: () => strategyApplicationResult([]),
}

// Deliberately not in sorted order
const allStrategies = [AverageStrategy, BestStrategy, UnfitStrategy, WorstStrategy]

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
  return act(() => {
    fireEvent(
      canvasControls,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: metaKey,
        altKey: false,
        shiftKey: false,
        clientX: startPoint.x,
        clientY: startPoint.y,
        buttons: 1,
      }),
    )

    fireEvent(
      canvasControls,
      new MouseEvent('mousemove', {
        bubbles: true,
        cancelable: true,
        metaKey: metaKey,
        altKey: false,
        shiftKey: false,
        clientX: endPoint.x,
        clientY: endPoint.y,
        buttons: 1,
      }),
    )
  })
}

function applicableStrategyForStrategy(strategy: CanvasStrategy): ApplicableStrategy {
  return applicableStrategy(strategy, strategy.name({} as any, {} as any, {} as any))
}

async function pressKey(key: string, modifiers: Modifiers): Promise<void> {
  return act(() => {
    fireEvent.keyDown(document.body, {
      bubbles: true,
      cancelable: true,
      key: key,
      metaKey: modifiers.cmd,
      altKey: modifiers.alt,
      shiftKey: modifiers.shift,
    })
    fireEvent.keyUp(document.body, {
      bubbles: true,
      cancelable: true,
      key: key,
      metaKey: modifiers.cmd,
      altKey: modifiers.alt,
      shiftKey: modifiers.shift,
    })
  })
}

async function pressTab(): Promise<void> {
  return pressKey('Tab', emptyModifiers)
}

async function pressShiftTab(): Promise<void> {
  return pressKey('Tab', shiftModifier)
}

describe('The strategy picker', () => {
  beforeEach(() => {
    viewport.set(2200, 1000)
  })
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
    await pressKey('3', emptyModifiers)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    await pressKey('2', emptyModifiers)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('1', emptyModifiers)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Supports using numeric keys to pick valid strategies whilst cmd is held down', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressKey('3', cmdModifier)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(WorstStrategy.id)
    await pressKey('2', cmdModifier)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('1', cmdModifier)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
  })
  it('Ignores numeric keys for invalid strategy numbers', async () => {
    const renderResult = await renderBasicModel()
    await startDraggingDefaultTarget(renderResult, false)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(BestStrategy.id)
    await pressKey('2', cmdModifier)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('0', emptyModifiers)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('4', emptyModifiers)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('9', emptyModifiers)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
    await pressKey('a', emptyModifiers)
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(AverageStrategy.id)
  })
})
