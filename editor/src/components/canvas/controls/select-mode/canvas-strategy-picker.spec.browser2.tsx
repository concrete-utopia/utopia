import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import type { CanvasStrategy } from '../../canvas-strategies/canvas-strategy-types'
import {
  ControlDelay,
  strategyApplicationResult,
} from '../../canvas-strategies/canvas-strategy-types'
import { CanvasControlsContainerID } from '../new-canvas-controls'
import type {
  ApplicableStrategy,
  MetaCanvasStrategy,
} from '../../canvas-strategies/canvas-strategies'
import { applicableStrategy } from '../../canvas-strategies/canvas-strategies'
import { cmdModifier, emptyModifiers, Modifiers, shiftModifier } from '../../../../utils/modifiers'
import {
  mouseDownAtPoint,
  mouseEnterAtPoint,
  mouseMoveToPoint,
  pressKey,
} from '../../event-helpers.test-utils'
import { getDrawToInsertStrategyName } from '../../canvas-strategies/strategies/draw-to-insert-metastrategy'
import { wait } from '../../../../utils/utils.test-utils'

const BestStrategy: CanvasStrategy = {
  id: 'BEST_STRATEGY',
  name: 'Best Strategy',
  descriptiveLabel: 'The Best Strategy',
  icon: { category: 'tools', type: 'pointer' },
  controlsToRender: [],
  fitness: 10,
  apply: () => strategyApplicationResult([], 'rerender-all-elements'),
}

const AverageStrategy: CanvasStrategy = {
  id: 'AVERAGE_STRATEGY',
  name: 'Average Strategy',
  descriptiveLabel: 'A Very Average Strategy',
  icon: { category: 'tools', type: 'pointer' },
  controlsToRender: [],
  fitness: 5,
  apply: () => strategyApplicationResult([], 'rerender-all-elements'),
}

const WorstStrategy: CanvasStrategy = {
  id: 'WORST_STRATEGY',
  name: 'Worst Strategy',
  descriptiveLabel: 'The Worst Strategy',
  icon: { category: 'tools', type: 'pointer' },
  controlsToRender: [],
  fitness: 1,
  apply: () => strategyApplicationResult([], 'rerender-all-elements'),
}

const UnfitStrategy: CanvasStrategy = {
  id: 'UNFIT_STRATEGY',
  name: 'Unfit Strategy',
  descriptiveLabel: 'Terribly Unfit Strategy',
  icon: { category: 'tools', type: 'pointer' },
  controlsToRender: [],
  fitness: 0,
  apply: () => strategyApplicationResult([], 'rerender-all-elements'),
}

// Deliberately not in sorted order
const allStrategies: MetaCanvasStrategy[] = [
  () => [AverageStrategy, BestStrategy, UnfitStrategy, WorstStrategy],
]

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
    { strategiesToUse: allStrategies },
  )
  await renderResult.getDispatchFollowUpActionsFinished()

  return renderResult
}

async function renderFlexModel(): Promise<EditorRenderResult> {
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(`
      <div style={{ width: '100%', height: '100%', position: 'relative' }}>
        <div
          style={{ backgroundColor: 'yellow', position: 'absolute', left: 40, top: 50, width: 200, height: 100 }}
          data-testid='absolute-target'
        />
        <div
          style={{ backgroundColor: 'blue', position: 'absolute', left: 40, top: 150, width: 200, height: 100, display: 'flex' }}
          data-testid='flex-target'
        />
      </div>
    `),
    'await-first-dom-report',
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
  it('Shows the correct strategy when using a delayed value', async () => {
    const renderResult = await renderFlexModel()

    const absoluteTarget = await renderResult.renderedDOM.findByTestId('absolute-target')
    const absoluteTargetElementRect = absoluteTarget.getBoundingClientRect()

    const absolutePoint = {
      x: absoluteTargetElementRect.x + absoluteTargetElementRect.width / 2,
      y: absoluteTargetElementRect.y + absoluteTargetElementRect.height / 2,
    }

    const flexTarget = await renderResult.renderedDOM.findByTestId('flex-target')
    const flexTargetElementRect = flexTarget.getBoundingClientRect()

    const flexPoint = {
      x: flexTargetElementRect.x + flexTargetElementRect.width / 2,
      y: flexTargetElementRect.y + flexTargetElementRect.height / 2,
    }

    const canvasControls = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    // Position the mouse over an absolute positioned element
    await mouseMoveToPoint(canvasControls, absolutePoint)
    await mouseEnterAtPoint(canvasControls, absolutePoint)

    // Enter insertion mode, so that the initially prefered insert strategy is absolute insertion
    await pressKey('d')
    await renderResult.getDispatchFollowUpActionsFinished()

    // Check we are in insertion mode and have the expected preferred strategy
    const expectedStartingStrategy = getDrawToInsertStrategyName('REPARENT_AS_ABSOLUTE', 'flow')
    expect(renderResult.getEditorState().editor.mode.type).toEqual('insert')
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
      expectedStartingStrategy,
    )

    // Quickly move the mouse over the flex element
    await mouseMoveToPoint(canvasControls, flexPoint)
    await mouseEnterAtPoint(canvasControls, flexPoint)
    await renderResult.getDispatchFollowUpActionsFinished()

    // Check that the preferred strategy has been updated
    const expectedEndingStrategy = getDrawToInsertStrategyName('REPARENT_AS_STATIC', 'flex')
    expect(renderResult.getEditorState().strategyState.currentStrategy).toEqual(
      expectedEndingStrategy,
    )

    // Wait until the strategy picker shows up
    await wait(ControlDelay + 1)

    // Check that the strategy picker shows the correct strategy
    const strategyPickerActiveRow = await renderResult.renderedDOM.findByTestId(
      'strategy-picker-active-row',
    )
    expect(strategyPickerActiveRow.textContent).toEqual(expectedEndingStrategy)
  })
})
