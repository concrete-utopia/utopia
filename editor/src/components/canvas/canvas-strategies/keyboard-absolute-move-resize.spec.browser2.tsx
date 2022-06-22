/* eslint-disable jest/expect-expect */
import { act, RenderResult } from '@testing-library/react'
import sinon, { SinonFakeTimers } from 'sinon'

import * as EP from '../../../core/shared/element-path'
import { isFeatureEnabled, setFeatureEnabled } from '../../../utils/feature-switches'
import { wait } from '../../../utils/utils.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { KeyboardInteractionTimeout } from './interaction-state'

describe('Keyboard Absolute Strategies E2E', () => {
  let originalCanvasStrategiesFSValue: boolean
  before(() => {
    viewport.set(2200, 1000)
    originalCanvasStrategiesFSValue = isFeatureEnabled('Canvas Strategies')
    setFeatureEnabled('Canvas Strategies', true)
  })
  after(() => {
    setFeatureEnabled('Canvas Strategies', originalCanvasStrategiesFSValue)
  })

  let clock: SinonFakeTimers
  beforeEach(function () {
    setFeatureEnabled('Canvas Strategies', true)
    clock = sinon.useFakeTimers({
      // the timers will tick so the editor is not totally broken, but we can fast-forward time at will
      // WARNING: the Sinon fake timers will advance in 20ms increments
      shouldAdvanceTime: true,
    })
  })
  afterEach(function () {
    clock.restore()
  })

  it('Pressing Shift + ArrowRight 3 times', async () => {
    const { expectElementLeftOnScreen, expectElementLeftInPrintedCode } = await setupTest()

    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)
    await expectElementLeftInPrintedCode(30)
  })

  it('Pressing Shift + ArrowRight 3 times, then pressing ArrowRight 3 times', async () => {
    const { expectElementLeftOnScreen, expectElementLeftInPrintedCode } = await setupTest()

    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)

    pressArrowRight3x()
    expectElementLeftOnScreen(33)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)
    await expectElementLeftInPrintedCode(33)
  })

  it('Pressing Shift + ArrowRight 3 times, then pressing ArrowLeft 3 times', async () => {
    const { expectElementLeftOnScreen, expectElementLeftInPrintedCode } = await setupTest()

    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)

    pressArrowLeft3x()
    expectElementLeftOnScreen(27)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)
    await expectElementLeftInPrintedCode(27)
  })

  it('Pressing Shift + ArrowRight 3 times, then pressing Esc before the keyboard strategy timer succeeds will cancel the strategy', async () => {
    const { expectElementLeftOnScreen, expectElementLeftInPrintedCode } = await setupTest()

    pressArrowRightHoldingShift3x()
    // the element visually moved 30 pixels to the right on screen
    expectElementLeftOnScreen(30)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)

    await expectElementLeftInPrintedCode(30)

    // move the element again
    pressArrowRightHoldingShift3x()
    // the element visually moved 30 pixels to the right on screen
    expectElementLeftOnScreen(60)
    // but it's still printed as 30 in code
    await expectElementLeftInPrintedCode(30)

    // press Escape to cancel changes
    pressEsc()

    // the element is back to +30, jumping back from 60
    expectElementLeftOnScreen(30)
    await expectElementLeftInPrintedCode(30)

    // Redo will not bring us back to +60 offset
    pressCmdShiftZ()
    expectElementLeftOnScreen(30)
    await expectElementLeftInPrintedCode(30)
  })

  it('Pressing Shift + ArrowRight 3 times, await keyboard strategy timeout, then press Cmd + Z to undo jumps back to original, but redoable', async () => {
    const { expectElementLeftOnScreen, expectElementLeftInPrintedCode } = await setupTest()

    // Setup: first we move the element 30 pixels to the right
    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    clock.tick(KeyboardInteractionTimeout)
    await expectElementLeftInPrintedCode(30)

    // then move the element again
    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(60)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)
    await expectElementLeftInPrintedCode(60)

    // Undo brings us back to the previous state with the 30 offset
    pressCmdZ()
    expectElementLeftOnScreen(30)
    await expectElementLeftInPrintedCode(30)

    // Redo redoes the +60 offset:
    pressCmdShiftZ()
    expectElementLeftOnScreen(60)
    await expectElementLeftInPrintedCode(60)
  })

  it('Pressing Shift + ArrowRight 3 times then IMMEDIATELY pressing Cmd + Z to undo jumps back to original, redoable', async () => {
    const { renderResult, expectElementLeftOnScreen, expectElementLeftInPrintedCode } =
      await setupTest()

    // Prepare the test, let's move the element by 30 and wait so we have a proper undo history entry
    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    clock.tick(KeyboardInteractionTimeout)
    await expectElementLeftInPrintedCode(30)

    // The actual test, move the element right 30
    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(60)
    await expectElementLeftInPrintedCode(30) // the printed code didn't update yet, because we are mid-interaction

    // And IMMEDIATELY press undo, which should save the interaction and undo it
    pressCmdZ()
    expect(renderResult.getEditorState().editor.canvas.interactionSession).toBeNull() // the interaction session is cleared
    expectElementLeftOnScreen(30)

    // TODO FIXME WE HAVE A BUG, We PRINT THE WRONG CODE!! this assertion should be true:
    // expectElementLeftInPrintedCode(30) // the printed happily stays 30

    // pressing Redo brings back the interaction
    pressCmdShiftZ()
    expectElementLeftOnScreen(60)
    await expectElementLeftInPrintedCode(60)
  })
})

async function setupTest() {
  expect(isFeatureEnabled('Canvas Strategies')).toBeTruthy()
  const initialElementLeft = 0
  const renderResult = await renderTestEditorWithCode(
    TestProjectDeluxeStallion(initialElementLeft),
    'await-first-dom-report',
  )
  await renderResult.dispatch(
    [selectComponents([EP.fromString('sb/scene/app-instance:aaa/bbb')], false)],
    true,
  )
  const bbbElementLeftAtStart = renderResult.renderedDOM
    .getByTestId('element-bbb')
    .getBoundingClientRect().x
  function expectElementLeftOnScreen(offset: number) {
    expect(elementLeft(renderResult.renderedDOM, 'element-bbb')).toEqual(
      bbbElementLeftAtStart + offset,
    )
  }
  async function expectElementLeftInPrintedCode(offset: number) {
    await renderResult.getDispatchFollowUpActionsFinished() // make sure the UPDATE_FROM_WORKER is settled
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(initialElementLeft + offset),
    )
  }

  return { renderResult, expectElementLeftOnScreen, expectElementLeftInPrintedCode }
}

function pressArrowRightHoldingShift3x() {
  act(() => {
    window.dispatchEvent(
      new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
    )
    window.dispatchEvent(
      new KeyboardEvent('keyup', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
    )
    window.dispatchEvent(
      new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
    )
    window.dispatchEvent(
      new KeyboardEvent('keyup', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
    )
    window.dispatchEvent(
      new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
    )
    window.dispatchEvent(
      new KeyboardEvent('keyup', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
    )
  })
}

function pressArrowRight3x() {
  act(() => {
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39 }))
    window.dispatchEvent(new KeyboardEvent('keyup', { key: 'ArrowRight', keyCode: 39 }))
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39 }))
    window.dispatchEvent(new KeyboardEvent('keyup', { key: 'ArrowRight', keyCode: 39 }))
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39 }))
    window.dispatchEvent(new KeyboardEvent('keyup', { key: 'ArrowRight', keyCode: 39 }))
  })
}

function pressArrowLeft3x() {
  act(() => {
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowLeft', keyCode: 37 }))
    window.dispatchEvent(new KeyboardEvent('keyup', { key: 'ArrowLeft', keyCode: 37 }))
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowLeft', keyCode: 37 }))
    window.dispatchEvent(new KeyboardEvent('keyup', { key: 'ArrowLeft', keyCode: 37 }))
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowLeft', keyCode: 37 }))
    window.dispatchEvent(new KeyboardEvent('keyup', { key: 'ArrowLeft', keyCode: 37 }))
  })
}

function pressEsc() {
  act(() => {
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', keyCode: 27 }))
    window.dispatchEvent(new KeyboardEvent('keyup', { key: 'Escape', keyCode: 27 }))
  })
}

function pressCmdZ() {
  act(() => {
    window.dispatchEvent(new KeyboardEvent('keydown', { key: 'z', keyCode: 90, metaKey: true }))
    window.dispatchEvent(new KeyboardEvent('keyup', { key: 'z', keyCode: 90, metaKey: true }))
  })
}

function pressCmdShiftZ() {
  act(() => {
    window.dispatchEvent(
      new KeyboardEvent('keydown', { key: 'z', keyCode: 90, metaKey: true, shiftKey: true }),
    )
    window.dispatchEvent(
      new KeyboardEvent('keyup', { key: 'z', keyCode: 90, metaKey: true, shiftKey: true }),
    )
  })
}

function elementLeft(renderedDom: RenderResult, testId: string): number {
  return renderedDom.getByTestId('element-bbb').getBoundingClientRect().x
}

const TestProjectDeluxeStallion = (bbbLeft: number) => `import * as React from 'react'
import Utopia, {
  Scene,
  View,
  Storyboard,
  registerModule,
} from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
      }}
      data-uid='aaa'
    >
      <View
        style={{
          backgroundColor: '#0091FFAA',
          position: 'absolute',
          left: ${bbbLeft},
          top: 100,
          width: 122,
          height: 101,
        }}
        data-uid='bbb'
        data-testid='element-bbb'
      />
      <View
        style={{
          backgroundColor: '#0091FFAA',
          position: 'absolute',
          left: 40,
          top: 300,
          width: 126,
          height: 96,
        }}
        data-uid='ccc'
      />
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        position: 'absolute',
        left: 0,
        top: 0,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app-instance' />
    </Scene>
  </Storyboard>
)
`
