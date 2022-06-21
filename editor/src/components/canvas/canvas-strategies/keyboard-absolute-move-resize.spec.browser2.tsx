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
    expect(isFeatureEnabled('Canvas Strategies')).toBeTruthy()
    const initialElementLeft = 40
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

    act(() => {
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
    })

    // the element visually moved 30 pixels to the right on screen
    expect(renderResult.renderedDOM.getByTestId('element-bbb').getBoundingClientRect().x).toEqual(
      bbbElementLeftAtStart + 30,
    )

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)

    // the follow up action is the UPDATE_FROM_WORKER which prints the new code with the updated coordinates
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(initialElementLeft + 30),
    )
  })

  it('Pressing Shift + ArrowRight 3 times, then pressing Esc before the strategy timer succeeds', async () => {
    expect(isFeatureEnabled('Canvas Strategies')).toBeTruthy()
    const initialElementLeft = 40
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

    act(() => {
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
    })

    // the element visually moved 30 pixels to the right on screen
    expect(renderResult.renderedDOM.getByTestId('element-bbb').getBoundingClientRect().x).toEqual(
      bbbElementLeftAtStart + 30,
    )

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(initialElementLeft + 30),
    )

    // move the element again
    act(() => {
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
    })

    // the element visually moved 30 pixels to the right on screen
    expect(renderResult.renderedDOM.getByTestId('element-bbb').getBoundingClientRect().x).toEqual(
      bbbElementLeftAtStart + 60,
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    // press Escape to cancel changes
    act(() => {
      window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', keyCode: 27 }))
    })

    // the element is back to +30, jumping back from 60
    expect(renderResult.renderedDOM.getByTestId('element-bbb').getBoundingClientRect().x).toEqual(
      bbbElementLeftAtStart + 30,
    )

    // the follow up action is the UPDATE_FROM_WORKER which prints the new code with the updated coordinates
    await renderResult.getDispatchFollowUpActionsFinished()

    // the last 3 arrow rights should be gone
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(initialElementLeft + 30),
    )
  })

  it('Pressing Shift + ArrowRight 3 times, await keyboard timer, then press Cmd + Z to undo jumps back to original', async () => {
    expect(isFeatureEnabled('Canvas Strategies')).toBeTruthy()
    const initialElementLeft = 40
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

    act(() => {
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
    })

    // the element visually moved 30 pixels to the right on screen
    expect(renderResult.renderedDOM.getByTestId('element-bbb').getBoundingClientRect().x).toEqual(
      bbbElementLeftAtStart + 30,
    )

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(initialElementLeft + 30),
    )

    // move the element again
    act(() => {
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
    })

    expect(renderResult.renderedDOM.getByTestId('element-bbb').getBoundingClientRect().x).toEqual(
      bbbElementLeftAtStart + 60,
    )

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(initialElementLeft + 60),
    )

    // press CMD + Z to Undo
    act(() => {
      window.dispatchEvent(new KeyboardEvent('keydown', { key: 'z', keyCode: 90, metaKey: true }))
    })

    expect(renderResult.renderedDOM.getByTestId('element-bbb').getBoundingClientRect().x).toEqual(
      bbbElementLeftAtStart + 30,
    )

    // the follow up action is the UPDATE_FROM_WORKER which prints the new code with the updated coordinates
    await renderResult.getDispatchFollowUpActionsFinished()

    // the last 3 arrow rights should be gone
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(initialElementLeft + 30),
    )
  })

  it('Pressing Shift + ArrowRight 3 times, immediately press Cmd + Z to undo jumps back to original', async () => {
    expect(isFeatureEnabled('Canvas Strategies')).toBeTruthy()
    const initialElementLeft = 40
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

    function expectElementLeft(offset: number) {
      expect(elementLeft(renderResult.renderedDOM, 'element-bbb')).toEqual(
        bbbElementLeftAtStart + offset,
      )
    }

    act(() => {
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
    })

    expect(renderResult.renderedDOM.getByTestId('element-bbb').getBoundingClientRect().x).toEqual(
      bbbElementLeftAtStart + 30,
    )

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)

    // we should now have an undo history entry

    await renderResult.getDispatchFollowUpActionsFinished()

    expectElementLeft(30)

    act(() => {
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
    })

    expectElementLeft(60)

    // press CMD + Z to Undo WITHOUT waiting
    act(() => {
      window.dispatchEvent(new KeyboardEvent('keydown', { key: 'z', keyCode: 90, metaKey: true }))
    })

    expectElementLeft(30)

    expect(renderResult.getEditorState().editor.canvas.interactionSession).toBeNull()

    // press CMD + SHIFT + Z to Redo waiting
    act(() => {
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'z', keyCode: 90, metaKey: true, shiftKey: true }),
      )
    })

    expectElementLeft(60)
  })
})

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
