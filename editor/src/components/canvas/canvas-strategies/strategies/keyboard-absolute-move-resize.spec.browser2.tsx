/* eslint-disable no-unused-expressions */
/* eslint-disable jest/expect-expect */
import { act, RenderResult } from '@testing-library/react'
import sinon, { SinonFakeTimers } from 'sinon'

import * as EP from '../../../../core/shared/element-path'
import { isFeatureEnabled, setFeatureEnabled } from '../../../../utils/feature-switches'
import { cmdModifier, shiftCmdModifier, shiftModifier } from '../../../../utils/modifiers'
import { wait } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'
import { pressKey } from '../../event-helpers.test-utils'
import { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../guideline'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { KeyboardInteractionTimeout } from '../interaction-state'

const defaultBBBProperties = {
  left: 0,
  top: 100,
  width: 122,
  height: 101,
}

function configureSetupTeardown(): { clock: { current: SinonFakeTimers } } {
  let clock: { current: SinonFakeTimers } = { current: null as any } // it will be non-null thanks to beforeEach
  beforeEach(function () {
    // TODO there is something wrong with sinon fake timers here that remotely break other tests that come after these. If your new browser tests are broken, this may be the reason.
    clock.current = sinon.useFakeTimers({
      // the timers will tick so the editor is not totally broken, but we can fast-forward time at will
      // WARNING: the Sinon fake timers will advance in 20ms increments
      shouldAdvanceTime: true,
    })
  })
  afterEach(function () {
    clock.current?.restore()
  })
  return { clock: clock }
}

describe('Keyboard Absolute Move E2E', () => {
  const { clock } = configureSetupTeardown()

  it('Pressing Shift + ArrowRight 3 times', async () => {
    const { expectElementLeftOnScreen, expectElementPropertiesInPrintedCode, getCanvasGuidelines } =
      await setupTest(defaultBBBProperties)

    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    expect(getCanvasGuidelines()).toEqual([])

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })
  })

  it('Pressing Shift + ArrowRight 3 times, then Shift + ArrowLeft 3 times', async () => {
    const { expectElementLeftOnScreen, expectElementPropertiesInPrintedCode, getCanvasGuidelines } =
      await setupTest(defaultBBBProperties)

    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    expect(getCanvasGuidelines()).toEqual([])

    pressArrowLeftHoldingShift3x()
    expectElementLeftOnScreen(0)
    expect(getCanvasGuidelines()).toEqual([
      {
        guideline: { type: 'XAxisGuideline', x: 0, yTop: 0, yBottom: 812 },
        snappingVector: { x: 0, y: 0 },
        pointsOfRelevance: [],
      },
    ])

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 0,
      top: 100,
      width: 122,
      height: 101,
    })
  })

  it('Pressing Shift + ArrowRight 3 times, then pressing ArrowRight 3 times', async () => {
    const { expectElementLeftOnScreen, expectElementPropertiesInPrintedCode } = await setupTest(
      defaultBBBProperties,
    )

    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)

    pressArrowRight3x()
    expectElementLeftOnScreen(33)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 33,
      top: 100,
      width: 122,
      height: 101,
    })
  })

  it('Pressing Shift + ArrowRight 3 times, then pressing ArrowLeft 3 times', async () => {
    const { expectElementLeftOnScreen, expectElementPropertiesInPrintedCode } = await setupTest(
      defaultBBBProperties,
    )

    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)

    pressArrowLeft3x()
    expectElementLeftOnScreen(27)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 27,
      top: 100,
      width: 122,
      height: 101,
    })
  })

  it('Pressing Shift + ArrowRight 3 times for element with missing left prop', async () => {
    const { expectElementLeftOnScreen, expectElementPropertiesInPrintedCode } = await setupTest({
      top: 100,
      width: 122,
      height: 101,
    })

    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      top: 100,
      width: 122,
      height: 101,
      left: 30,
    })
  })
})

describe('Keyboard Absolute Resize E2E', () => {
  const { clock } = configureSetupTeardown()

  it('Pressing Cmd + ArrowRight 3 times, then pressing Cmd + ArrowLeft once', async () => {
    const {
      expectElementWidthOnScreen,
      expectElementPropertiesInPrintedCode,
      getCanvasGuidelines,
    } = await setupTest({
      left: 10,
      top: 100,
      width: 28,
      height: 101,
    })

    pressArrowRightHoldingCmd3x()
    expectElementWidthOnScreen(3)
    expect(getCanvasGuidelines()).toEqual([])

    pressArrorLeftHoldingCmd()
    expectElementWidthOnScreen(2)
    expect(getCanvasGuidelines()).toEqual([
      {
        guideline: { type: 'XAxisGuideline', x: 40, yBottom: 396, yTop: 300 },
        pointsOfRelevance: [],
        snappingVector: { x: 0, y: 0 },
      },
    ])

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 10,
      top: 100,
      width: 30,
      height: 101,
    })
  })
})

describe('Keyboard Strategies Escape Behavior', () => {
  const { clock } = configureSetupTeardown()
  it('pressing Esc does not cancel the keyboard-based strategy', async () => {
    const { expectElementLeftOnScreen, expectElementPropertiesInPrintedCode } = await setupTest(
      defaultBBBProperties,
    )

    pressArrowRightHoldingShift3x()
    // the element visually moved 30 pixels to the right on screen
    expectElementLeftOnScreen(30)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)

    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })

    // move the element again
    pressArrowRightHoldingShift3x()
    // the element visually moved 30 pixels to the right on screen
    expectElementLeftOnScreen(60)
    // but it's still printed as 30 in code
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })

    // press Escape does not cancel the keyboard-based strategy, instead commits it
    pressEsc()

    expectElementLeftOnScreen(60)
    await expectElementPropertiesInPrintedCode({
      left: 60,
      top: 100,
      width: 122,
      height: 101,
    })
  })
})

describe('Keyboard Strategies Deletion Behavior', () => {
  const { clock } = configureSetupTeardown()

  it('Pressing ArrowRight 3 times, then immediately deleting the element: the element is deleted, but undoable', async () => {
    const {
      expectElementLeftOnScreen,
      expectElementPropertiesInPrintedCode,
      expectElementDoesntExist,
      getCanvasGuidelines,
    } = await setupTest(defaultBBBProperties)

    // setting up the project
    pressArrowRight3x()
    expectElementLeftOnScreen(3)
    clock.current.tick(KeyboardInteractionTimeout)

    // the test begins
    pressArrowRight3x()
    expectElementLeftOnScreen(6)

    // delete the element
    pressBackspace()

    // the element is deleted
    expectElementDoesntExist()

    // undo the deletion
    pressCmdZ()

    // the element is back to +3, jumping back from the dead
    expectElementLeftOnScreen(6)

    // undo the move
    pressCmdZ()

    // the element is back to 0
    expectElementLeftOnScreen(3)
  })
})

describe('Keyboard Strategies Undo Behavior', () => {
  const { clock } = configureSetupTeardown()

  it('Pressing Shift + ArrowRight 3 times, await keyboard strategy timeout, then press Cmd + Z to undo jumps back to original, but redoable', async () => {
    const { expectElementLeftOnScreen, expectElementPropertiesInPrintedCode } = await setupTest(
      defaultBBBProperties,
    )

    // Setup: first we move the element 30 pixels to the right
    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })

    // then move the element again
    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(60)

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 60,
      top: 100,
      width: 122,
      height: 101,
    })

    // Undo brings us back to the previous state with the 30 offset
    pressCmdZ()
    expectElementLeftOnScreen(30)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })

    // Redo redoes the +60 offset:
    pressCmdShiftZ()
    expectElementLeftOnScreen(60)
    await expectElementPropertiesInPrintedCode({
      left: 60,
      top: 100,
      width: 122,
      height: 101,
    })
  })

  it('Pressing Shift + ArrowRight 3 times then IMMEDIATELY pressing Cmd + Z to undo jumps back to original, redoable', async () => {
    const { renderResult, expectElementLeftOnScreen, expectElementPropertiesInPrintedCode } =
      await setupTest(defaultBBBProperties)

    // Prepare the test, let's move the element by 30 and wait so we have a proper undo history entry
    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })

    // The actual test, move the element right 30
    pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(60)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    }) // the printed code didn't update yet, because we are mid-interaction

    // And IMMEDIATELY press undo, which should save the interaction and undo it
    pressCmdZ()
    expect(renderResult.getEditorState().editor.canvas.interactionSession).toBeNull() // the interaction session is cleared
    expectElementLeftOnScreen(30)

    await expectElementPropertiesInPrintedCode({
      left: 30, // the printed happily stays 30
      top: 100,
      width: 122,
      height: 101,
    })

    // pressing Redo brings back the interaction
    pressCmdShiftZ()
    expectElementLeftOnScreen(60)
    await expectElementPropertiesInPrintedCode({
      left: 60,
      top: 100,
      width: 122,
      height: 101,
    })
  })
})

function elementExists(renderedDom: RenderResult, testId: string): boolean {
  return renderedDom.queryByTestId(testId) != null
}

function elementLeft(renderedDom: RenderResult, testId: string): number {
  return renderedDom.getByTestId(testId).getBoundingClientRect().x
}

function elementWidth(renderedDom: RenderResult, testId: string): number {
  return renderedDom.getByTestId(testId).getBoundingClientRect().width
}

async function setupTest(initialBBBProperties: { [key: string]: any }) {
  const renderResult = await renderTestEditorWithCode(
    TestProjectDeluxeStallion(initialBBBProperties),
    'await-first-dom-report',
  )
  await renderResult.dispatch(
    [selectComponents([EP.fromString('sb/scene/app-instance:aaa/bbb')], false)],
    true,
  )
  const bbbElementLeftAtStart = elementLeft(renderResult.renderedDOM, 'element-bbb')
  const bbbElementWidthAtStart = elementWidth(renderResult.renderedDOM, 'element-bbb')
  function expectElementLeftOnScreen(offset: number) {
    expect(elementLeft(renderResult.renderedDOM, 'element-bbb')).toEqual(
      bbbElementLeftAtStart + offset,
    )
  }

  function expectElementDoesntExist() {
    expect(elementExists(renderResult.renderedDOM, 'element-bbb')).toBeFalsy()
  }

  function expectElementWidthOnScreen(offset: number) {
    expect(elementWidth(renderResult.renderedDOM, 'element-bbb')).toEqual(
      bbbElementWidthAtStart + offset,
    )
  }
  async function expectElementPropertiesInPrintedCode(bbbProperties: { [key: string]: any }) {
    await renderResult.getDispatchFollowUpActionsFinished() // make sure the UPDATE_FROM_WORKER is settled
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(bbbProperties),
    )
  }
  function getCanvasGuidelines(): Array<GuidelineWithSnappingVectorAndPointsOfRelevance> {
    return renderResult.getEditorState().editor.canvas.controls.snappingGuidelines
  }
  return {
    renderResult,
    expectElementLeftOnScreen,
    expectElementWidthOnScreen,
    expectElementPropertiesInPrintedCode,
    expectElementDoesntExist,
    getCanvasGuidelines,
  }
}

function pressArrowRightHoldingCmd3x() {
  pressKey('ArrowRight', { modifiers: cmdModifier })
  pressKey('ArrowRight', { modifiers: cmdModifier })
  pressKey('ArrowRight', { modifiers: cmdModifier })
}

function pressArrorLeftHoldingCmd() {
  pressKey('ArrowLeft', { modifiers: cmdModifier })
}

function pressArrowLeftHoldingShift3x() {
  pressKey('ArrowLeft', { modifiers: shiftModifier })
  pressKey('ArrowLeft', { modifiers: shiftModifier })
  pressKey('ArrowLeft', { modifiers: shiftModifier })
}

function pressArrowRightHoldingShift3x() {
  pressKey('ArrowRight', { modifiers: shiftModifier })
  pressKey('ArrowRight', { modifiers: shiftModifier })
  pressKey('ArrowRight', { modifiers: shiftModifier })
}

function pressArrowRight3x() {
  pressKey('ArrowRight')
  pressKey('ArrowRight')
  pressKey('ArrowRight')
}

function pressArrowLeft3x() {
  pressKey('ArrowLeft')
  pressKey('ArrowLeft')
  pressKey('ArrowLeft')
}

function pressEsc() {
  pressKey('Escape')
}

function pressBackspace() {
  pressKey('Backspace')
}

function pressCmdZ() {
  pressKey('z', { modifiers: cmdModifier })
}

function pressCmdShiftZ() {
  pressKey('z', { modifiers: shiftCmdModifier })
}

const TestProjectDeluxeStallion = (bbbDimensions: { [key: string]: any }) => {
  let dimensionLines: Array<string> = []
  const prefix = `import * as React from 'react'
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
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
`
  const suffix = `        }}
        data-uid='bbb'
        data-testid='element-bbb'
      />
      <View
        style={{
          backgroundColor: '#aaaaaa33',
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
  let result = prefix
  for (const dimensionKey of Object.keys(bbbDimensions)) {
    const line = `          ${dimensionKey}: ${bbbDimensions[dimensionKey]},\n`
    result += line
  }
  result += suffix
  return result
}
