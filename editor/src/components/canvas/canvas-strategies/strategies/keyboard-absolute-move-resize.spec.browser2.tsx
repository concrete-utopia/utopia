/* eslint-disable no-unused-expressions */
/* eslint-disable jest/expect-expect */
import type { RenderResult } from '@testing-library/react'
import type { SinonFakeTimers } from 'sinon'
import sinon from 'sinon'

import * as EP from '../../../../core/shared/element-path'
import {
  altModifier,
  cmdModifier,
  shiftCmdModifier,
  shiftModifier,
} from '../../../../utils/modifiers'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import { selectComponents, setHighlightedView } from '../../../editor/actions/action-creators'
import { pressKey, keyDown, keyUp } from '../../event-helpers.test-utils'
import type { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../guideline'
import {
  TestAppUID,
  TestSceneUID,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { KeyboardInteractionTimeout } from '../interaction-state'
import type { FragmentLikeType } from './fragment-like-helpers'
import { AllFragmentLikeTypes } from './fragment-like-helpers'
import {
  getClosingFragmentLikeTag,
  getOpeningFragmentLikeTag,
  FragmentLikeElementUid,
} from './fragment-like-helpers.test-utils'
import { ResizeMinimumValue } from './keyboard-absolute-resize-strategy'
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'

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

    await pressArrowRightHoldingShift3x()
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

    await pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    expect(getCanvasGuidelines()).toEqual([])

    await pressArrowLeftHoldingShift3x()
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

    await pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)

    await pressArrowRight3x()
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

    await pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)

    await pressArrowLeft3x()
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

    await pressArrowRightHoldingShift3x()
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

  describe('retargets to group children', () => {
    AllFragmentLikeTypes.forEach((type) => {
      it(`moves children of ${type}`, async () => {
        const editor = await renderTestEditorWithCode(
          projectWithGroup(type),
          'await-first-dom-report',
        )
        await selectComponentsForTest(editor, [EP.fromString(`sb/${FragmentLikeElementUid}`)])

        await pressArrowRightHoldingShift3x()

        const descriptiveLabel =
          editor.getEditorState().strategyState.currentStrategyDescriptiveLabel
        expect(descriptiveLabel).toEqual('Moving Elements (Children)')

        await editor.getDispatchFollowUpActionsFinished()

        const aaa = editor.renderedDOM.getByTestId('aaa')
        const bbb = editor.renderedDOM.getByTestId('bbb')

        expect(aaa.style.top).toEqual('210px')
        expect(aaa.style.left).toEqual('38px')
        expect(bbb.style.top).toEqual('8px')
        expect(bbb.style.left).toEqual('38px')
      })
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

    await cmdKeyDown()
    await keyDownArrowRightHoldingCmd3x()
    expectElementWidthOnScreen(3)
    expect(getCanvasGuidelines()).toEqual([])

    await keyDownArrowLeftHoldingCmd()
    expectElementWidthOnScreen(2)
    expect(getCanvasGuidelines()).toEqual([
      {
        guideline: { type: 'XAxisGuideline', x: 40, yBottom: 396, yTop: 100 },
        pointsOfRelevance: [],
        snappingVector: { x: 0, y: 0 },
      },
    ])
    await cmdKeyUp()

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 10,
      top: 100,
      width: 30,
      height: 101,
    })
  })
  it('Pressing Shift + Cmd + ArrowLeft 3 times, then pressing Shift + Cmd + ArrowRight once', async () => {
    const width = 5
    const {
      expectElementWidthOnScreen,
      expectElementPropertiesInPrintedCode,
      getCanvasGuidelines,
    } = await setupTest({
      left: 10,
      top: 100,
      width: width,
      height: 101,
    })

    await pressKey('ArrowLeft', { modifiers: shiftCmdModifier })
    await pressKey('ArrowLeft', { modifiers: shiftCmdModifier })
    await pressKey('ArrowLeft', { modifiers: shiftCmdModifier })

    expectElementWidthOnScreen(-width + ResizeMinimumValue) // the expected size is the min value
    expect(getCanvasGuidelines()).toEqual([])

    await pressKey('ArrowRight', { modifiers: shiftCmdModifier })
    await cmdKeyUp()

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 10,
      top: 100,
      width: 11,
      height: 101,
    })
  })

  it('keeps trueing up groups as directions change', async () => {
    const renderResult = await renderTestEditorWithCode(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <Group
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 150,
                height: 250,
                backgroundColor: 'white',
              }}
              data-uid='group'
            >
              <div
                style={{
                  height: '100%',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 83,
                  backgroundColor: 'blue',
                }}
                data-uid='left'
              />
              <div
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  left: 100,
                  top: 26,
                  width: 50,
                  height: 16,
                }}
                data-uid='right'
              />
            </Group>
          </div>
        `),
      ),
      'await-first-dom-report',
    )

    const groupPath = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group`,
    )
    await renderResult.dispatch([selectComponents([groupPath], false)], true)

    await keyDownArrowLeftHoldingCmd()
    await keyDownArrowLeftHoldingCmd()
    await keyDownArrowLeftHoldingCmd()
    await keyDownArrowLeftHoldingCmd()
    await keyDownArrowLeftHoldingCmd()
    await keyDownArrowLeftHoldingCmd()

    await keyDownArrowDownHoldingCmd()
    await keyDownArrowDownHoldingCmd()
    await keyDownArrowDownHoldingCmd()
    await keyDownArrowDownHoldingCmd()
    await keyDownArrowDownHoldingCmd()

    clock.current.tick(KeyboardInteractionTimeout)

    await wait(500)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      formatTestProjectCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <Group
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 144,
                height: 255,
                backgroundColor: 'white',
              }}
              data-uid='group'
            >
              <div
                style={{
                  height: 255, 
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 80,
                  backgroundColor: 'blue',
                }}
                data-uid='left'
              />
              <div
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  left: 96,
                  top: 27,
                  width: 48,
                  height: 16,
                }}
                data-uid='right'
              />
            </Group>
          </div>
        `),
      ),
    )
  })
})

describe('Keyboard switching back and forth between absolute move and absolute resize', () => {
  const { clock } = configureSetupTeardown()

  it('Pressing ArrowRight 3 times, then Cmd + ArrowRight 3 times, then ArrowLeft once, then Cmd + ArrowLeft once', async () => {
    // This should result in 4 separate interactions - move right, increase size, move left, decrease size
    const { expectElementPropertiesInPrintedCode } = await setupTest({
      left: 10,
      top: 100,
      width: 28,
      height: 101,
    })

    await pressArrowRight3x()
    await cmdKeyDown()
    await keyDownArrowRightHoldingCmd3x()
    await cmdKeyUp()
    await pressArrowLeft()
    await cmdKeyDown()
    await keyDownArrowLeftHoldingCmd()
    await cmdKeyUp()

    // tick the clock so useClearKeyboardInteraction is fired
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 12,
      top: 100,
      width: 30,
      height: 101,
    })
  })

  describe('retargets to group children', () => {
    AllFragmentLikeTypes.forEach((type) => {
      it(`resizes children of ${type}`, async () => {
        const editor = await renderTestEditorWithCode(
          projectWithGroup(type),
          'await-first-dom-report',
        )
        await selectComponentsForTest(editor, [EP.fromString(`sb/${FragmentLikeElementUid}`)])

        await keyDownArrowRightHoldingCmd3x()

        const descriptiveLabel =
          editor.getEditorState().strategyState.currentStrategyDescriptiveLabel
        expect(descriptiveLabel).toEqual('Resizing Elements (Children)')

        await editor.getDispatchFollowUpActionsFinished()

        const aaa = editor.renderedDOM.getByTestId('aaa')
        const bbb = editor.renderedDOM.getByTestId('bbb')

        expect(aaa.style.top).toEqual('210px')
        expect(aaa.style.left).toEqual('8px')
        expect(aaa.style.width).toEqual('76px')
        expect(aaa.style.height).toEqual('109px')
        expect(bbb.style.top).toEqual('8px')
        expect(bbb.style.left).toEqual('8px')
        expect(bbb.style.width).toEqual('210px')
        expect(bbb.style.height).toEqual('202px')
      })
    })
  })
})

describe('Keyboard move shows correct distance controls', () => {
  configureSetupTeardown()

  it('Pressing ArrowRight 3 times whilst holding the option key and hovering an element shows distance controls to that element', async () => {
    const { renderResult } = await setupTest({
      left: 10,
      top: 10,
      width: 10,
      height: 10,
    })

    const targetToHighlight = 'sb/scene/app-instance:aaa/ccc'

    await renderResult.dispatch([setHighlightedView(EP.fromString(targetToHighlight))], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    await keyDown('Alt', { modifiers: altModifier })
    const distanceLabelRight = await renderResult.renderedDOM.getByTestId(`distance-0-label`)
    const distanceLabelBottom = await renderResult.renderedDOM.getByTestId(`distance-1-label`)
    expect(distanceLabelRight.innerHTML).toEqual(`20`)
    expect(distanceLabelBottom.innerHTML).toEqual(`280`)

    // After each move we should still see the distance controls to the hovered element
    await pressKey('ArrowRight', { modifiers: altModifier })
    expect(distanceLabelRight.innerHTML).toEqual(`19`)
    expect(distanceLabelBottom.innerHTML).toEqual(`280`)

    await pressKey('ArrowRight', { modifiers: altModifier })
    expect(distanceLabelRight.innerHTML).toEqual(`18`)
    expect(distanceLabelBottom.innerHTML).toEqual(`280`)

    await pressKey('ArrowRight', { modifiers: altModifier })
    expect(distanceLabelRight.innerHTML).toEqual(`17`)
    expect(distanceLabelBottom.innerHTML).toEqual(`280`)
    await keyUp('Alt')

    const noDistanceLabelRight = await renderResult.renderedDOM.queryByTestId(`distance-0-label`)
    const noDistanceLabelBottom = await renderResult.renderedDOM.queryByTestId(`distance-1-label`)
    expect(noDistanceLabelRight).toBeNull()
    expect(noDistanceLabelBottom).toBeNull()
  })

  it('Pressing ArrowRight 3 times whilst holding the option key and not hovering an element shows distance controls to the parent', async () => {
    const { renderResult } = await setupTest({
      left: 10,
      top: 10,
      width: 10,
      height: 10,
    })

    await keyDown('Alt', { modifiers: altModifier })
    const distanceLabelLeft = await renderResult.renderedDOM.getByTestId(`distance-left-label`)
    const distanceLabelRight = await renderResult.renderedDOM.getByTestId(`distance-right-label`)
    const distanceLabelTop = await renderResult.renderedDOM.getByTestId(`distance-top-label`)
    const distanceLabelBottom = await renderResult.renderedDOM.getByTestId(`distance-bottom-label`)
    expect(distanceLabelLeft.innerHTML).toEqual(`10`)
    expect(distanceLabelRight.innerHTML).toEqual(`355`)
    expect(distanceLabelTop.innerHTML).toEqual(`10`)
    expect(distanceLabelBottom.innerHTML).toEqual(`792`)

    // After each move we should still see the distance controls to the parent
    await pressKey('ArrowRight', { modifiers: altModifier })
    expect(distanceLabelLeft.innerHTML).toEqual(`11`)
    expect(distanceLabelRight.innerHTML).toEqual(`354`)
    expect(distanceLabelTop.innerHTML).toEqual(`10`)
    expect(distanceLabelBottom.innerHTML).toEqual(`792`)

    await pressKey('ArrowRight', { modifiers: altModifier })
    expect(distanceLabelLeft.innerHTML).toEqual(`12`)
    expect(distanceLabelRight.innerHTML).toEqual(`353`)
    expect(distanceLabelTop.innerHTML).toEqual(`10`)
    expect(distanceLabelBottom.innerHTML).toEqual(`792`)

    await pressKey('ArrowRight', { modifiers: altModifier })
    expect(distanceLabelLeft.innerHTML).toEqual(`13`)
    expect(distanceLabelRight.innerHTML).toEqual(`352`)
    expect(distanceLabelTop.innerHTML).toEqual(`10`)
    expect(distanceLabelBottom.innerHTML).toEqual(`792`)
    await keyUp('Alt')

    const noDistanceLabelLeft = await renderResult.renderedDOM.queryByTestId(`distance-left-label`)
    const noDistanceLabelRight = await renderResult.renderedDOM.queryByTestId(
      `distance-right-label`,
    )
    const noDistanceLabelTop = await renderResult.renderedDOM.queryByTestId(`distance-top-label`)
    const noDistanceLabelBottom = await renderResult.renderedDOM.queryByTestId(
      `distance-bottom-label`,
    )
    expect(noDistanceLabelLeft).toBeNull()
    expect(noDistanceLabelRight).toBeNull()
    expect(noDistanceLabelTop).toBeNull()
    expect(noDistanceLabelBottom).toBeNull()
  })
})

describe('Keyboard Strategies Escape Behavior', () => {
  const { clock } = configureSetupTeardown()
  it('pressing Esc does not cancel the keyboard-based strategy', async () => {
    const { expectElementLeftOnScreen, expectElementPropertiesInPrintedCode } = await setupTest(
      defaultBBBProperties,
    )

    await pressArrowRightHoldingShift3x()
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
    await pressArrowRightHoldingShift3x()
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
    await pressEsc()

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
    await pressArrowRight3x()
    expectElementLeftOnScreen(3)
    clock.current.tick(KeyboardInteractionTimeout)

    // the test begins
    await pressArrowRight3x()
    expectElementLeftOnScreen(6)

    // delete the element
    await pressBackspace()

    // the element is deleted
    expectElementDoesntExist()

    // undo the deletion
    await pressCmdZ()

    // the element is back to +3, jumping back from the dead
    expectElementLeftOnScreen(6)

    // undo the move
    await pressCmdZ()

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
    await pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })

    // then move the element again
    await pressArrowRightHoldingShift3x()
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
    await pressCmdZ()
    expectElementLeftOnScreen(30)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })

    // Redo redoes the +60 offset:
    await pressCmdShiftZ()
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
    await pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(30)
    clock.current.tick(KeyboardInteractionTimeout)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    })

    // The actual test, move the element right 30
    await pressArrowRightHoldingShift3x()
    expectElementLeftOnScreen(60)
    await expectElementPropertiesInPrintedCode({
      left: 30,
      top: 100,
      width: 122,
      height: 101,
    }) // the printed code didn't update yet, because we are mid-interaction

    // And IMMEDIATELY press undo, which should save the interaction and undo it
    await pressCmdZ()
    expect(renderResult.getEditorState().editor.canvas.interactionSession).toBeNull() // the interaction session is cleared
    expectElementLeftOnScreen(30)

    await expectElementPropertiesInPrintedCode({
      left: 30, // the printed happily stays 30
      top: 100,
      width: 122,
      height: 101,
    })

    // pressing Redo brings back the interaction
    await pressCmdShiftZ()
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

// MacOS doesn't trigger keyup events for any keys whilst cmd is held down
async function cmdKeyDown() {
  await keyDown('Meta', { modifiers: cmdModifier })
}

async function cmdKeyUp() {
  await keyUp('Meta')
}

async function keyDownArrowRightHoldingCmd3x() {
  await keyDown('ArrowRight', { modifiers: cmdModifier })
  await keyDown('ArrowRight', { modifiers: cmdModifier })
  await keyDown('ArrowRight', { modifiers: cmdModifier })
}

async function keyDownArrowLeftHoldingCmd() {
  await keyDown('ArrowLeft', { modifiers: cmdModifier })
}

async function pressArrowLeftHoldingShift3x() {
  await pressKey('ArrowLeft', { modifiers: shiftModifier })
  await pressKey('ArrowLeft', { modifiers: shiftModifier })
  await pressKey('ArrowLeft', { modifiers: shiftModifier })
}

async function pressArrowRightHoldingShift3x() {
  await pressKey('ArrowRight', { modifiers: shiftModifier })
  await pressKey('ArrowRight', { modifiers: shiftModifier })
  await pressKey('ArrowRight', { modifiers: shiftModifier })
}

async function pressArrowRight3x() {
  await pressKey('ArrowRight')
  await pressKey('ArrowRight')
  await pressKey('ArrowRight')
}

async function pressArrowLeft() {
  await pressKey('ArrowLeft')
}

async function pressArrowLeft3x() {
  await pressKey('ArrowLeft')
  await pressKey('ArrowLeft')
  await pressKey('ArrowLeft')
}

async function keyDownArrowDownHoldingCmd() {
  await keyDown('ArrowDown', { modifiers: cmdModifier })
}

async function pressEsc() {
  await pressKey('Escape')
}

async function pressBackspace() {
  await pressKey('Backspace')
}

async function pressCmdZ() {
  await cmdKeyDown()
  await pressKey('z', { modifiers: cmdModifier })
  await cmdKeyUp()
}

async function pressCmdShiftZ() {
  await cmdKeyDown()
  await pressKey('z', { modifiers: shiftCmdModifier })
  await cmdKeyUp()
}

const TestProjectDeluxeStallion = (bbbDimensions: { [key: string]: any }) => {
  let dimensionLines: Array<string> = []
  const prefix = `import * as React from 'react'
import Utopia, { Scene, View, Storyboard } from 'utopia-api'

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

const projectWithGroup = (type: FragmentLikeType) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    ${getOpeningFragmentLikeTag(type)}
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 73,
          height: 109,
          left: 8,
          top: 210,
          position: 'absolute',
        }}
        data-uid='aaa'
        data-testid='aaa'
      >
        whaddup
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 207,
          height: 202,
          left: 8,
          top: 8,
          position: 'absolute',
        }}
        data-uid='aab'
        data-testid='bbb'
      >
        whaddup
      </div>
      ${getClosingFragmentLikeTag(type)}
  </Storyboard>
)
`
