import { assertNever } from '../../../../core/shared/utils'
import { cmdModifier, Modifiers, shiftModifier } from '../../../../utils/modifiers'
import { wait } from '../../../../utils/utils.test-utils'
import { cssNumber } from '../../../inspector/common/css-utils'
import { EdgePiece } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  AdjustPrecision,
  cssNumberWithRenderedValue,
  unitlessCSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import {
  paddingControlHandleTestId,
  paddingControlTestId,
  PaddingResizeControlHoverTimeout,
} from '../../controls/select-mode/padding-resize-control'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import {
  offsetPaddingByEdge,
  paddingToPaddingString,
  CSSPaddingMeasurements,
  CSSPaddingMappedValues,
  combinePaddings,
  paddingPropForEdge,
  PaddingAdjustMode,
} from '../../padding-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { PaddingTearThreshold, SetPaddingStrategyName } from './set-padding-strategy'

const EdgePieces: Array<EdgePiece> = ['top', 'bottom', 'left', 'right']

describe('Padding resize strategy', () => {
  it('Padding resize handle is not present for elements that have no padding set', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div
            data-uid='mydiv'
            data-testid='mydiv'
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 28,
              top: 28,
              width: 612,
              height: 461,
            }}
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 50,
      y: divBounds.y + 40,
    }

    mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    const paddingControls = EdgePieces.flatMap((edge) => [
      ...editor.renderedDOM.queryAllByTestId(paddingControlTestId(edge)),
      ...editor.renderedDOM.queryAllByTestId(paddingControlHandleTestId(edge)),
    ])

    expect(paddingControls).toEqual([])
  })

  it('Padding resize handle is present for elements that are dimensioned and have children', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div data-uid='root'>
        <div
          data-uid='mydiv'
          data-testid='mydiv'
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 28,
            top: 28,
            width: 612,
            height: 461,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 22,
              height: 22,
            }}
            data-uid='002'
          />
        </div>
      </div>`),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 25,
      y: divBounds.y + 24,
    }

    mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    EdgePieces.forEach((edge) => {
      const paddingControlOuter = editor.renderedDOM.getByTestId(paddingControlTestId(edge))
      expect(paddingControlOuter).toBeTruthy()
      const paddingControlHandle = editor.renderedDOM.getByTestId(paddingControlHandleTestId(edge))
      expect(paddingControlHandle).toBeTruthy()
    })
  })

  it('Padding resize handle is present for elements that are dimensioned and have only text children', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div data-uid='root'>
        <div
          data-uid='mydiv'
          data-testid='mydiv'
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 28,
            top: 28,
            width: 612,
            height: 461,
          }}
        >
          Hello <br /> there!
        </div>
      </div>`),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 25,
      y: divBounds.y + 24,
    }

    mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    EdgePieces.forEach((edge) => {
      const paddingControlOuter = editor.renderedDOM.getByTestId(paddingControlTestId(edge))
      expect(paddingControlOuter).toBeTruthy()
      const paddingControlHandle = editor.renderedDOM.getByTestId(paddingControlHandleTestId(edge))
      expect(paddingControlHandle).toBeTruthy()
    })
  })

  it("padding controls don't show up for elements that are smaller than 40px", async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
      data-testid='mydiv'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 28,
        top: 28,
        width: 39,
        height: 39,
      }}
      data-uid='24a'
    />`),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 1,
      y: divBounds.y + 1,
    }

    mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    const paddingControls = EdgePieces.flatMap((edge) => [
      ...editor.renderedDOM.queryAllByTestId(paddingControlTestId(edge)),
      ...editor.renderedDOM.queryAllByTestId(paddingControlHandleTestId(edge)),
    ])

    expect(paddingControls).toEqual([])
  })

  it('Padding resize handles are present and visible after the timeout', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues(
        paddingToPaddingString({
          paddingTop: unitlessCSSNumberWithRenderedValue(22),
          paddingBottom: unitlessCSSNumberWithRenderedValue(33),
          paddingLeft: unitlessCSSNumberWithRenderedValue(44),
          paddingRight: unitlessCSSNumberWithRenderedValue(55),
        }),
      ),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 5,
      y: divBounds.y + 4,
    }

    mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    const paddingResizeControlContainerBounds = div.getBoundingClientRect()
    const paddingResizeControlContainerCorner = {
      x: paddingResizeControlContainerBounds.x + 5,
      y: paddingResizeControlContainerBounds.y + 4,
    }

    EdgePieces.forEach((edge) => {
      const paddingControlHandle = editor.renderedDOM.getByTestId(paddingControlHandleTestId(edge))
      expect(paddingControlHandle).toBeTruthy()
      expect(paddingControlHandle.style.opacity).toEqual('0')
    })

    mouseMoveToPoint(canvasControlsLayer, paddingResizeControlContainerCorner)

    await wait(PaddingResizeControlHoverTimeout + 1)

    EdgePieces.forEach((edge) => {
      const paddingControlOuter = editor.renderedDOM.getByTestId(paddingControlTestId(edge))
      expect(paddingControlOuter).toBeTruthy()

      const paddingControlHandle = editor.renderedDOM.getByTestId(paddingControlHandleTestId(edge))
      expect(paddingControlHandle).toBeTruthy()
      expect(paddingControlHandle.style.opacity).toEqual('1')
    })
  })

  it('Padding resize handles are present', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues(
        paddingToPaddingString({
          paddingTop: unitlessCSSNumberWithRenderedValue(22),
          paddingBottom: unitlessCSSNumberWithRenderedValue(33),
          paddingLeft: unitlessCSSNumberWithRenderedValue(44),
          paddingRight: unitlessCSSNumberWithRenderedValue(55),
        }),
      ),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 5,
      y: divBounds.y + 4,
    }

    mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    EdgePieces.forEach((edge) => {
      const paddingControlOuter = editor.renderedDOM.getByTestId(paddingControlTestId(edge))
      expect(paddingControlOuter).toBeTruthy()
      const paddingControlHandle = editor.renderedDOM.getByTestId(paddingControlHandleTestId(edge))
      expect(paddingControlHandle).toBeTruthy()
    })
  })

  it('The set padding strategy is not in the picker when unrelated interactions are active', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues(
        paddingToPaddingString({
          paddingTop: unitlessCSSNumberWithRenderedValue(22),
          paddingBottom: unitlessCSSNumberWithRenderedValue(33),
          paddingLeft: unitlessCSSNumberWithRenderedValue(44),
          paddingRight: unitlessCSSNumberWithRenderedValue(55),
        }),
      ),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const { x, y, width, height } = div.getBoundingClientRect()
    const divCenter = {
      x: x + width / 2,
      y: y + height / 2,
    }

    // Start a drag that will move the element
    mouseDownAtPoint(canvasControlsLayer, divCenter)
    mouseMoveToPoint(
      canvasControlsLayer,
      { x: divCenter.x + 100, y: divCenter.y + 100 },
      { eventOptions: { buttons: 1 } },
    )

    // Check that the strategy picker does not include the set padding strategy
    const applicableStrategies = editor.getEditorState().strategyState.sortedApplicableStrategies
    expect(applicableStrategies).not.toBeNull()
    expect(applicableStrategies!.length).toBeGreaterThan(0)
    expect(applicableStrategies!.find((s) => s.name === SetPaddingStrategyName)).toBeUndefined()
  })

  it('Adjust padding values when padding is specified in `em` units', async () => {
    const dragDelta = 100
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues('2em 1em 3em 2em'),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDelta, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithStringPaddingValues('8.3em 1em 3em 2em'),
    )
  })

  it('padding can be set to zero and then resized to a non-zero value', async () => {
    const dragDeltaToZero = -100
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues('2em 1em 3em 2em'),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    const dragDeltaFromZero = 100
    await testPaddingResizeForEdge(editor, dragDeltaFromZero, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithStringPaddingValues(`${dragDeltaFromZero}px 1em 3em 2em`),
    )
  })

  it('padding can be set to zero without removing the property', async () => {
    const dragDeltaToZero = -11
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues('11px 11px 11px 11px'),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithStringPaddingValues(`0px 11px 11px 11px`),
    )
  })

  it('padding can be set below zero, but above the remove threshold without removing the property', async () => {
    const originalPadding = 11
    const dragDelta = -originalPadding - Math.abs(PaddingTearThreshold) / 2
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues(`${originalPadding}px 11px 11px 11px`),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDelta, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithStringPaddingValues(`0px 11px 11px 11px`),
    )
  })

  it('paddingLeft can be removed by dragging', async () => {
    const dragDeltaToZero = -100
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues('10px 10px 10px 10px'),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'left', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingTop: `'10px'`,
        paddingBottom: `'10px'`,
        paddingRight: `'10px'`,
      }),
    )
  })

  it('paddingTop can be removed by dragging', async () => {
    const dragDeltaToZero = -100
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues('10px 10px 10px 10px'),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingBottom: `'10px'`,
        paddingLeft: `'10px'`,
        paddingRight: `'10px'`,
      }),
    )
  })

  it('paddingRight can be removed by dragging', async () => {
    const dragDeltaToZero = -100
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues('10px 10px 10px 10px'),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'right', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingTop: `'10px'`,
        paddingBottom: `'10px'`,
        paddingLeft: `'10px'`,
      }),
    )
  })

  it('paddingBottom can be removed by dragging', async () => {
    const dragDeltaToZero = -100
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues('10px 10px 10px 10px'),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'bottom', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingTop: `'10px'`,
        paddingLeft: `'10px'`,
        paddingRight: `'10px'`,
      }),
    )
  })

  it('paddingTop can be readded by dragging', async () => {
    const dragDeltaToZero = 10
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingBottom: `'10px'`,
        paddingLeft: `'10px'`,
        paddingRight: `'10px'`,
      }),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithStringPaddingValues('10px 10px 10px 10px'),
    )
  })

  it('paddingTop can be removed by dragging when longhand props are present', async () => {
    const dragDeltaToZero = -100
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingTop: `'10px'`,
        paddingLeft: `'10px'`,
        paddingRight: `'10px'`,
      }),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingLeft: `'10px'`,
        paddingRight: `'10px'`,
      }),
    )
  })

  it('paddingTop can be readded when only two longhands are present', async () => {
    const dragDeltaToZero = 10
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingLeft: `'10px'`,
        paddingRight: `'10px'`,
      }),
      'await-first-dom-report',
    )

    await testPaddingResizeForEdge(editor, dragDeltaToZero, 'top', 'precise')
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithLongHandPaddingValues({
        paddingTop: `'10px'`,
        paddingLeft: `'10px'`,
        paddingRight: `'10px'`,
      }),
    )
  })

  describe('Adjust multiple edges simultaneously', () => {
    it('adjust along the horizontal cross-axis', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithLongHandPaddingValues({
          paddingBottom: `'10px'`,
          paddingTop: `'10px'`,
          paddingLeft: `'10px'`,
          paddingRight: `'10px'`,
        }),
        'await-first-dom-report',
      )

      await testPaddingResizeForEdge(editor, 10, 'left', 'precise', 'cross-axis')
      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithStringPaddingValues(
          paddingToPaddingString({
            paddingBottom: cssNumberWithRenderedValue(cssNumber(10, 'px'), 10),
            paddingTop: cssNumberWithRenderedValue(cssNumber(10, 'px'), 10),
            paddingLeft: cssNumberWithRenderedValue(cssNumber(20, 'px'), 20),
            paddingRight: cssNumberWithRenderedValue(cssNumber(20, 'px'), 20),
          }),
        ),
      )
    })

    it('adjust along the vertical cross-axis', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithLongHandPaddingValues({
          paddingBottom: `'10px'`,
          paddingTop: `'10px'`,
          paddingLeft: `'10px'`,
          paddingRight: `'10px'`,
        }),
        'await-first-dom-report',
      )

      await testPaddingResizeForEdge(editor, 10, 'top', 'precise', 'cross-axis')
      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithStringPaddingValues(
          paddingToPaddingString({
            paddingBottom: cssNumberWithRenderedValue(cssNumber(20, 'px'), 20),
            paddingTop: cssNumberWithRenderedValue(cssNumber(20, 'px'), 20),
            paddingLeft: cssNumberWithRenderedValue(cssNumber(10, 'px'), 10),
            paddingRight: cssNumberWithRenderedValue(cssNumber(10, 'px'), 10),
          }),
        ),
      )
    })

    it('adjust all 4 paddings at the same time', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithLongHandPaddingValues({
          paddingBottom: `'10px'`,
          paddingTop: `'10px'`,
          paddingLeft: `'10px'`,
          paddingRight: `'10px'`,
        }),
        'await-first-dom-report',
      )

      await testPaddingResizeForEdge(editor, 10, 'top', 'precise', 'all')
      await editor.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithStringPaddingValues(
          paddingToPaddingString({
            paddingBottom: cssNumberWithRenderedValue(cssNumber(20, 'px'), 20),
            paddingTop: cssNumberWithRenderedValue(cssNumber(20, 'px'), 20),
            paddingLeft: cssNumberWithRenderedValue(cssNumber(20, 'px'), 20),
            paddingRight: cssNumberWithRenderedValue(cssNumber(20, 'px'), 20),
          }),
        ),
      )
    })
  })

  describe('Padding controls on component instances', () => {
    it('controls are shown if padding is specified on the component instance', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithComponentThatDefinesPaddingInternally({
          internalPadding: '10px',
          externalPadding: '20px',
        }),
        'await-first-dom-report',
      )

      clickOnMyDiv(editor)
      EdgePieces.forEach((edge) => {
        const paddingControlOuter = editor.renderedDOM.getByTestId(paddingControlTestId(edge))
        expect(paddingControlOuter).toBeTruthy()
        const paddingControlHandle = editor.renderedDOM.getByTestId(
          paddingControlHandleTestId(edge),
        )
        expect(paddingControlHandle).toBeTruthy()
      })
    })

    it("controls are shown if padding is NOT specified on the component instance and instance doesn't have computed padding", async () => {
      const editor = await renderTestEditorWithCode(
        projectWithComponentThatDefinesPaddingInternally({}),
        'await-first-dom-report',
      )

      clickOnMyDiv(editor)
      EdgePieces.forEach((edge) => {
        const paddingControlOuter = editor.renderedDOM.getByTestId(paddingControlTestId(edge))
        expect(paddingControlOuter).toBeTruthy()
        const paddingControlHandle = editor.renderedDOM.getByTestId(
          paddingControlHandleTestId(edge),
        )
        expect(paddingControlHandle).toBeTruthy()
      })
    })

    it('controls are NOT shown if padding is NOT specified on the component instance and instance has computed padding', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithComponentThatDefinesPaddingInternally({
          internalPadding: '5px',
        }),
        'await-first-dom-report',
      )

      clickOnMyDiv(editor)
      const paddingControls = EdgePieces.flatMap((edge) => [
        ...editor.renderedDOM.queryAllByTestId(paddingControlTestId(edge)),
        ...editor.renderedDOM.queryAllByTestId(paddingControlHandleTestId(edge)),
      ])

      expect(paddingControls).toEqual([])
    })
  })

  describe('Adjusting individual padding values, precise', () => {
    // the expect is in `testAdjustIndividualPaddingValue`
    // eslint-disable-next-line jest/expect-expect
    it('top', async () => testAdjustIndividualPaddingValue('top', 'precise'))
    // eslint-disable-next-line jest/expect-expect
    it('bottom', async () => testAdjustIndividualPaddingValue('top', 'precise'))
    // eslint-disable-next-line jest/expect-expect
    it('left', async () => testAdjustIndividualPaddingValue('top', 'precise'))
    // eslint-disable-next-line jest/expect-expect
    it('right', async () => testAdjustIndividualPaddingValue('top', 'precise'))
  })

  describe('Adjusting individual padding values, coarse', () => {
    // the expect is in `testAdjustIndividualPaddingValue`
    // eslint-disable-next-line jest/expect-expect
    it('top', async () => testAdjustIndividualPaddingValue('top', 'coarse'))
    // eslint-disable-next-line jest/expect-expect
    it('bottom', async () => testAdjustIndividualPaddingValue('top', 'coarse'))
    // eslint-disable-next-line jest/expect-expect
    it('left', async () => testAdjustIndividualPaddingValue('top', 'coarse'))
    // eslint-disable-next-line jest/expect-expect
    it('right', async () => testAdjustIndividualPaddingValue('top', 'coarse'))
  })
})

async function testAdjustIndividualPaddingValue(edge: EdgePiece, precision: AdjustPrecision) {
  const padding: CSSPaddingMeasurements = {
    paddingTop: unitlessCSSNumberWithRenderedValue(22),
    paddingBottom: unitlessCSSNumberWithRenderedValue(33),
    paddingLeft: unitlessCSSNumberWithRenderedValue(44),
    paddingRight: unitlessCSSNumberWithRenderedValue(55),
  }
  const dragDelta = 12
  const editor = await renderTestEditorWithCode(
    makeTestProjectCodeWithStringPaddingValues(paddingToPaddingString(padding)),
    'await-first-dom-report',
  )

  const defaultPadding: CSSPaddingMappedValues<number> = {
    paddingTop: 0,
    paddingRight: 0,
    paddingBottom: 0,
    paddingLeft: 0,
  }

  await testPaddingResizeForEdge(editor, dragDelta, edge, precision)
  await editor.getDispatchFollowUpActionsFinished()
  expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
    makeTestProjectCodeWithStringPaddingValues(
      paddingToPaddingString(
        combinePaddings(
          defaultPadding,
          offsetPaddingByEdge(paddingPropForEdge(edge), dragDelta, padding, precision),
        ),
      ),
    ),
  )
}

async function testPaddingResizeForEdge(
  editor: EditorRenderResult,
  delta: number,
  edge: EdgePiece,
  precision: AdjustPrecision,
  mode: PaddingAdjustMode = 'individual',
) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 5,
    y: divBounds.y + 4,
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

  const paddingControl = editor.renderedDOM.getByTestId(paddingControlHandleTestId(edge))
  const paddingControlBounds = paddingControl.getBoundingClientRect()

  const paddingControlCenter = {
    x: Math.floor(paddingControlBounds.x + paddingControlBounds.width / 2),
    y: Math.floor(paddingControlBounds.y + paddingControlBounds.height / 2),
  }
  const endPoint = offsetPointByEdge(edge, delta, paddingControlCenter)

  const modifiers: Modifiers = {
    ctrl: false,
    cmd: precision === 'coarse',
    alt: mode === 'cross-axis' || mode === 'all',
    shift: mode === 'all',
  }
  mouseDragFromPointToPoint(paddingControl, paddingControlCenter, endPoint, { modifiers })
  await editor.getDispatchFollowUpActionsFinished()
}

interface Point {
  x: number
  y: number
}

function offsetPointByEdge(edge: EdgePiece, delta: number, point: Point): Point {
  const { x, y } = point
  switch (edge) {
    case 'bottom':
      return { x: x, y: y - delta }
    case 'top':
      return { x: x, y: y + delta }
    case 'left':
      return { x: x + delta, y: y }
    case 'right':
      return { x: x - delta, y: y }
    default:
      assertNever(edge)
  }
}

function makeTestProjectCodeWithStringPaddingValues(padding: string): string {
  return makeTestProjectCodeWithSnippet(`
    <div data-uid='root'>
      <div
        data-uid='mydiv'
        data-testid='mydiv'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 28,
          top: 28,
          width: 612,
          height: 461,
          padding: '${padding}',
        }}
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: '100%',
            height: '100%',
          }}
          data-uid='002'
        />
      </div>
    </div>`)
}

function makeTestProjectCodeWithLongHandPaddingValues(
  padding: Partial<CSSPaddingMappedValues<string>>,
): string {
  return makeTestProjectCodeWithSnippet(`
    <div data-uid='root'>
      <div
        data-uid='mydiv'
        data-testid='mydiv'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 28,
          top: 28,
          width: 612,
          height: 461,
          ${formatPaddingLonghandValues(padding)}
        }}
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: '100%',
            height: '100%',
          }}
          data-uid='002'
        />
      </div>
    </div>`)
}

function formatPaddingLonghandValues(padding: Partial<CSSPaddingMappedValues<string>>): string {
  return [
    padding.paddingTop == null ? null : `        paddingTop: ${padding.paddingTop},`,
    padding.paddingBottom == null ? null : `        paddingBottom: ${padding.paddingBottom},`,
    padding.paddingLeft == null ? null : `        paddingLeft: ${padding.paddingLeft},`,
    padding.paddingRight == null ? null : `        paddingRight: ${padding.paddingRight},`,
  ]
    .filter((s) => s != null)
    .join('\n')
}

function clickOnMyDiv(editor: EditorRenderResult) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 25,
    y: divBounds.y + 24,
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })
}
interface HorribleComponentProps {
  internalPadding?: string
  externalPadding?: string
}

function projectWithComponentThatDefinesPaddingInternally(props: HorribleComponentProps): string {
  return `import * as React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
  
  const HorribleComponent = (props) => {
    return (
      <div
        data-testid='mydiv'
        style={{
          width: '300px',
          height: '400px',
          backgroundColor: 'green',
          ${props.internalPadding != null ? `padding: '${props.internalPadding}',` : ''}
          ...props.style,
        }}
        data-uid='8bc'
      />
    )
  }
  
  export var storyboard = (
    <Storyboard data-uid='0cd'>
      <HorribleComponent
        style={{
          position: 'absolute',
          left: 420,
          top: 420,
          ${props.externalPadding != null ? `padding: '${props.externalPadding}',` : ''}
        }}
        data-uid='ca3'
      />
    </Storyboard>
  )
  `
}
