import { assertNever } from '../../../../core/shared/utils'
import { cmdModifier, shiftModifier } from '../../../../utils/modifiers'
import { wait } from '../../../../utils/utils.test-utils'
import { EdgePiece } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  AdjustPrecision,
  unitlessCSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import {
  paddingControlHandleTestId,
  paddingControlTestId,
  PaddingResizeControlContainerTestId,
  PaddingResizeControlHoverTimeout,
} from '../../controls/select-mode/padding-resize-control'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseEnterAtPoint,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import {
  offsetPaddingByEdge,
  paddingToPaddingString,
  CSSPaddingMeasurements,
  CSSPaddingMappedValues,
  combinePaddings,
  paddingPropForEdge,
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
              backgroundColor: '#0091FFAA',
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
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 28,
            top: 28,
            width: 612,
            height: 461,
          }}
        >
          <div
            style={{
              backgroundColor: '#0091FFAA',
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

    const paddingResizeControlContainer = editor.renderedDOM.getByTestId(
      PaddingResizeControlContainerTestId,
    )
    const paddingResizeControlContainerBounds = div.getBoundingClientRect()
    const paddingResizeControlContainerCorner = {
      x: paddingResizeControlContainerBounds.x + 5,
      y: paddingResizeControlContainerBounds.y + 4,
    }

    mouseEnterAtPoint(paddingResizeControlContainer, paddingResizeControlContainerCorner)

    await wait(PaddingResizeControlHoverTimeout + 1)

    EdgePieces.forEach((edge) => {
      const paddingControlOuter = editor.renderedDOM.getByTestId(paddingControlTestId(edge))
      expect(paddingControlOuter).toBeTruthy()
      const paddingControlHandle = editor.renderedDOM.getByTestId(paddingControlHandleTestId(edge))
      expect(paddingControlHandle).toBeTruthy()
      expect(paddingControlHandle.style.visibility).toEqual('visible')
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

  const modifiers = precision === 'coarse' ? shiftModifier : undefined
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
          backgroundColor: '#0091FFAA',
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
            backgroundColor: '#0091FFAA',
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
          backgroundColor: '#0091FFAA',
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
            backgroundColor: '#0091FFAA',
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
