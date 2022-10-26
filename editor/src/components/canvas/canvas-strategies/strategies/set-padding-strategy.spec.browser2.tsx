import { assertNever } from '../../../../core/shared/utils'
import { cmdModifier } from '../../../../utils/modifiers'
import { EdgePiece } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  paddingControlHandleTestId,
  paddingControlTestId,
} from '../../controls/select-mode/padding-resize-control'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import { offsetPaddingByEdge, paddingToPaddingString, SimpleCSSPadding } from '../../padding-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { SetPaddingStrategyName } from './set-padding-strategy'

const edgePieces: Array<EdgePiece> = ['top', 'bottom', 'left', 'right']

describe('Padding resize strategy', () => {
  it('Padding resize handle is not present for elements that have no padding set', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
      data-testid='mydiv'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 28,
        top: 28,
        width: 612,
        height: 461,
      }}
      data-uid='24a'
    >
      <div
        style={{
          backgroundColor: '#0091FFAA',
          width: '100%',
          height: '100%',
        }}
        data-uid='002'
      />
    </div>`),
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

    const paddingControls = edgePieces.flatMap((edge) =>
      editor.renderedDOM.queryAllByTestId(paddingControlTestId(edge)),
    )

    expect(paddingControls).toEqual([])
  })

  it('Padding resize is present', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithStringPaddingValues(
        paddingToPaddingString({
          paddingTop: 22,
          paddingBottom: 33,
          paddingLeft: 44,
          paddingRight: 55,
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

    edgePieces.forEach((edge) => {
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
          paddingTop: 22,
          paddingBottom: 33,
          paddingLeft: 44,
          paddingRight: 55,
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

  describe('Adjusting individual padding values', () => {
    // the expect is in `testAdjustIndividualPaddingValue`
    // eslint-disable-next-line jest/expect-expect
    it('top', async () => testAdjustIndividualPaddingValue('top'))
    // eslint-disable-next-line jest/expect-expect
    it('bottom', async () => testAdjustIndividualPaddingValue('top'))
    // eslint-disable-next-line jest/expect-expect
    it('left', async () => testAdjustIndividualPaddingValue('top'))
    // eslint-disable-next-line jest/expect-expect
    it('right', async () => testAdjustIndividualPaddingValue('top'))
  })
})

async function testAdjustIndividualPaddingValue(edge: EdgePiece) {
  const padding: SimpleCSSPadding = {
    paddingTop: 22,
    paddingBottom: 33,
    paddingLeft: 44,
    paddingRight: 55,
  }
  const dragDelta = 12
  const editor = await renderTestEditorWithCode(
    makeTestProjectCodeWithStringPaddingValues(paddingToPaddingString(padding)),
    'await-first-dom-report',
  )

  await testPaddingResizeForEdge(editor, dragDelta, edge)
  await editor.getDispatchFollowUpActionsFinished()
  expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
    makeTestProjectCodeWithStringPaddingValues(
      paddingToPaddingString(offsetPaddingByEdge(edge, dragDelta, padding)),
    ),
  )
}

async function testPaddingResizeForEdge(
  editor: EditorRenderResult,
  delta: number,
  edge: EdgePiece,
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
  const bounds = paddingControl.getBoundingClientRect()

  const center = {
    x: Math.floor(bounds.x + bounds.width / 2),
    y: Math.floor(bounds.y + bounds.height / 2),
  }
  const endPoint = offsetPointByEdge(edge, delta, center)

  mouseDragFromPointToPoint(paddingControl, center, endPoint)
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
  return makeTestProjectCodeWithSnippet(`<div
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
      data-uid='24a'
    >
      <div
        style={{
          backgroundColor: '#0091FFAA',
          width: '100%',
          height: '100%',
        }}
        data-uid='002'
      />
    </div>`)
}
