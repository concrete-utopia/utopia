import { assertNever } from '../../../../core/shared/utils'
import { cmdModifier } from '../../../../utils/modifiers'
import { EdgePiece } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  paddingControlHandleTestId,
  paddingControlTestId,
} from '../../controls/select-mode/padding-resize-control'
import { mouseClickAtPoint, mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import { offsetPaddingByEdge, paddingToPaddingString, SimpleCSSPadding } from '../../padding-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'

describe('Padding resize strategy', () => {
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

    const edgePieces: Array<EdgePiece> = ['top', 'bottom', 'left', 'right']
    edgePieces.forEach((edge) => {
      const paddingControlOuter = editor.renderedDOM.getByTestId(paddingControlTestId(edge))
      expect(paddingControlOuter).toBeTruthy()
      const paddingControlHandle = editor.renderedDOM.getByTestId(paddingControlHandleTestId(edge))
      expect(paddingControlHandle).toBeTruthy()
    })
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
