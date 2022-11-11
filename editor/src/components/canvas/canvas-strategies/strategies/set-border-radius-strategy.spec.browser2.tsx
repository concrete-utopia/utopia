import { canvasVector, CanvasVector, size } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { cmdModifier } from '../../../../utils/modifiers'
import { BorderRadiusCorner, BorderRadiusCorners } from '../../border-radius-utis'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { CircularHandleTestId } from '../../controls/select-mode/border-radius-control'
import { mouseClickAtPoint, mouseDragFromPointToPoint } from '../../event-helpers.test-utils'
import {
  renderTestEditorWithCode,
  makeTestProjectCodeWithSnippet,
  EditorRenderResult,
  getPrintedUiJsCode,
} from '../../ui-jsx.test-utils'

describe('set border radius strategy', () => {
  it('border radius controls show up for elements that have border radius set', async () => {
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: 22`),
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

    const paddingControls = BorderRadiusCorners.flatMap((corner) =>
      editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
    )

    expect(paddingControls.length).toEqual(4)
  })

  it("border radius controls don't show up for elements that have don't border radius set", async () => {
    const editor = await renderTestEditorWithCode(codeForDragTest(``), 'await-first-dom-report')

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 50,
      y: divBounds.y + 40,
    }

    mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    const paddingControls = BorderRadiusCorners.flatMap((corner) =>
      editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
    )

    expect(paddingControls).toEqual([])
  })

  it('can only adjust border radius to 50% at most', async () => {
    const { width, height } = size(600, 400)
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '4px'`),
      'await-first-dom-report',
    )

    const expectedBorderRadius = Math.min(width, height) / 2

    await doDragTest(editor, 'tl', 400)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '${expectedBorderRadius}px'`),
    )
  })

  it('can only adjust border radius to 0 at min', async () => {
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '4px'`),
      'await-first-dom-report',
    )
    await doDragTest(editor, 'tl', -10)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '0px'`),
    )
  })

  describe('adjust padding via handles', () => {
    it('top left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tl', 10)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px'`),
      )
    })

    it('top right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tr', 10)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px'`),
      )
    })

    it('bottom left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'bl', 10)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px'`),
      )
    })

    it('bottom right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'br', 10)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px'`),
      )
    })
  })
})

function codeForDragTest(borderRadius: string): string {
  return makeTestProjectCodeWithSnippet(`<div
    data-testid='mydiv'
    style={{
      backgroundColor: '#0091FFAA',
      position: 'absolute',
      left: 28,
      top: 28,
      width: 600,
      height: 400,
      ${borderRadius}
    }}
    data-uid='24a'
  >
    <div
      style={{
        backgroundColor: '#0091FFAA',
        width: 22,
        height: 22,
      }}
      data-uid='002'
    />
  </div>`)
}

async function doDragTest(editor: EditorRenderResult, corner: BorderRadiusCorner, offset: number) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

  const borderRadiusControl = editor.renderedDOM.getByTestId(CircularHandleTestId(corner))
  const borderRadiusControlBounds = borderRadiusControl.getBoundingClientRect()

  const center = {
    x: Math.floor(borderRadiusControlBounds.x + borderRadiusControlBounds.width / 2),
    y: Math.floor(borderRadiusControlBounds.y + borderRadiusControlBounds.height / 2),
  }

  const dragDelta = dragDeltaFromEdgePosition(corner, offset)

  mouseDragFromPointToPoint(borderRadiusControl, center, {
    x: center.x + dragDelta.x,
    y: center.y + dragDelta.y,
  })
  await editor.getDispatchFollowUpActionsFinished()
}

function dragDeltaFromEdgePosition(corner: BorderRadiusCorner, offset: number): CanvasVector {
  switch (corner) {
    case 'tl':
      return canvasVector({ x: offset, y: offset })
    case 'tr':
      return canvasVector({ x: -offset, y: offset })
    case 'bl':
      return canvasVector({ x: offset, y: -offset })
    case 'br':
      return canvasVector({ x: -offset, y: -offset })
    default:
      assertNever(corner)
  }
}
