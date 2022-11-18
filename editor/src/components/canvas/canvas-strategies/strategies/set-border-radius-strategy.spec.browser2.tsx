import {
  canvasVector,
  CanvasVector,
  size,
  windowPoint,
  WindowPoint,
} from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { cmdModifier, emptyModifiers, Modifiers } from '../../../../utils/modifiers'
import { wait } from '../../../../utils/utils.test-utils'
import { BorderRadiusCorner, BorderRadiusCorners } from '../../border-radius-control-utils'
import { EdgePosition, EdgePositionBottomRight } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { CircularHandleTestId } from '../../controls/select-mode/border-radius-control'
import {
  mouseClickAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointWithDelta,
  mouseEnterAtPoint,
} from '../../event-helpers.test-utils'
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

  it('only explicitely defined border controls show up for component instances', async () => {
    const editor = await renderTestEditorWithCode(
      codeWithComponentInstance(`borderTopLeftRadius: '30px',
                                 borderTopRightRadius: '30px',`),
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

    expect(paddingControls.length).toEqual(2)
  })

  it('can handle 4-value syntax', async () => {
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '4px 5px 6px 7px'`),
      'await-first-dom-report',
    )

    await doDragTest(editor, 'tl', 10, emptyModifiers)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '4px 5px 6px 7px',
                       borderTopLeftRadius: '14px',`),
    )
  })

  it('can only adjust border radius to 50% at most', async () => {
    const { width, height } = size(600, 400)
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '4px'`),
      'await-first-dom-report',
    )

    const expectedBorderRadius = Math.min(width, height) / 2

    await doDragTest(editor, 'tl', 400, emptyModifiers)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '${expectedBorderRadius}px'`),
    )
  })

  it('can only adjust border radius to 0 at min', async () => {
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '4px'`),
      'await-first-dom-report',
    )
    await doDragTest(editor, 'tl', -10, emptyModifiers)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '0px'`),
    )
  })

  it('can resize border radius on element that has larger than 50% border radius', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div
    data-testid='mydiv'
    style={{
      backgroundColor: '#0091FFAA',
      position: 'absolute',
      left: 28,
      top: 28,
      width: 600,
      height: 400,
      borderRadius: '4px',
    }}
    data-uid='24a'
  />`,
      ),
      'await-first-dom-report',
    )
    await doDragTest(editor, 'tl', 400, emptyModifiers)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '200px'`),
    )

    resizeElement(
      editor,
      windowPoint({ x: -300, y: -300 }),
      EdgePositionBottomRight,
      emptyModifiers,
    )

    await doDragTest(editor, 'tl', -5, emptyModifiers)
    await doDragTest(editor, 'tl', -5, emptyModifiers)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div
    data-testid='mydiv'
    style={{
      backgroundColor: '#0091FFAA',
      position: 'absolute',
      left: 28,
      top: 28,
      width: 300,
      height: 100,
      borderRadius: '40px',
    }}
    data-uid='24a'
  />`,
      ),
    )
  })

  describe('adjust border radius via handles', () => {
    it('top left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tl', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px'`),
      )
    })

    it('top right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tr', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px'`),
      )
    })

    it('bottom left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'bl', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px'`),
      )
    })

    it('bottom right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'br', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px'`),
      )
    })
  })

  describe('adjust border radius via handles, individually', () => {
    it('top left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px',`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tl', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '4px',
                         borderTopLeftRadius: '14px',`),
      )
    })

    it('top right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tr', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '4px',
                         borderTopRightRadius: '14px',`),
      )
    })

    it('bottom left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'bl', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '4px',
                         borderBottomLeftRadius: '14px',`),
      )
    })

    it('bottom right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '4px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'br', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '4px',
                         borderBottomRightRadius: '14px',`),
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
  />`)
}

function codeWithComponentInstance(borderRadius: string): string {
  return makeTestProjectCodeWithSnippet(`<View
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
  />`)
}

async function doDragTest(
  editor: EditorRenderResult,
  corner: BorderRadiusCorner,
  offset: number,
  modifiers: Modifiers,
) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + Math.floor(divBounds.width / 2),
    y: divBounds.y + Math.floor(divBounds.height / 2),
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

  const borderRadiusControl = editor.renderedDOM.getByTestId(CircularHandleTestId(corner))
  const borderRadiusControlBounds = borderRadiusControl.getBoundingClientRect()

  const center = {
    x: Math.floor(borderRadiusControlBounds.x + borderRadiusControlBounds.width / 2),
    y: Math.floor(borderRadiusControlBounds.y + borderRadiusControlBounds.height / 2),
  }

  const dragDelta = dragDeltaFromEdgePosition(corner, offset)

  mouseEnterAtPoint(borderRadiusControl, divCorner)

  mouseDragFromPointToPoint(
    borderRadiusControl,
    center,
    {
      x: center.x + dragDelta.x,
      y: center.y + dragDelta.y,
    },
    { modifiers },
  )
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

function resizeElement(
  renderResult: EditorRenderResult,
  dragDelta: WindowPoint,
  edgePosition: EdgePosition,
  modifiers: Modifiers,
) {
  const canvasControl = renderResult.renderedDOM.getByTestId(
    `resize-control-${edgePosition.x}-${edgePosition.y}`,
  )

  const resizeCornerBounds = canvasControl.getBoundingClientRect()
  const startPoint = windowPoint({
    x: resizeCornerBounds.x + 2,
    y: resizeCornerBounds.y + 2,
  })

  mouseDragFromPointWithDelta(canvasControl, startPoint, dragDelta, { modifiers: modifiers })
}
