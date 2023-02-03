import { fromString } from '../../../../core/shared/element-path'
import {
  canvasVector,
  CanvasVector,
  Size,
  size,
  windowPoint,
  WindowPoint,
} from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { cmdModifier, emptyModifiers, Modifiers } from '../../../../utils/modifiers'
import { selectComponents } from '../../../editor/actions/action-creators'
import { BorderRadiusCorner, BorderRadiusCorners } from '../../border-radius-control-utils'
import { EdgePosition, EdgePositionBottomRight } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { CircularHandleTestId } from '../../controls/select-mode/border-radius-control'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
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

    await mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    const borderRadiusControls = BorderRadiusCorners.flatMap((corner) =>
      editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
    )

    expect(borderRadiusControls.length).toEqual(4)
  })

  it("border radius controls do show up for elements that have don't border radius set", async () => {
    const editor = await renderTestEditorWithCode(codeForDragTest(``), 'await-first-dom-report')

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 50,
      y: divBounds.y + 40,
    }

    await mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    const borderRadiusControls = BorderRadiusCorners.flatMap((corner) =>
      editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
    )

    expect(borderRadiusControls.length).toEqual(4)
  })

  it("border radius controls don't show up for elements that are smaller than 40px", async () => {
    const editor = await renderTestEditorWithCode(
      divWithDimensions({ width: 20, height: 20 }),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 1,
      y: divBounds.y + 1,
    }

    await mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    const paddingControls = BorderRadiusCorners.flatMap((corner) =>
      editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
    )

    expect(paddingControls).toEqual([])
  })

  describe('Border radius controls on component instances', () => {
    it('controls are shown if border radius is specified on the component instance', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithComponentThatDefinesBorderRadiusInternally({
          internalBorderRadius: '10px',
          externalBorderRadius: '20px',
        }),
        'await-first-dom-report',
      )

      await clickOnMyDiv(editor)
      const borderRadiusControls = BorderRadiusCorners.flatMap((corner) =>
        editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
      )

      expect(borderRadiusControls.length).toEqual(4)
    })

    it("controls are shown if border radius is NOT specified on the component instance and instance doesn't have computed border radius", async () => {
      const editor = await renderTestEditorWithCode(
        projectWithComponentThatDefinesBorderRadiusInternally({}),
        'await-first-dom-report',
      )

      await clickOnMyDiv(editor)
      const borderRadiusControls = BorderRadiusCorners.flatMap((corner) =>
        editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
      )

      expect(borderRadiusControls.length).toEqual(4)
    })

    it('controls are NOT shown if border radius is NOT specified on the component instance and instance has computed border radius', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithComponentThatDefinesBorderRadiusInternally({
          internalBorderRadius: '5px',
        }),
        'await-first-dom-report',
      )

      await clickOnMyDiv(editor)
      const borderRadiusControls = BorderRadiusCorners.flatMap((corner) =>
        editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
      )

      expect(borderRadiusControls).toEqual([])
    })

    it('controls are shown if the root element is selected', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithComponentThatDefinesBorderRadiusInternally({
          internalBorderRadius: '5px',
        }),
        'await-first-dom-report',
      )

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId('mydiv')
      const divBounds = div.getBoundingClientRect()
      const divCorner = {
        x: divBounds.x + Math.floor(divBounds.width / 2),
        y: divBounds.y + Math.floor(divBounds.height / 2),
      }

      const selectedViews = [fromString('Storyboard/Horrible:RootDiv')]
      await editor.dispatch([selectComponents(selectedViews, false)], true)

      await mouseDoubleClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

      const borderRadiusControls = BorderRadiusCorners.flatMap((corner) =>
        editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
      )

      expect(borderRadiusControls.length).toEqual(4)
    })
  })

  it('can handle 4-value syntax', async () => {
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '14px 15px 16px 17px'`),
      'await-first-dom-report',
    )

    await doDragTest(editor, 'tl', 10, emptyModifiers)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '14px 15px 16px 17px',
                       borderTopLeftRadius: '24px',`),
    )
  })

  it('can only adjust border radius to 50% at most', async () => {
    const { width, height } = size(600, 400)
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '24px'`),
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
      codeForDragTest(`borderRadius: '14px'`),
      'await-first-dom-report',
    )
    await doDragTest(editor, 'tl', -20, emptyModifiers)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '0px'`),
    )
  })

  it('can resize border radius on element that has larger than 50% border radius', async () => {
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
            width: 600,
            height: 400,
            borderRadius: '4px',
          }}
        />
      </div>`),
      'await-first-dom-report',
    )
    await doDragTest(editor, 'tl', 400, emptyModifiers)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '200px'`),
    )

    await resizeElement(
      editor,
      windowPoint({ x: -300, y: -300 }),
      EdgePositionBottomRight,
      emptyModifiers,
    )

    await doDragTest(editor, 'tl', -5, emptyModifiers)
    await doDragTest(editor, 'tl', -5, emptyModifiers)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
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
              width: 300,
              height: 100,
              borderRadius: '40px',
            }}
          />
        </div>`),
    )
  })

  it('when resize starts from below 12px, delta is applied as if border radius was 12px', async () => {
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '4px'`),
      'await-first-dom-report',
    )
    await doDragTest(editor, 'tl', 10, emptyModifiers)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '22px'`),
    )
  })

  describe('adjust border radius via handles', () => {
    it('top left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tl', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '24px'`),
      )
    })

    it('top right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tr', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '24px'`),
      )
    })

    it('bottom left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'bl', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '24px'`),
      )
    })

    it('bottom right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'br', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '24px'`),
      )
    })
  })

  describe('adjust border radius via handles, individually', () => {
    it('top left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px',`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tl', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px',
                         borderTopLeftRadius: '24px',`),
      )
    })

    it('top right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tr', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px',
                         borderTopRightRadius: '24px',`),
      )
    })

    it('bottom left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'bl', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px',
                         borderBottomLeftRadius: '24px',`),
      )
    })

    it('bottom right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'br', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '14px',
                         borderBottomRightRadius: '24px',`),
      )
    })
  })
})

function codeForDragTest(borderRadius: string): string {
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
          width: 600,
          height: 400,
          ${borderRadius}
        }}
      />
    </div>`)
}

function divWithDimensions(sizee: Size): string {
  return makeTestProjectCodeWithSnippet(`<div
    data-testid='mydiv'
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 28,
      top: 28,
      width: '${sizee.width}px',
      height: '${sizee.height}px',
      borderRadius: '5px',
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

  await mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

  const borderRadiusControl = editor.renderedDOM.getByTestId(CircularHandleTestId(corner))
  const borderRadiusControlBounds = borderRadiusControl.getBoundingClientRect()

  const center = {
    x: Math.floor(borderRadiusControlBounds.x + borderRadiusControlBounds.width / 2),
    y: Math.floor(borderRadiusControlBounds.y + borderRadiusControlBounds.height / 2),
  }

  const dragDelta = dragDeltaFromEdgePosition(corner, offset)

  await mouseEnterAtPoint(borderRadiusControl, divCorner)

  await mouseDragFromPointToPoint(
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

async function resizeElement(
  renderResult: EditorRenderResult,
  dragDelta: WindowPoint,
  edgePosition: EdgePosition,
  modifiers: Modifiers,
): Promise<void> {
  const canvasControl = renderResult.renderedDOM.getByTestId(
    `resize-control-${edgePosition.x}-${edgePosition.y}`,
  )

  const resizeCornerBounds = canvasControl.getBoundingClientRect()
  const startPoint = windowPoint({
    x: resizeCornerBounds.x + 2,
    y: resizeCornerBounds.y + 2,
  })

  await mouseDragFromPointWithDelta(canvasControl, startPoint, dragDelta, { modifiers: modifiers })
}

async function clickOnMyDiv(editor: EditorRenderResult) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 25,
    y: divBounds.y + 24,
  }

  await mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })
}
interface HorribleComponentProps {
  internalBorderRadius?: string
  externalBorderRadius?: string
}

function projectWithComponentThatDefinesBorderRadiusInternally(
  props: HorribleComponentProps,
): string {
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
            ${
              props.internalBorderRadius != null
                ? `borderRadius: '${props.internalBorderRadius}',`
                : ''
            }
            ...props.style,
          }}
          data-uid='RootDiv'
        />
      )
    }
    
    export var storyboard = (
      <Storyboard data-uid='Storyboard'>
        <HorribleComponent
          style={{
            position: 'absolute',
            left: 420,
            top: 420,
            ${
              props.externalBorderRadius != null
                ? `borderRadius: '${props.externalBorderRadius}',`
                : ''
            }
          }}
          data-uid='Horrible'
        />
      </Storyboard>
    )
    `
}
