import { fromString } from '../../../../core/shared/element-path'
import type { CanvasVector, Size, WindowPoint } from '../../../../core/shared/math-utils'
import { canvasVector, size, windowPoint } from '../../../../core/shared/math-utils'
import { assertNever } from '../../../../core/shared/utils'
import { TailwindConfigPath } from '../../../../core/tailwind/tailwind-config'
import { createModifiedProject } from '../../../../sample-projects/sample-project-utils.test-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier, emptyModifiers } from '../../../../utils/modifiers'
import {
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
  wait,
} from '../../../../utils/utils.test-utils'
import { selectComponents, setFocusedElement } from '../../../editor/actions/action-creators'
import { StoryboardFilePath } from '../../../editor/store/editor-state'
import type { BorderRadiusCorner } from '../../border-radius-control-utils'
import { BorderRadiusCorners } from '../../border-radius-control-utils'
import type { EdgePosition } from '../../canvas-types'
import { EdgePositionBottomRight } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { CircularHandleTestId } from '../../controls/select-mode/border-radius-control'
import {
  mouseClickAtPoint,
  mouseDragFromPointToPoint,
  mouseDragFromPointWithDelta,
  mouseEnterAtPoint,
  mouseMoveToPoint,
} from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import {
  renderTestEditorWithCode,
  makeTestProjectCodeWithSnippet,
  getPrintedUiJsCode,
  renderTestEditorWithModel,
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

      const targetPath = fromString('Storyboard/Horrible:RootDiv')
      const selectedViews = [targetPath]
      await editor.dispatch(
        [
          setFocusedElement(fromString('Storyboard/Horrible')),
          selectComponents(selectedViews, false),
        ],
        true,
      )
      await mouseMoveToPoint(canvasControlsLayer, divCorner)

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
      codeForDragTest(`borderTopLeftRadius: '24px',
                       borderTopRightRadius: '15px',
                       borderBottomRightRadius: '16px',
                       borderBottomLeftRadius: '17px',
                       overflow: 'hidden'`),
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
      codeForDragTest(`borderRadius: '${expectedBorderRadius}px', overflow: 'hidden'`),
    )
  })

  it('can only adjust border radius to 0 at min', async () => {
    const editor = await renderTestEditorWithCode(
      codeForDragTest(`borderRadius: '14px'`),
      'await-first-dom-report',
    )
    await doDragTest(editor, 'tl', -20, emptyModifiers)
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      codeForDragTest(`borderRadius: '0px', overflow: 'hidden'`),
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
      codeForDragTest(`borderRadius: '200px', overflow: 'hidden'`),
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
              overflow: 'hidden',
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
      codeForDragTest(`borderRadius: '22px', overflow: 'hidden'`),
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
        codeForDragTest(`borderRadius: '24px', overflow: 'hidden'`),
      )
    })

    it('top right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tr', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '24px', overflow: 'hidden'`),
      )
    })

    it('bottom left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'bl', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '24px', overflow: 'hidden'`),
      )
    })

    it('bottom right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'br', 10, emptyModifiers)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderRadius: '24px', overflow: 'hidden'`),
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
        codeForDragTest(`borderTopLeftRadius: '24px',
                          borderTopRightRadius: '14px',
                          borderBottomRightRadius: '14px',
                          borderBottomLeftRadius: '14px',
                          overflow: 'hidden'`),
      )
    })

    it('top right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tr', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderTopLeftRadius: '14px',
                          borderTopRightRadius: '24px',
                          borderBottomRightRadius: '14px',
                          borderBottomLeftRadius: '14px',
                          overflow: 'hidden'`),
      )
    })

    it('bottom left', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'bl', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderTopLeftRadius: '14px',
                          borderTopRightRadius: '14px',
                          borderBottomRightRadius: '14px',
                          borderBottomLeftRadius: '24px',
                          overflow: 'hidden'`),
      )
    })

    it('bottom right', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px'`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'br', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`borderTopLeftRadius: '14px',
                          borderTopRightRadius: '14px',
                          borderBottomRightRadius: '24px',
                          borderBottomLeftRadius: '14px',
                          overflow: 'hidden'`),
      )
    })
  })

  describe('Overflow property handling', () => {
    it('does not overwrite existing overflow property', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px', overflow: 'visible',`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tl', 10, cmdModifier)
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        codeForDragTest(`overflow: 'visible',
                          borderTopLeftRadius: '24px',
                          borderTopRightRadius: '14px',
                          borderBottomRightRadius: '14px',
                          borderBottomLeftRadius: '14px'`),
      )
    })

    it('shows toast message', async () => {
      const editor = await renderTestEditorWithCode(
        codeForDragTest(`borderRadius: '14px',`),
        'await-first-dom-report',
      )
      await doDragTest(editor, 'tl', 10, cmdModifier)
      expect(editor.getEditorState().editor.toasts).toHaveLength(1)
      expect(editor.getEditorState().editor.toasts[0]).toEqual({
        id: 'property-added',
        level: 'NOTICE',
        message: 'Element now hides overflowing content',
        persistent: false,
      })
    })
  })

  describe('Tailwind', () => {
    setFeatureForBrowserTestsUseInDescribeBlockOnly('Tailwind', true)

    const TailwindProject = (classes: string) =>
      createModifiedProject({
        [StoryboardFilePath]: `
    import React from 'react'
    import { Scene, Storyboard } from 'utopia-api'
    export var storyboard = (
      <Storyboard data-uid='sb'>
        <Scene
          id='scene'
          commentId='scene'
          data-uid='scene'
          style={{
            width: 700,
            height: 759,
            position: 'absolute',
            left: 212,
            top: 128,
          }}
        >
          <div
            data-uid='mydiv'
            data-testid='mydiv'
            className='top-28 left-28 w-28 h-28 bg-black absolute ${classes}'
          />
        </Scene>
      </Storyboard>
    )
    
    `,
        [TailwindConfigPath]: `
        const TailwindConfig = { }
        export default TailwindConfig
    `,
        'app.css': `
        @tailwind base;
        @tailwind components;
        @tailwind utilities;`,
      })

    it('border radius controls show up for elements that have tailwind border radius set', async () => {
      const editor = await renderTestEditorWithModel(
        TailwindProject('rounded-[10px]'),
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [fromString('sb/scene/mydiv')])

      const borderRadiusControls = BorderRadiusCorners.flatMap((corner) =>
        editor.renderedDOM.queryAllByTestId(CircularHandleTestId(corner)),
      )

      expect(borderRadiusControls.length).toEqual(4)
    })

    describe('adjust border radius via handles', () => {
      it('top left', async () => {
        const editor = await renderTestEditorWithModel(
          TailwindProject('rounded-[10px]'),
          'await-first-dom-report',
        )
        await doDragTest(editor, 'tl', 10, emptyModifiers)
        await editor.getDispatchFollowUpActionsFinished()
        const div = editor.renderedDOM.getByTestId('mydiv')
        expect(div.className).toEqual(
          'top-28 left-28 w-28 h-28 bg-black absolute rounded-[22px] overflow-hidden',
        )
      })

      it('top right', async () => {
        const editor = await renderTestEditorWithModel(
          TailwindProject('rounded-[10px]'),
          'await-first-dom-report',
        )
        await doDragTest(editor, 'tr', 10, emptyModifiers)
        await editor.getDispatchFollowUpActionsFinished()
        const div = editor.renderedDOM.getByTestId('mydiv')
        expect(div.className).toEqual(
          'top-28 left-28 w-28 h-28 bg-black absolute rounded-[22px] overflow-hidden',
        )
      })

      it('bottom left', async () => {
        const editor = await renderTestEditorWithModel(
          TailwindProject('rounded-[10px]'),
          'await-first-dom-report',
        )
        await doDragTest(editor, 'tl', 10, emptyModifiers)
        await editor.getDispatchFollowUpActionsFinished()
        const div = editor.renderedDOM.getByTestId('mydiv')
        expect(div.className).toEqual(
          'top-28 left-28 w-28 h-28 bg-black absolute rounded-[22px] overflow-hidden',
        )
      })

      it('bottom right', async () => {
        const editor = await renderTestEditorWithModel(
          TailwindProject('rounded-[10px]'),
          'await-first-dom-report',
        )
        await doDragTest(editor, 'tl', 10, emptyModifiers)
        await editor.getDispatchFollowUpActionsFinished()
        const div = editor.renderedDOM.getByTestId('mydiv')
        expect(div.className).toEqual(
          'top-28 left-28 w-28 h-28 bg-black absolute rounded-[22px] overflow-hidden',
        )
      })
      describe('adjust border radius via handles, individually', () => {
        it('top left', async () => {
          const editor = await renderTestEditorWithModel(
            TailwindProject('rounded-[10px]'),
            'await-first-dom-report',
          )
          await doDragTest(editor, 'tl', 10, cmdModifier)
          await editor.getDispatchFollowUpActionsFinished()
          const div = editor.renderedDOM.getByTestId('mydiv')
          expect(div.className).toEqual(
            'top-28 left-28 w-28 h-28 bg-black absolute rounded-tl-[22px] rounded-tr-[10px] rounded-br-[10px] rounded-bl-[10px] overflow-hidden',
          )
        })

        it('top right', async () => {
          const editor = await renderTestEditorWithModel(
            TailwindProject('rounded-[10px]'),
            'await-first-dom-report',
          )
          await doDragTest(editor, 'tr', 10, cmdModifier)
          await editor.getDispatchFollowUpActionsFinished()
          const div = editor.renderedDOM.getByTestId('mydiv')
          expect(div.className).toEqual(
            'top-28 left-28 w-28 h-28 bg-black absolute rounded-tl-[10px] rounded-tr-[22px] rounded-br-[10px] rounded-bl-[10px] overflow-hidden',
          )
        })

        it('bottom left', async () => {
          const editor = await renderTestEditorWithModel(
            TailwindProject('rounded-[10px]'),
            'await-first-dom-report',
          )
          await doDragTest(editor, 'bl', 10, cmdModifier)
          await editor.getDispatchFollowUpActionsFinished()
          const div = editor.renderedDOM.getByTestId('mydiv')
          expect(div.className).toEqual(
            'top-28 left-28 w-28 h-28 bg-black absolute rounded-tl-[10px] rounded-tr-[10px] rounded-br-[10px] rounded-bl-[22px] overflow-hidden',
          )
        })

        it('bottom right', async () => {
          const editor = await renderTestEditorWithModel(
            TailwindProject('rounded-[10px]'),
            'await-first-dom-report',
          )
          await doDragTest(editor, 'br', 10, cmdModifier)
          await editor.getDispatchFollowUpActionsFinished()
          const div = editor.renderedDOM.getByTestId('mydiv')
          expect(div.className).toEqual(
            'top-28 left-28 w-28 h-28 bg-black absolute rounded-tl-[10px] rounded-tr-[10px] rounded-br-[22px] rounded-bl-[10px] overflow-hidden',
          )
        })
      })

      describe('Overflow property handling', () => {
        it('does not overwrite existing overflow property', async () => {
          const editor = await renderTestEditorWithModel(
            TailwindProject('rounded-[10px] overflow-visible'),
            'await-first-dom-report',
          )
          await doDragTest(editor, 'tl', 10, cmdModifier)
          await editor.getDispatchFollowUpActionsFinished()
          const div = editor.renderedDOM.getByTestId('mydiv')
          expect(div.className).toEqual(
            'top-28 left-28 w-28 h-28 bg-black absolute overflow-visible rounded-tl-[22px] rounded-tr-[10px] rounded-br-[10px] rounded-bl-[10px]',
          )
        })

        it('shows toast message', async () => {
          const editor = await renderTestEditorWithModel(
            TailwindProject('rounded-[10px]'),
            'await-first-dom-report',
          )
          await doDragTest(editor, 'tl', 10, cmdModifier)
          expect(editor.getEditorState().editor.toasts).toHaveLength(1)
          expect(editor.getEditorState().editor.toasts[0]).toEqual({
            id: 'property-added',
            level: 'NOTICE',
            message: 'Element now hides overflowing content',
            persistent: false,
          })
        })
      })

      it('can handle 4-value syntax', async () => {
        const editor = await renderTestEditorWithModel(
          TailwindProject(
            'rounded-tl-[14px] rounded-tr-[15px] rounded-br-[16px] rounded-bl-[17px] overflow-visible',
          ),
          'await-first-dom-report',
        )

        await doDragTest(editor, 'tl', 10, emptyModifiers)
        await editor.getDispatchFollowUpActionsFinished()
        const div = editor.renderedDOM.getByTestId('mydiv')
        expect(div.className).toEqual(
          'top-28 left-28 w-28 h-28 bg-black absolute rounded-tl-[24px] rounded-tr-[15px] rounded-br-[16px] rounded-bl-[17px] overflow-visible',
        )
      })

      // it('can remove tailwind padding', async () => {
      //   const editor = await renderTestEditorWithModel(
      //     TailwindProject('p-4'),
      //     'await-first-dom-report',
      //   )
      //   await selectComponentsForTest(editor, [EP.fromString('sb/scene/mydiv')])
      //   await testPaddingResizeForEdge(editor, -150, 'top', 'precise')
      //   await editor.getDispatchFollowUpActionsFinished()
      //   const div = editor.renderedDOM.getByTestId('mydiv')
      //   expect(div.className).toEqual('top-10 left-10 absolute flex flex-row pb-4 pl-4 pr-4')
      // })

      // it('can set tailwind padding longhand', async () => {
      //   const editor = await renderTestEditorWithModel(
      //     TailwindProject('pt-12'),
      //     'await-first-dom-report',
      //   )
      //   await selectComponentsForTest(editor, [EP.fromString('sb/scene/mydiv')])
      //   await testPaddingResizeForEdge(editor, 50, 'top', 'precise')
      //   await editor.getDispatchFollowUpActionsFinished()
      //   const div = editor.renderedDOM.getByTestId('mydiv')
      //   expect(div.className).toEqual('top-10 left-10 absolute flex flex-row pt-24')
      // })
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
