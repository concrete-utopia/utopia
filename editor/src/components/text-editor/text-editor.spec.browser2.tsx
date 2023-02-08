import { expectSingleUndoStep, wait } from '../../utils/utils.test-utils'
import { altCmdModifier, cmdModifier, Modifiers, shiftCmdModifier } from '../../utils/modifiers'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDragFromPointToPoint,
  pressKey,
} from '../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { TextEditorSpanId } from './text-editor'
import { TextRelatedProperties } from 'src/core/properties/css-properties'

describe('Use the text editor', () => {
  it('Click to edit text', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

    await enterTextEditMode(editor)
    typeText(' Utopia')
    await expectSingleUndoStep(editor, async () => closeTextEditor())
    await editor.getDispatchFollowUpActionsFinished()

    expect(editor.getEditorState().editor.mode.type).toEqual('select')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >Hello Utopia</div>
          </Storyboard>
        )`),
    )
  })
  it('Add new text', async () => {
    const editor = await renderTestEditorWithCode(projectWithoutText, 'await-first-dom-report')

    await enterTextEditMode(editor)
    typeText('Utopia')
    await expectSingleUndoStep(editor, async () => closeTextEditor())
    await editor.getDispatchFollowUpActionsFinished()

    expect(editor.getEditorState().editor.mode.type).toEqual('select')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >Utopia</div>
          </Storyboard>
        )`),
    )
  })
  it('Do not save content before exiting the text editor', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

    await enterTextEditMode(editor)
    typeText(' Utopia')
    await editor.getDispatchFollowUpActionsFinished()

    expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >Hello</div>
          </Storyboard>
        )`),
    )
  })
  it('Escapes HTML entities', async () => {
    const editor = await renderTestEditorWithCode(projectWithoutText, 'await-first-dom-report')

    await enterTextEditMode(editor)
    typeText('this is a <test> with bells & whistles')
    await expectSingleUndoStep(editor, async () => closeTextEditor())
    await editor.getDispatchFollowUpActionsFinished()

    expect(editor.getEditorState().editor.mode.type).toEqual('select')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >this is a &lt;test&gt; with bells & whistles</div>
          </Storyboard>
        )`),
    )
  })
  it(`ensure that a bunch of the text editor properties are set to 'inherit'`, async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

    await enterTextEditMode(editor)
    const textEditorElement = document.getElementById(TextEditorSpanId)
    if (textEditorElement == null) {
      throw new Error('A text editor should exist at this point.')
    }
    expect(textEditorElement.style.font).toEqual('inherit')
    expect(textEditorElement.style.fontFamily).toEqual('inherit')
    expect(textEditorElement.style.fontFeatureSettings).toEqual('inherit')
    expect(textEditorElement.style.fontKerning).toEqual('inherit')
    expect(textEditorElement.style.fontOpticalSizing).toEqual('inherit')
    expect(textEditorElement.style.fontSize).toEqual('inherit')
    expect(textEditorElement.style.fontSizeAdjust).toEqual('inherit')
    expect(textEditorElement.style.fontStretch).toEqual('inherit')
    expect(textEditorElement.style.fontStyle).toEqual('inherit')
    expect(textEditorElement.style.fontSynthesis).toEqual('inherit')
    expect(textEditorElement.style.fontVariant).toEqual('inherit')
    expect(textEditorElement.style.fontVariantCaps).toEqual('inherit')
    expect(textEditorElement.style.fontVariantEastAsian).toEqual('inherit')
    expect(textEditorElement.style.fontVariantLigatures).toEqual('inherit')
    expect(textEditorElement.style.fontVariantNumeric).toEqual('inherit')
    expect(textEditorElement.style.fontVariantPosition).toEqual('inherit')
    expect(textEditorElement.style.fontVariationSettings).toEqual('inherit')
    expect(textEditorElement.style.fontWeight).toEqual('inherit')
    expect(textEditorElement.style.textAlign).toEqual('inherit')
    expect(textEditorElement.style.textAlignLast).toEqual('inherit')
    expect(textEditorElement.style.textCombineUpright).toEqual('inherit')
    expect(textEditorElement.style.textDecorationColor).toEqual('inherit')
    expect(textEditorElement.style.textDecorationLine).toEqual('inherit')
    expect(textEditorElement.style.textDecorationSkipInk).toEqual('inherit')
    expect(textEditorElement.style.textDecorationStyle).toEqual('inherit')
    expect(textEditorElement.style.textDecorationThickness).toEqual('inherit')
    expect(textEditorElement.style.textEmphasisColor).toEqual('inherit')
    expect(textEditorElement.style.textEmphasisPosition).toEqual('inherit')
    expect(textEditorElement.style.textEmphasisStyle).toEqual('inherit')
    expect(textEditorElement.style.textIndent).toEqual('inherit')
    expect(textEditorElement.style.textOrientation).toEqual('inherit')
    expect(textEditorElement.style.textOverflow).toEqual('inherit')
    expect(textEditorElement.style.textRendering).toEqual('inherit')
    expect(textEditorElement.style.textShadow).toEqual('inherit')
    expect(textEditorElement.style.textTransform).toEqual('inherit')
    expect(textEditorElement.style.textUnderlineOffset).toEqual('inherit')
    expect(textEditorElement.style.textUnderlinePosition).toEqual('inherit')
    expect(textEditorElement.style.letterSpacing).toEqual('inherit')
    expect(textEditorElement.style.lineHeight).toEqual('inherit')
  })
  describe('formatting shortcuts', () => {
    it('supports bold', async () => {
      const { before, after } = await testModifier(cmdModifier, 'b')
      expect(before).toEqual(projectWithStyle({ fontWeight: 'bold' }))
      expect(after).toEqual(projectWithStyle({}))
    })
    it('doesnt unset bold when regular is not default', async () => {
      const { before, after } = await testModifier(
        cmdModifier,
        'b',
        projectWithoutTextWithStyleProp('font', 'bold 1.2em "Fira Sans"'),
      )
      expect(before).toEqual(
        projectWithStyle({ font: 'bold 1.2em "Fira Sans"', fontWeight: 'normal' }),
      )
      expect(after).toEqual(
        projectWithStyle({ font: 'bold 1.2em "Fira Sans"', fontWeight: 'bold' }),
      )
    })
    it('supports italic', async () => {
      const { before, after } = await testModifier(cmdModifier, 'i')
      expect(before).toEqual(projectWithStyle({ fontStyle: 'italic' }))
      expect(after).toEqual(projectWithStyle({}))
    })
    it('doesnt unset italic when normal is not default', async () => {
      const { before, after } = await testModifier(
        cmdModifier,
        'i',
        projectWithoutTextWithStyleProp('font', 'italic 1.2em "Fira Sans"'),
      )
      expect(before).toEqual(
        projectWithStyle({ font: 'italic 1.2em "Fira Sans"', fontStyle: 'normal' }),
      )
      expect(after).toEqual(
        projectWithStyle({ font: 'italic 1.2em "Fira Sans"', fontStyle: 'italic' }),
      )
    })
    it('supports underline', async () => {
      const { before, after } = await testModifier(cmdModifier, 'u')
      expect(before).toEqual(projectWithStyle({ textDecoration: 'underline' }))
      expect(after).toEqual(projectWithStyle({}))
    })

    it('supports strikethrough', async () => {
      const { before, after } = await testModifier(shiftCmdModifier, 'x')
      expect(before).toEqual(projectWithStyle({ textDecoration: 'line-through' }))
      expect(after).toEqual(projectWithStyle({}))
    })

    xit('supports increasing font size', async () => {
      const { before, after } = await testModifier(shiftCmdModifier, '.')
      expect(before).toEqual(projectWithStyle({ fontSize: '17px' }))
      expect(after).toEqual(projectWithStyle({ fontSize: '18px' }))
    })

    xit('supports increasing font weight', async () => {
      const { before, after } = await testModifier(altCmdModifier, '.')
      expect(before).toEqual(projectWithStyleNoQuotes('fontWeight', '500'))
      expect(after).toEqual(projectWithStyleNoQuotes('fontWeight', '600'))
    })
    xit('supports decreasing font size', async () => {
      const { before, after } = await testModifier(shiftCmdModifier, ',')
      expect(before).toEqual(projectWithStyle({ fontSize: '15px' }))
      expect(after).toEqual(projectWithStyle({ fontSize: '14px' }))
    })

    xit('supports decreasing font weight', async () => {
      const { before, after } = await testModifier(altCmdModifier, ',')
      expect(before).toEqual(projectWithStyleNoQuotes('fontWeight', '300'))
      expect(after).toEqual(projectWithStyleNoQuotes('fontWeight', '200'))
    })

    it("doesn't care about selection", async () => {
      const editor = await renderTestEditorWithCode(projectWithoutText, 'await-first-dom-report')
      await prepareTestModifierEditor(editor)

      const textEditorElement = document.getElementById(TextEditorSpanId)
      expect(textEditorElement).not.toBe(null)
      if (textEditorElement != null) {
        const range = document.createRange()
        range.selectNodeContents(textEditorElement)
        range.collapse(true)

        if (textEditorElement.firstChild != null) {
          range.setStart(textEditorElement.firstChild, 3)
          range.setEnd(textEditorElement.firstChild, 5)

          const selection = window.getSelection()
          if (selection != null) {
            selection.removeAllRanges()
            selection.addRange(range)
          }
        }
      }

      await pressShortcut(editor, cmdModifier, 'b')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithStyle({ fontWeight: 'bold' }),
      )
    })
  })
  it('position cursor with double click', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('div')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 20,
      y: divBounds.y + 10,
    }

    await mouseDoubleClickAtPoint(canvasControlsLayer, divCorner)
    await editor.getDispatchFollowUpActionsFinished()

    await wait(50) // give it time to adjust the caret position

    typeText('--HEY--')

    await closeTextEditor()
    await editor.getDispatchFollowUpActionsFinished()

    expect(editor.getEditorState().editor.mode.type).toEqual('select')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >He--HEY--llo</div>
          </Storyboard>
        )`),
    )
  })
  describe('blur', () => {
    describe('when the element is empty', () => {
      it('keeps existing elements', async () => {
        const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

        await enterTextEditMode(editor)

        deleteTypedText()

        await closeTextEditor()
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().editor.mode.type).toEqual('select')
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          formatTestProjectCode(`
            import * as React from 'react'
            import { Storyboard } from 'utopia-api'


            export var storyboard = (
              <Storyboard data-uid='sb'>
                <div
                  data-testid='div'
                  style={{
                    backgroundColor: '#0091FFAA',
                    position: 'absolute',
                    left: 0,
                    top: 0,
                    width: 288,
                    height: 362,
                  }}
                  data-uid='39e'
                />
              </Storyboard>
            )`),
        )
      })
      it('deletes new elements', async () => {
        const editor = await renderTestEditorWithCode(emptyProject, 'await-first-dom-report')

        const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

        await pressKey('t')
        await editor.getDispatchFollowUpActionsFinished()
        await mouseDragFromPointToPoint(canvasControlsLayer, { x: 500, y: 200 }, { x: 600, y: 300 })

        typeText('I will go away')

        deleteTypedText()

        await closeTextEditor()
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().editor.mode.type).toEqual('select')
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          formatTestProjectCode(`
            import * as React from 'react'
            import { Storyboard } from 'utopia-api'


            export var storyboard = (
              <Storyboard data-uid='sb' />
            )`),
        )
      })
    })
    describe('collapses runs of text', () => {
      it('only when the elements involved are eligible', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithARunOfText,
          'await-first-dom-report',
        )

        const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
        const div = editor.renderedDOM.getByTestId('first-div')
        const divBounds = div.getBoundingClientRect()
        const divCorner = {
          x: divBounds.x + 20,
          y: divBounds.y + 10,
        }

        await mouseDoubleClickAtPoint(canvasControlsLayer, divCorner)
        await editor.getDispatchFollowUpActionsFinished()

        await wait(50) // give it time to adjust the caret position

        typeText('l')

        await closeTextEditor()
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().editor.mode.type).toEqual('select')
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='storyboard'>
            <div
              data-testid='first-div'
              data-uid='first-div'
            >
              Helllo this
            </div>
            <div
              style={{ backgroundColor: 'red' }}
              data-testid='third-div'
              data-uid='third-div'
            >
              is
            </div>
            <div
              data-testid='fourth-div'
              data-uid='fourth-div'
            >
              text! 
            </div>
          </Storyboard>
        )`),
        )
      })
      it('only when the elements involved are eligible after adding some text', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithARunOfText,
          'await-first-dom-report',
        )

        const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
        const div = editor.renderedDOM.getByTestId('first-div')
        const divBounds = div.getBoundingClientRect()
        const divCorner = {
          x: divBounds.x + 30,
          y: divBounds.y + 10,
        }

        await mouseDoubleClickAtPoint(canvasControlsLayer, divCorner)
        await editor.getDispatchFollowUpActionsFinished()

        await wait(50) // give it time to adjust the caret position

        typeText(' there')

        await closeTextEditor()
        await editor.getDispatchFollowUpActionsFinished()

        expect(editor.getEditorState().editor.mode.type).toEqual('select')
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='storyboard'>
            <div
              data-testid='first-div'
              data-uid='first-div'
            >
              Hell thereo this
            </div>
            <div
              style={{ backgroundColor: 'red' }}
              data-testid='third-div'
              data-uid='third-div'
            >
              is
            </div>
            <div
              data-testid='fourth-div'
              data-uid='fourth-div'
            >
              text! 
            </div>
          </Storyboard>
        )`),
        )
      })
    })
  })
  describe('multiline editing', () => {
    it('renders and escapes newlines', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      await enterTextEditMode(editor)
      typeText('\nHow are you?')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >
              Hello
              <br />
              How are you?
            </div>
          </Storyboard>
        )`),
      )

      await enterTextEditMode(editor)
      typeText('\n\nblablabla')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >
              Hello
              <br />
              How are you?
              <br />
              <br />
              blablabla
            </div>
          </Storyboard>
        )`),
      )
    })
    it('does not trim trailing newlines', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      await enterTextEditMode(editor)
      typeText('\n\n')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >
              Hello
              <br />
              <br />
            </div>
          </Storyboard>
        )`),
      )

      await enterTextEditMode(editor)
      typeText('test')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >
              Hello
              <br />
              <br />
              test
            </div>
          </Storyboard>
        )`),
      )
    })
    it('trims all whitespaces', async () => {
      const editor = await renderTestEditorWithCode(
        formatTestProjectCode(`import * as React from 'react'
      import { Storyboard } from 'utopia-api'
      
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <div
            data-testid='div'
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 288,
              height: 362,
            }}
            data-uid='39e'
          >
            
           Lorem   ipsum dolor sit amet, consectetur
           adipiscing elit, sed    do eiusmod
           
           tempor


          </div>
        </Storyboard>
      )
      `),
        'await-first-dom-report',
      )

      await enterTextEditMode(editor)
      typeText('\n\n')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
              <br />
              <br />
            </div>
          </Storyboard>
        )`),
      )
    })
    it('doesnt trim whitespace around code', async () => {
      const editor = await renderTestEditorWithCode(
        formatTestProjectCode(`import * as React from 'react'
      import { Storyboard } from 'utopia-api'
      
      const ipsum = 'ipsum in a variable'
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <div
            data-testid='div'
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 288,
              height: 362,
            }}
            data-uid='39e'
          >
            Lorem {ipsum} dolor   sit   amet
          </div>
        </Storyboard>
      )
      `),
        'await-first-dom-report',
      )

      await enterTextEditMode(editor)
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'

        const ipsum = 'ipsum in a variable'
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
              }}
              data-uid='39e'
            >
              Lorem {ipsum} dolor sit amet
            </div>
          </Storyboard>
        )`),
      )
    })
  })
  describe('inline expressions', () => {
    it('handles expressions', async () => {
      const editor = await renderTestEditorWithCode(projectWithoutText, 'await-first-dom-report')

      await enterTextEditMode(editor)
      typeText('the answer is {41 + 1}')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
              import * as React from 'react'
              import { Storyboard } from 'utopia-api'


              export var storyboard = (
                <Storyboard data-uid='sb'>
                  <div
                    data-testid='div'
                    style={{
                      backgroundColor: '#0091FFAA',
                      position: 'absolute',
                      left: 0,
                      top: 0,
                      width: 288,
                      height: 362,
                    }}
                    data-uid='39e'
                  >the answer is {41 + 1}</div>
                </Storyboard>
              )`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('the answer is 42')
    })
  })
})

function projectWithStyle(props: { [prop: string]: string }) {
  const styleProps = {
    backgroundColor: '#0091FFAA',
    position: 'absolute',
    left: 0,
    top: 0,
    width: 288,
    height: 362,
    ...props,
  }
  return formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={${JSON.stringify(styleProps)}}
              data-uid='39e'
            >Hello Utopia</div>
          </Storyboard>
        )`)
}

function projectWithStyleNoQuotes(prop: string, value: string) {
  return formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard } from 'utopia-api'


        export var storyboard = (
          <Storyboard data-uid='sb'>
            <div
              data-testid='div'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 288,
                height: 362,
                ${prop}: ${value}
              }}
              data-uid='39e'
            >Hello Utopia</div>
          </Storyboard>
        )`)
}

function deleteTypedText() {
  const range = document.createRange()
  const selection = window.getSelection()
  if (selection != null) {
    selection.removeAllRanges()
    range.selectNodeContents(document.getElementById(TextEditorSpanId) ?? document.body)
    selection.addRange(range)
  }
  typeText('')
}

async function prepareTestModifierEditor(editor: EditorRenderResult) {
  await enterTextEditMode(editor)
  typeText('Hello Utopia')
}

async function pressShortcut(editor: EditorRenderResult, mod: Modifiers, key: string) {
  await expectSingleUndoStep(editor, async () => {
    await pressKey(key, {
      modifiers: mod,
      targetElement: document.getElementById(TextEditorSpanId) ?? undefined,
    })
  })
  await closeTextEditor()
  await editor.getDispatchFollowUpActionsFinished()
}

async function testModifier(
  mod: Modifiers,
  key: string,
  startingProject: string = projectWithoutText,
) {
  const editor = await renderTestEditorWithCode(startingProject, 'await-first-dom-report')

  await prepareTestModifierEditor(editor)
  await pressShortcut(editor, mod, key)
  const before = getPrintedUiJsCode(editor.getEditorState())

  await enterTextEditMode(editor)
  await pressShortcut(editor, mod, key)
  const after = getPrintedUiJsCode(editor.getEditorState())

  return { before, after }
}

async function enterTextEditMode(editor: EditorRenderResult): Promise<void> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('div')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  await pressKey('t')
  await editor.getDispatchFollowUpActionsFinished()
  await mouseClickAtPoint(canvasControlsLayer, divCorner)
  await editor.getDispatchFollowUpActionsFinished()
}

function typeText(text: string) {
  document.execCommand('insertText', false, text)
}

async function closeTextEditor() {
  await pressKey('Escape')
  await wait(0) // this is needed so we wait until the dispatch call is launched in a settimeout when the text editor unmounts
}

const projectWithText = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-testid='div'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 288,
        height: 362,
      }}
      data-uid='39e'
    >
      Hello
    </div>
  </Storyboard>
)
`)

const projectWithoutText = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-testid='div'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 288,
        height: 362,
      }}
      data-uid='39e'
    />
  </Storyboard>
)
`)

function projectWithoutTextWithStyleProp(prop: string, value: string) {
  return formatTestProjectCode(`import * as React from 'react'
    import { Storyboard } from 'utopia-api'


    export var storyboard = (
      <Storyboard data-uid='sb'>
        <div
          data-testid='div'
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 0,
            top: 0,
            width: 288,
            height: 362,
            ${prop}: '${value}'
          }}
          data-uid='39e'
        />
      </Storyboard>
    )
  `)
}

const emptyProject = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
  </Storyboard>
)
`)

const projectWithARunOfText = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-testid='first-div'
      data-uid='first-div'
    >Hello</div>
    <div
      data-testid='second-div'
      data-uid='second-div'
    >this</div>
    <div
      style={{
        backgroundColor: 'red'
      }}
      data-testid='third-div'
      data-uid='third-div'
    >is</div>
    <div
      data-testid='fourth-div'
      data-uid='fourth-div'
    >text!</div>
  </Storyboard>
)
`)
