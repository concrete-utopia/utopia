import * as EP from '../../core/shared/element-path'
import {
  expectSingleUndo2Saves,
  expectSingleUndoNSaves,
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
  wait,
} from '../../utils/utils.test-utils'
import type { Modifiers } from '../../utils/modifiers'
import { altCmdModifier, cmdModifier, shiftCmdModifier } from '../../utils/modifiers'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  mouseDragFromPointToPoint,
  pressKey,
} from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { TextEditorSpanId } from './text-editor'
import { FOR_TESTS_setNextGeneratedUid } from '../../core/model/element-template-utils.test-utils'
import {
  selectComponents,
  setFocusedElement,
  switchEditorMode,
} from '../editor/actions/action-creators'
import { EditorModes } from '../editor/editor-modes'

describe('Use the text editor', () => {
  it('Click to edit text', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

    await enterTextEditMode(editor)
    typeText(' Utopia')
    await expectSingleUndoNSaves(editor, 1, async () => closeTextEditor())
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
    await expectSingleUndoNSaves(editor, 1, async () => closeTextEditor())
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
            >${textSpan('Utopia')}</div>
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
    await expectSingleUndoNSaves(editor, 1, async () => closeTextEditor())
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
            >${textSpan('this is a &lt;test&gt; with bells & whistles')}</div>
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
      // FIXME This should not be triggering 4 saves!
      const { before, after } = await testModifierExpectingWayTooManySavesTheFirstTime(
        cmdModifier,
        'b',
        projectWithoutTextWithExtraStyle({ font: 'bold 1.2em "Fira Sans"' }),
      )
      expect(before).toEqual(
        projectWithStyle({ fontWeight: 'normal' }, { font: 'bold 1.2em "Fira Sans"' }),
      )
      expect(after).toEqual(
        projectWithStyle({ fontWeight: 'bold' }, { font: 'bold 1.2em "Fira Sans"' }),
      )
    })
    it('supports italic', async () => {
      const { before, after } = await testModifier(cmdModifier, 'i')
      expect(before).toEqual(projectWithStyle({ fontStyle: 'italic' }))
      expect(after).toEqual(projectWithStyle({}))
    })
    it('doesnt unset italic when normal is not default', async () => {
      // FIXME This should not be triggering 4 saves!
      const { before, after } = await testModifierExpectingWayTooManySavesTheFirstTime(
        cmdModifier,
        'i',
        projectWithoutTextWithExtraStyle({ font: 'italic 1.2em "Fira Sans"' }),
      )
      expect(before).toEqual(
        projectWithStyle({ fontStyle: 'normal' }, { font: 'italic 1.2em "Fira Sans"' }),
      )
      expect(after).toEqual(
        projectWithStyle({ fontStyle: 'italic' }, { font: 'italic 1.2em "Fira Sans"' }),
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
  it('handles an element access using a function', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithSnippet(`<span data-uid='span'>{style[getBackgroundColor()]}</span>`),
      'await-first-dom-report',
    )
    expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('lightblue')

    await editor.dispatch([selectComponents([EP.fromString('sb/39e/span')], false)], true)
    await pressKey('enter')
    await editor.getDispatchFollowUpActionsFinished()

    typeText(`{style[getColor()]}`)
    await closeTextEditor()

    await editor.getDispatchFollowUpActionsFinished()
    await wait(50)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      projectWithSnippet(`<span data-uid='span'>{style[getColor()]}</span>`),
    )
    expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('green')
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
      it('deletes existing span elements', async () => {
        const editor = await renderTestEditorWithCode(
          formatTestProjectCode(`
            import * as React from 'react'
            import { Storyboard } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='sb'>
                <span
                  data-uid='span'
                  data-testid='span'
                  style={{
                    position: 'absolute',
                    wordBreak: 'break-word',
                    left: 0,
                    top: 0,
                    width: 'max-content',
                    height: 'max-content',
                  }}
                >
                  Hello
                </span>
              </Storyboard>
            )
          `),
          'await-first-dom-report',
        )

        await enterTextEditMode(editor, 'start', 'span')

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
            )
          `),
        )
      })
      it('does not delete span elements with event handlers', async () => {
        const editor = await renderTestEditorWithCode(
          formatTestProjectCode(`
            import * as React from 'react'
            import { Storyboard } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='sb'>
                <span
                  data-uid='span'
                  data-testid='span'
                  style={{
                    position: 'absolute',
                    wordBreak: 'break-word',
                    left: 0,
                    top: 0,
                    width: 'max-content',
                    height: 'max-content',
                  }}
                  onClick={() => console.log('click')}
                >
                  Hello
                </span>
              </Storyboard>
            )
          `),
          'await-first-dom-report',
        )

        await enterTextEditMode(editor, 'start', 'span')

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
                <span
                  data-uid='span'
                  data-testid='span'
                  style={{
                    position: 'absolute',
                    wordBreak: 'break-word',
                    left: 0,
                    top: 0,
                    width: 'max-content',
                    height: 'max-content',
                  }}
                  onClick={() => console.log('click')}
                />
              </Storyboard>
            )
          `),
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
  describe('elements with custom css', () => {
    it('allows leading spaces while editing text-wrap:pretty', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithoutTextWithExtraStyle({ textWrap: 'pretty' }),
        'await-first-dom-report',
      )

      await enterTextEditMode(editor)
      typeText('Hello')
      typeText(' ')
      typeText('Utopia')
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
                textWrap: 'pretty',
              }}
              data-uid='39e'
            >
              Hello Utopia
            </div>
          </Storyboard>
        )`),
      )
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
    const tests = [
      {
        label: 'handles expressions',
        writtenText: 'the answer is {41 + 1}',
        codeResult: 'the answer is {41 + 1}',
        renderedText: 'the answer is 42',
      },
      {
        label: 'handles conditionals',
        writtenText: 'The user name is {1 === 1 ? "Bob" : "Sam"}',
        codeResult: 'The user name is {1 === 1 ? "Bob" : "Sam"}',
        renderedText: 'The user name is Bob',
      },
      {
        label: 'htmlencodes angular brackets in text parts',
        writtenText: 'The <username> is {1 === 1 ? "Bob" : "Sam"}',
        codeResult: 'The &lt;username&gt; is {1 === 1 ? "Bob" : "Sam"}',
        renderedText: 'The <username> is Bob',
      },
      {
        label: 'does not htmlencode angular brackets in js parts',
        writtenText: 'The username is {1 >= 1 ? "Bob" : "Sam"}',
        codeResult: 'The username is {1 >= 1 ? "Bob" : "Sam"}',
        renderedText: 'The username is Bob',
      },
      {
        label: 'handles angular brackets in text and js parts both',
        writtenText: 'The <username> is {1 >= 1 ? "Bob" : "Sam"}',
        codeResult: 'The &lt;username&gt; is {1 >= 1 ? "Bob" : "Sam"}',
        renderedText: 'The <username> is Bob',
      },
      {
        label: 'handles angular brackets in multiple text and js parts',
        writtenText:
          'The <username> is {1 >= 1 ? "Bob" : "Sam"} The <username> is {1 < 1 ? "Bob" : "Sam"}',
        codeResult:
          'The &lt;username&gt; is {1 >= 1 ? "Bob" : "Sam"} The &lt;username&gt; is {1 < 1 ? "Bob" : "Sam"}',
        renderedText: 'The <username> is Bob The <username> is Sam',
      },
      {
        label: 'ignores closing curly bracket when inside quotation marks',
        writtenText: 'The username is {1 >= 1 ? "Bob" : "Sam}" + ">"}',
        codeResult: 'The username is {1 >= 1 ? "Bob" : "Sam}" + ">"}',
        renderedText: 'The username is Bob',
      },
      {
        label: 'handles expressions with formatted strings inside',
        // eslint-disable-next-line no-template-curly-in-string
        writtenText: 'The username is {1 >= 1 ? `${"Bob"}` : "Sam"}',
        // eslint-disable-next-line no-template-curly-in-string
        codeResult: 'The username is {1 >= 1 ? `${"Bob"}` : "Sam"}',
        renderedText: 'The username is Bob',
      },
      {
        label: 'handles nulls',
        // eslint-disable-next-line no-template-curly-in-string
        writtenText: 'The username is {1 >= 1 ? null : null}',
        // eslint-disable-next-line no-template-curly-in-string
        codeResult: 'The username is {1 >= 1 ? null : null}',
        renderedText: 'The username is',
      },
    ]
    tests.forEach((t) => {
      it(`${t.label}`, async () => {
        const editor = await renderTestEditorWithCode(projectWithoutText, 'await-first-dom-report')

        await enterTextEditMode(editor)
        typeText(t.writtenText)
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
                  >
                    ${textSpan(t.codeResult)}
                  </div>
                </Storyboard>
              )`),
        )
        expect(editor.renderedDOM.getByTestId('div').innerText).toEqual(t.renderedText)
      })
    })
  })
  describe('conditional clauses', () => {
    it('editing the true clause', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hello' : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/03d')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('hi')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hi' : <div data-uid='33d' />
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('hi')
    })
    it('editing the active true clause from the selected conditional', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hello' : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('hi')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hi' : <div data-uid='33d' />
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('hi')
    })
    it('editing the active true clause with null inside from the selected conditional', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? null : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('hi')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hi' : <div data-uid='33d' />
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('hi')
    })
    it('editing the full conditional from the selected conditional (to a non-conditional)', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hello' : 'utopia'
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('hi')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(projectWithSnippet('hi'))
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('hi')
    })
    it('editing the full conditional from the selected conditional (keeping it a conditional)', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hello' : 'utopia'
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText(`{
        // @utopia/uid=cond
        true ? 'hi' : 'utopia'
      }`)
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hi' : 'utopia'
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('hi')
    })
    it('editing is not allowed with siblings', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? 'hello' : <div data-uid='33d' />
        }<div />`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/409')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()
      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
    it('editing the false clause', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : 'hello'
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/03d')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('hi')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : 'hi'
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('hi')
    })
    it('editing the active false clause from the selected conditional', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : 'hello'
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('hi')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : 'hi'
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('hi')
    })
    it('editing expression in the true clause', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? (
            // @utopia/uid=536
            myvar1
          ) : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/536')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('{myvar2}')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? (
            // @utopia/uid=536
            myvar2
          ) : (
            <div data-uid='33d' />
          )
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('content of myvar2')
    })
    it('editing expression in the true clause is not allowed when the expression returns elements', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? (
            // @utopia/uid=536
            (() => <div>hello</div>)()
          ) : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/536')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()
      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
    it('editing a map expression in the true clause is not allowed when the expression returns elements', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? (
            // @utopia/uid=6b9
            [0,1,2].map(() => <div>hello</div>)
          ) : <div data-uid='xxx' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/6b9')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()
      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual(
        'sb/39e/cond/6b9/2c7~~~1',
      )
    })
    it('editing expression in the active true clause from the selected conditional', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? myvar1 : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('{myvar2}')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? myvar2 : <div data-uid='33d' />
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('content of myvar2')
    })
    it('editing a map expression in the active true clause from the selected conditional', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? [0,1,2].map(() => 'hello') : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('{myvar2}')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? myvar2 : <div data-uid='33d' />
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('content of myvar2')
    })
    it('editing expression in the active true clause from the selected conditional is not allowed when the expression returns elements', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? (() => <div>hello</div>)() : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
    it('editing a map expression in the active true clause from the selected conditional is not allowed when the expression returns elements', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          true ? [0,1,2].map(() => <div>hello</div>) : <div data-uid='33d' />
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
    it('editing expression in the false clause', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : (
            // @utopia/uid=536
            myvar1
          )
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/536')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('{myvar2}')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? (
            <div data-uid='33d' />
          ) : (
            // @utopia/uid=536
            myvar2
          )
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('content of myvar2')
    })
    it('editing a map expression in the false clause', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : (
            // @utopia/uid=089
            [0,1,2].map(() => 'hello')
          )
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/089')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('{myvar2}')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? (
            <div data-uid='33d' />
          ) : (
            // @utopia/uid=089
            myvar2
          )
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('content of myvar2')
    })

    it('editing expression in the false clause is not allowed when the expression returns elements', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : [0,1,2].map(() => <div>hello</div>)
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/089')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
    it('editing a map expression in the false clause is not allowed when the expression returns elements', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : (
            // @utopia/uid=536
            (() => <div>hello</div>)()
          )
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/536')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
    it('editing expression in the false clause from the selected conditional', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : myvar1
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('{myvar2}')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : myvar2
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('content of myvar2')
    })
    it('editing a map expression in the false clause from the selected conditional', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : [0,1,2].map(() => 'hello')
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      typeText('{myvar2}')
      await closeTextEditor()

      await editor.getDispatchFollowUpActionsFinished()
      await wait(50)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : myvar2
        }`),
      )
      expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('content of myvar2')
    })
    it('editing expression in the false clause from the selected conditional is not allowed when the expression returns elements', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : (() => <div>hello</div>)()
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
    it('editing a map expression in the false clause from the selected conditional is not allowed when the expression returns elements', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithSnippet(`{
          // @utopia/uid=cond
          false ? <div data-uid='33d' /> : [0,1,2].map(() => <div>hello</div>)
        }`),
        'await-first-dom-report',
      )

      await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond')], false)], true)
      await pressKey('enter')
      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
  })
  it('editing expression (in the true clause) and deleting curly braces converts it to string literal', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithSnippet(`{
        // @utopia/uid=cond
        true ? (
          // @utopia/uid=536
          myvar1
        ) : <div data-uid='33d' />
      }`),
      'await-first-dom-report',
    )

    await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/536')], false)], true)
    await pressKey('enter')
    await editor.getDispatchFollowUpActionsFinished()

    typeText('this is just a string')
    await closeTextEditor()

    await editor.getDispatchFollowUpActionsFinished()
    await wait(50)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      projectWithSnippet(`{
        // @utopia/uid=cond
        true ? (
          // @utopia/uid=536
          'this is just a string'
        ) : <div data-uid='33d' />
      }`),
    )
    expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('this is just a string')
  })
  it('editing a map expression (in the true clause) and deleting curly braces converts it to string literal', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithSnippet(`{
        // @utopia/uid=cond
        true ? (
          // @utopia/uid=089
          [0,1,2].map(() => 'hello')
        ) : <div data-uid='33d' />
      }`),
      'await-first-dom-report',
    )

    await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/089')], false)], true)
    await pressKey('enter')
    await editor.getDispatchFollowUpActionsFinished()

    typeText('this is just a string')
    await closeTextEditor()

    await editor.getDispatchFollowUpActionsFinished()
    await wait(50)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      projectWithSnippet(`{
        // @utopia/uid=cond
        true ? (
          // @utopia/uid=089
          'this is just a string'
        ): <div data-uid='33d' />
      }`),
    )
    expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('this is just a string')
  })
  it('editing expression (in the false clause) and deleting curly braces converts it to string literal', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithSnippet(`{
        // @utopia/uid=cond
        false ? <div data-uid='33d' /> : (
          // @utopia/uid=536
          myvar1
        )
      }`),
      'await-first-dom-report',
    )

    await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/536')], false)], true)
    await pressKey('enter')
    await editor.getDispatchFollowUpActionsFinished()

    typeText('this is just a string')
    await closeTextEditor()

    await editor.getDispatchFollowUpActionsFinished()
    await wait(50)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      projectWithSnippet(`{
        // @utopia/uid=cond
        false ? <div data-uid='33d' /> : (
          // @utopia/uid=536
          'this is just a string'
        )
      }`),
    )
    expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('this is just a string')
  })
  it('editing a map expression (in the false clause) and deleting curly braces converts it to string literal', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithSnippet(`{
        // @utopia/uid=cond
        false ? <div data-uid='33d' /> : (
          // @utopia/uid=089
          [0,1,2].map(() => 'hello')
        )
      }`),
      'await-first-dom-report',
    )

    await editor.dispatch([selectComponents([EP.fromString('sb/39e/cond/089')], false)], true)
    await pressKey('enter')
    await editor.getDispatchFollowUpActionsFinished()

    typeText('this is just a string')
    await closeTextEditor()

    await editor.getDispatchFollowUpActionsFinished()
    await wait(50)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      projectWithSnippet(`{
        // @utopia/uid=cond
        false ? <div data-uid='33d' /> : (
          // @utopia/uid=089
          'this is just a string'
        )
      }`),
    )
    expect(editor.renderedDOM.getByTestId('div').innerText).toEqual('this is just a string')
  })

  xdescribe('Editing with steganograpy enabled', () => {
    const steganoProject = `import * as React from 'react'
    import { Storyboard, Scene } from 'utopia-api'

    const StoryboardWrapper = ({ style }) => {
      const text = 'Hello'
      return (
        <div data-uid='div' data-testid='div' style={{ ...style }}>
          {text}
        </div>
      )
    }

    export var storyboard = (
      <Storyboard data-uid='sb'>
        <Scene
          data-uid='scene'
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 144,
            top: 58,
            width: 288,
            height: 362,
          }}
        >
          <StoryboardWrapper data-uid='wrapper' />
        </Scene>
      </Storyboard>
    )
    `

    it('updates string literal valued variable in component body when it is edited', async () => {
      const editor = await renderTestEditorWithCode(steganoProject, 'await-first-dom-report', {
        applySteganography: 'apply-steganography',
      })

      await enterTextEditMode(editor, 'start')
      typeText(' Utopia')
      await expectSingleUndo2Saves(editor, async () => closeTextEditor())
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard, Scene } from 'utopia-api'

const StoryboardWrapper = ({ style }) => {
  const text = ' UtopiaHello'
  return (
    <div
      data-uid='div'
      data-testid='div'
      style={{ ...style }}
    >
      {text}
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 144,
        top: 58,
        width: 288,
        height: 362,
      }}
    >
      <StoryboardWrapper data-uid='wrapper' />
    </Scene>
  </Storyboard>
)
`)
    })

    it('updates text var passed through props', async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
        import { Storyboard, Scene } from 'utopia-api'
        
        const variable = 'constant'
        
        const StoryboardWrapper = ({ style, text }) => {
          return (
            <div
              data-uid='div'
              data-testid='div'
              style={{ ...style }}
            >
              {text}
            </div>
          )
        }
        
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              data-uid='scene'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 144,
                top: 58,
                width: 288,
                height: 362,
              }}
            >
              <StoryboardWrapper text={variable} data-uid='wrapper' />
            </Scene>
          </Storyboard>
        )`,
        'await-first-dom-report',
        { applySteganography: 'apply-steganography' },
      )

      await enterTextEditMode(editor, 'start')
      typeText('this is not a ')
      await expectSingleUndo2Saves(editor, async () => closeTextEditor())
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard, Scene } from 'utopia-api'

const variable = 'this is not a constant'

const StoryboardWrapper = ({ style, text }) => {
  return (
    <div
      data-uid='div'
      data-testid='div'
      style={{ ...style }}
    >
      {text}
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 144,
        top: 58,
        width: 288,
        height: 362,
      }}
    >
      <StoryboardWrapper
        text={variable}
        data-uid='wrapper'
      />
    </Scene>
  </Storyboard>
)
`)
    })

    it('can handle quote marks and newlines in the updated text', async () => {
      const editor = await renderTestEditorWithCode(steganoProject, 'await-first-dom-report', {
        applySteganography: 'apply-steganography',
      })

      await enterTextEditMode(editor, 'start')
      typeText(`"quote" 'unquote'`)
      document.execCommand('insertParagraph', false)
      typeText('fin')
      await expectSingleUndo2Saves(editor, async () => closeTextEditor())
      await editor.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard, Scene } from 'utopia-api'

const StoryboardWrapper = ({ style }) => {
  const text = '"quote" \\'unquote\\'\\nfinHello'
  return (
    <div
      data-uid='div'
      data-testid='div'
      style={{ ...style }}
    >
      {text}
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 144,
        top: 58,
        width: 288,
        height: 362,
      }}
    >
      <StoryboardWrapper data-uid='wrapper' />
    </Scene>
  </Storyboard>
)
`)
    })

    it('can update text that has escaped newlines to begin with', async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      
      const StoryboardWrapper = ({ style }) => {
        const text = 'line 1\\nline 2'
        return (
          <div
            data-uid='div'
            data-testid='div'
            style={{ ...style }}
          >
            {text}
          </div>
        )
      }
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            data-uid='scene'
            style={{
              backgroundColor: '#0091FFAA',
              position: 'absolute',
              left: 144,
              top: 58,
              width: 288,
              height: 362,
            }}
          >
            <StoryboardWrapper data-uid='wrapper' />
          </Scene>
        </Storyboard>
      )`,
        'await-first-dom-report',
        {
          applySteganography: 'apply-steganography',
        },
      )

      await enterTextEditMode(editor, 'start')
      typeText('hello')
      await expectSingleUndo2Saves(editor, async () => closeTextEditor())
      await editor.getDispatchFollowUpActionsFinished()
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard, Scene } from 'utopia-api'

const StoryboardWrapper = ({ style }) => {
  const text = 'helloline 1\\nline 2'
  return (
    <div
      data-uid='div'
      data-testid='div'
      style={{ ...style }}
    >
      {text}
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      data-uid='scene'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 144,
        top: 58,
        width: 288,
        height: 362,
      }}
    >
      <StoryboardWrapper data-uid='wrapper' />
    </Scene>
  </Storyboard>
)
`)
    })

    it('does not print back weird characters into the file on unrelated edits', async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
        import { Storyboard, Scene } from 'utopia-api'
        
        const StoryboardWrapper = ({ style }) => {
          const text = 'Hello'
          return (
            <div data-uid='div' data-testid='div' style={{ ...style }}>
              {text}
            </div>
          )
        }
        
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              data-uid='scene'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 144,
                top: 58,
                width: 288,
                height: 362,
              }}
            >
              <StoryboardWrapper data-uid='wrapper' />
            </Scene>
          </Storyboard>
        )
        `,
        'await-first-dom-report',
        { applySteganography: 'apply-steganography' },
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/wrapper')])
      await pressKey('Backspace')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
        import * as React from 'react'
        import { Storyboard, Scene } from 'utopia-api'

        const StoryboardWrapper = ({ style }) => {
          const text = 'Hello'
          return (
            <div data-uid='div' data-testid='div' style={{ ...style }}>
              {text}
            </div>
          )
        }

        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              data-uid='scene'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 144,
                top: 58,
                width: 288,
                height: 362,
              }}
            />
          </Storyboard>
        )
`),
      )
    })

    it("can edit string when there's more than one declaration", async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
        import { Storyboard, Scene } from 'utopia-api'
        
        const aaa = 'aaa'
        const bbb = 'bbb'
        const ccc = 'ccc'
        
        export var storyboard = (
          <Storyboard>
            <Scene
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: 144,
                top: 58,
                width: 288,
                height: 362,
              }}
            >
              <div data-testid='111' style={{}}>
                {aaa}
              </div>
              <div
                data-testid='222'
                style={{
                  width: 288,
                  height: 19,
                  position: 'absolute',
                  left: 0,
                  top: 41,
                }}
              >
                {bbb}
              </div>
              <div
                data-testid='333'
                style={{
                  width: 288,
                  height: 19,
                  position: 'absolute',
                  left: 0,
                  top: 71,
                }}
              >
                {ccc}
              </div>
            </Scene>
          </Storyboard>
        )        
      `,
        'await-first-dom-report',
        {
          applySteganography: 'apply-steganography',
        },
      )

      await enterTextEditMode(editor, 'start', '111', '111-span')
      typeText('111')
      await expectSingleUndo2Saves(editor, async () => closeTextEditor())

      await enterTextEditMode(editor, 'start', '222', '222-span')
      typeText('222')
      await expectSingleUndo2Saves(editor, async () => closeTextEditor())

      await enterTextEditMode(editor, 'start', '333', '333-span')
      typeText('333')
      await expectSingleUndo2Saves(editor, async () => closeTextEditor())

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard, Scene } from 'utopia-api'

const aaa = '111aaa'
const bbb = '222bbb'
const ccc = '333ccc'

export var storyboard = (
  <Storyboard>
    <Scene
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 144,
        top: 58,
        width: 288,
        height: 362,
      }}
    >
      <div data-testid='111' style={{}}>
        {aaa}
      </div>
      <div
        data-testid='222'
        style={{
          width: 288,
          height: 19,
          position: 'absolute',
          left: 0,
          top: 41,
        }}
      >
        {bbb}
      </div>
      <div
        data-testid='333'
        style={{
          width: 288,
          height: 19,
          position: 'absolute',
          left: 0,
          top: 71,
        }}
      >
        {ccc}
      </div>
    </Scene>
  </Storyboard>
)
`)
    })
  })
})

describe('SWITCH_EDITOR_MODE', () => {
  describe('TextEditMode', () => {
    it('should be permitted for something that is text editable', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      await editor.dispatch(
        [
          switchEditorMode(
            EditorModes.textEditMode(
              EP.fromString('sb/39e'),
              null,
              'existing',
              'no-text-selection',
            ),
          ),
        ],
        true,
      )
      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
    })
    it('should not be permitted for something that is not text editable', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      await editor.dispatch(
        [
          switchEditorMode(
            EditorModes.textEditMode(EP.fromString('sb'), null, 'existing', 'no-text-selection'),
          ),
        ],
        true,
      )
      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
  })
})

describe('Mouse behavior during text editing', () => {
  const beforeUnloadCallback = () => {
    throw new Error('Should not navigate away on click')
  }
  before(() => window.addEventListener('beforeunload', beforeUnloadCallback))
  after(() => window.removeEventListener('beforeunload', beforeUnloadCallback))
  // eslint-disable-next-line jest/expect-expect
  it('Clicking on link does not navigate away', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithSnippet(`<a href='hello' data-uid='link' data-testid='link'>Hello link</a>`),
      'await-first-dom-report',
    )
    await enterTextEditMode(editor)

    const textEditor = editor.renderedDOM.getByTestId(TextEditorSpanId)
    const textEditorBounds = textEditor.getBoundingClientRect()
    const textEditorCorner = {
      x: textEditorBounds.x + 20,
      y: textEditorBounds.y + 10,
    }

    await mouseClickAtPoint(textEditor, textEditorCorner)
    // no expectation, we just need to avoid the error in beforeUnloadCallback
  })
})

function textSpan(text: string, extraStyleProps?: { [prop: string]: string }): string {
  const styleProps = {
    position: 'absolute',
    wordBreak: 'break-word',
    left: 51,
    top: 41,
    width: 'max-content',
    height: 'max-content',
    ...extraStyleProps,
  }
  return `
    <span
      style={${JSON.stringify(styleProps)}}
      data-uid='text-span'
    >
      ${text}
    </span>
  `
}

function projectWithStyle(
  props: { [prop: string]: string },
  divExtraStyleProps?: { [prop: string]: string },
) {
  const divStyleProps = {
    backgroundColor: '#0091FFAA',
    position: 'absolute',
    left: 0,
    top: 0,
    width: 288,
    height: 362,
    ...divExtraStyleProps,
  }

  return formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'


    export var storyboard = (
      <Storyboard data-uid='sb'>
        <div
          data-testid='div'
          style={${JSON.stringify(divStyleProps)}}
          data-uid='39e'
        >
          ${textSpan('Hello Utopia', props)}
        </div>
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

async function pressShortcut(
  editor: EditorRenderResult,
  mod: Modifiers,
  key: string,
  expectFarTooManySaves: boolean = false,
) {
  await expectSingleUndoNSaves(editor, expectFarTooManySaves ? 4 : 2, async () => {
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

  await pressShortcut(editor, mod, key)
  const after = getPrintedUiJsCode(editor.getEditorState())

  return { before, after }
}

// FIXME This function should not exist!
async function testModifierExpectingWayTooManySavesTheFirstTime(
  mod: Modifiers,
  key: string,
  startingProject: string = projectWithoutText,
) {
  const editor = await renderTestEditorWithCode(startingProject, 'await-first-dom-report')

  await prepareTestModifierEditor(editor)
  await pressShortcut(editor, mod, key, true)
  const before = getPrintedUiJsCode(editor.getEditorState())

  await pressShortcut(editor, mod, key)
  const after = getPrintedUiJsCode(editor.getEditorState())

  return { before, after }
}

async function enterTextEditMode(
  editor: EditorRenderResult,
  where: 'start' | 'end' = 'end',
  testId: string = 'div',
  nextUid = 'text-span',
): Promise<void> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const element = editor.renderedDOM.getByTestId(testId)
  const bounds = element.getBoundingClientRect()
  const corner = {
    x: bounds.x + (where === 'start' ? 1 : 50),
    y: bounds.y + (where === 'start' ? 1 : 40),
  }

  FOR_TESTS_setNextGeneratedUid(nextUid)

  await pressKey('t')
  await editor.getDispatchFollowUpActionsFinished()
  await mouseClickAtPoint(canvasControlsLayer, corner)
  await editor.getDispatchFollowUpActionsFinished()
}

function typeText(text: string) {
  document.execCommand('insertText', false, text)
}

async function closeTextEditor() {
  await pressKey('Escape')
  await wait(0) // this is needed so we wait until the dispatch call is launched in a settimeout when the text editor unmounts
}

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

function projectWithSnippet(snippet: string) {
  return formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

function getBackgroundColor() {
  return 'backgroundColor'
}
function getColor() {
  return 'color'
}
const style = {
  backgroundColor: 'lightblue',
  color: 'green'
}

const myvar1 = 'content of myvar1'
const myvar2 = 'content of myvar2'
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
      ${snippet}
    </div>
  </Storyboard>
)
`)
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

function projectWithoutTextWithExtraStyle(extraStyleProps: { [prop: string]: string }) {
  const styleProps = {
    backgroundColor: '#0091FFAA',
    position: 'absolute',
    left: 0,
    top: 0,
    width: 288,
    height: 362,
    ...extraStyleProps,
  }

  return formatTestProjectCode(`import * as React from 'react'
    import { Storyboard } from 'utopia-api'


    export var storyboard = (
      <Storyboard data-uid='sb'>
        <div
          data-testid='div'
          style={${JSON.stringify(styleProps)}}
          data-uid='39e'
        />
      </Storyboard>
    )
  `)
}

const projectWithTextSpan = formatTestProjectCode(`
import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
    <span
      data-testid='span'
      style={{
        position: 'absolute',
        wordBreak: 'break-word',
        left: 0,
        top: 0,
        width: 'max-content',
        height: 'max-content',
      }}
    >
      Hello
    </span>
  </Storyboard>
)
`)

function projectWithoutTextSpanWithExtraStyle(extraStyleProps: { [prop: string]: string }) {
  const styleProps = {
    backgroundColor: '#0091FFAA',
    position: 'absolute',
    left: 0,
    top: 0,
    width: 288,
    height: 362,
    ...extraStyleProps,
  }

  return formatTestProjectCode(`
    import * as React from 'react'
    import { Storyboard } from 'utopia-api'


    export var storyboard = (
      <Storyboard data-uid='sb'>
        <span
          data-testid='span'
          style={{
            position: 'absolute',
            wordBreak: 'break-word',
            left: 0,
            top: 0,
            width: 'max-content',
            height: 'max-content',
          }}
        >
          Hello
        </span>
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
