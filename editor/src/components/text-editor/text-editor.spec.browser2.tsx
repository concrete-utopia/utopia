import { setFeatureEnabled } from '../../utils/feature-switches'
import { cmdModifier, Modifiers, shiftCmdModifier } from '../../utils/modifiers'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { TextEditorSpanId } from './text-editor'

describe('Use the text editor', () => {
  before(() => {
    setFeatureEnabled('Text editing', true)
  })
  it('Edit existing text', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

    await enterTextEditMode(editor)
    typeText(' Utopia')
    closeTextEditor()
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
    closeTextEditor()
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
    closeTextEditor()
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
            >this is a &lt;test&gt; with bells &amp; whistles</div>
          </Storyboard>
        )`),
    )
  })
  describe('formatting shortcuts', () => {
    it('supports bold', async () => {
      const { before, after } = await testModifier(cmdModifier, 'b')
      expect(before).toEqual(projectWithStyle('fontWeight', 'bold'))
      expect(after).toEqual(projectWithStyle('fontWeight', 'normal'))
    })

    it('supports italic', async () => {
      const { before, after } = await testModifier(cmdModifier, 'i')
      expect(before).toEqual(projectWithStyle('fontStyle', 'italic'))
      expect(after).toEqual(projectWithStyle('fontStyle', 'normal'))
    })

    it('supports underline', async () => {
      const { before, after } = await testModifier(cmdModifier, 'u')
      expect(before).toEqual(projectWithStyle('textDecoration', 'underline'))
      expect(after).toEqual(projectWithStyle('textDecoration', 'none'))
    })

    it('supports strikethrough', async () => {
      const { before, after } = await testModifier(shiftCmdModifier, 'x')
      expect(before).toEqual(projectWithStyle('textDecoration', 'line-through'))
      expect(after).toEqual(projectWithStyle('textDecoration', 'none'))
    })

    it("doesn't care about selection", async () => {
      const editor = await renderTestEditorWithCode(projectWithoutText, 'await-first-dom-report')
      await prepareTestModifierEditor(editor)

      const textEditorElement = document.getElementById(TextEditorSpanId)
      expect(textEditorElement).not.toBe(null)
      if (textEditorElement != null) {
        const sel = document.createRange()
        sel.collapse()
        sel.selectNodeContents(textEditorElement)
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
        projectWithStyle('fontWeight', 'bold'),
      )
    })
  })
})

function projectWithStyle(prop: string, value: string) {
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
                ${prop}: '${value}'
              }}
              data-uid='39e'
            >Hello Utopia</div>
          </Storyboard>
        )`)
}

async function prepareTestModifierEditor(editor: EditorRenderResult) {
  await enterTextEditMode(editor)
  typeText('Hello Utopia')
}

async function pressShortcut(editor: EditorRenderResult, mod: Modifiers, key: string) {
  pressKey(key, {
    modifiers: mod,
    targetElement: document.getElementById(TextEditorSpanId) ?? undefined,
  })
  closeTextEditor()
  await editor.getDispatchFollowUpActionsFinished()
}

async function testModifier(mod: Modifiers, key: string) {
  const editor = await renderTestEditorWithCode(projectWithoutText, 'await-first-dom-report')

  await prepareTestModifierEditor(editor)
  await pressShortcut(editor, mod, key)
  const before = getPrintedUiJsCode(editor.getEditorState())

  await enterTextEditMode(editor)
  await pressShortcut(editor, mod, key)
  const after = getPrintedUiJsCode(editor.getEditorState())

  return { before, after }
}

async function enterTextEditMode(editor: EditorRenderResult) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('div')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner)
  await editor.getDispatchFollowUpActionsFinished()
  pressKey('t')
  await editor.getDispatchFollowUpActionsFinished()
}

function typeText(text: string) {
  document.execCommand('insertText', false, text)
}

function closeTextEditor() {
  pressKey('Escape')
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
