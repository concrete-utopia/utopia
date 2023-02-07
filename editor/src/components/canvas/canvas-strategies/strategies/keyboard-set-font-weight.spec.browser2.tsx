import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../../event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../../ui-jsx.test-utils'

describe('adjust font weight with the keyboard', () => {
  describe('no font weight specified', () => {
    it('increase font weight', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithNoFontWeight,
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 1, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('500')
    })

    it('increase font weight multiple times', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithNoFontWeight,
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 2, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('600')
    })

    it('try to increase font weight above the limit', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithNoFontWeight,
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 6, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('900')
    })

    it('decrease font weight', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithNoFontWeight,
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 1 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('300')
    })

    it('decrease font weight multiple times', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithNoFontWeight,
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 3 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('100')
    })

    it('try to increase font weight below the limit', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithNoFontWeight,
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 9 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('100')
    })
  })

  describe('font weight already set', () => {
    it('increase font weight', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontWeight('500'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 1, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('600')
    })

    it('increase font weight multiple times', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontWeight('500'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 3, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('800')
    })

    it('decrease font weight', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontWeight('400'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 1 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('300')
    })

    it('decrease font weight multiple times', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontWeight('500'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 3 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontWeight).toEqual('200')
    })
  })
})

async function doSelect(editor: EditorRenderResult) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('div')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  await mouseClickAtPoint(canvasControlsLayer, divCorner)
  await editor.getDispatchFollowUpActionsFinished()
}

async function doTestWithDelta(
  editor: EditorRenderResult,
  delta: { decreaseBy: number; increaseBy: number },
) {
  for (let i = 0; i < delta.increaseBy; i++) {
    await pressKey('.', { modifiers: { shift: false, cmd: true, alt: true, ctrl: false } })
  }

  for (let i = 0; i < delta.decreaseBy; i++) {
    await pressKey(',', { modifiers: { shift: false, cmd: true, alt: true, ctrl: false } })
  }

  await editor.getDispatchFollowUpActionsFinished()
}

const projectWithNoFontWeight = `import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-testid='div'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 377,
        top: 271,
        width: 288,
        height: 362,
      }}
      data-uid='39e'
    >
      hello
    </div>
  </Storyboard>
)
`

const projectWithFontWeight = (fontWeight: string) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-testid='div'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 377,
        top: 271,
        width: 288,
        height: 362,
        fontWeight: '${fontWeight}'
      }}
      data-uid='39e'
    >
      hello
    </div>
  </Storyboard>
)
`
