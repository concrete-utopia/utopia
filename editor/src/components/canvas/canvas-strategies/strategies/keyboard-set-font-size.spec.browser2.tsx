import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../../event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../../ui-jsx.test-utils'

describe('adjust font size with the keyboard', () => {
  describe('no font size specified', () => {
    it('increase font size', async () => {
      const editor = await renderTestEditorWithCode(projectWithNoFontSize, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 1, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('17px')
    })

    it('increase font size multiple times', async () => {
      const editor = await renderTestEditorWithCode(projectWithNoFontSize, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 5, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('21px')
    })

    it('decrease font size', async () => {
      const editor = await renderTestEditorWithCode(projectWithNoFontSize, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 1 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('15px')
    })

    it('decrease font size multiple times', async () => {
      const editor = await renderTestEditorWithCode(projectWithNoFontSize, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 5 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('11px')
    })
  })

  describe('font size already set', () => {
    it('increase font size', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontSize('20px'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 1, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('21px')
    })

    it('increase font size multiple times', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontSize('20px'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 5, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('25px')
    })

    it('decrease font size', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontSize('20px'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 1 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('19px')
    })

    it('decrease font size multiple times', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontSize('20px'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 5 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('15px')
    })
  })
  describe('font size in em units', () => {
    it('increase font size', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontSize('2em'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 1, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('2.1em')
    })

    it('decrease font size', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFontSize('2em'),
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await doTestWithDelta(editor, { increaseBy: 0, decreaseBy: 1 })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.fontSize).toEqual('1.9em')
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

  mouseClickAtPoint(canvasControlsLayer, divCorner)
  await editor.getDispatchFollowUpActionsFinished()
}

async function doTestWithDelta(
  editor: EditorRenderResult,
  delta: { decreaseBy: number; increaseBy: number },
) {
  for (let i = 0; i < delta.increaseBy; i++) {
    pressKey('.', { modifiers: { shift: true, cmd: true, alt: false, ctrl: false } })
  }

  for (let i = 0; i < delta.decreaseBy; i++) {
    pressKey(',', { modifiers: { shift: true, cmd: true, alt: false, ctrl: false } })
  }

  await editor.getDispatchFollowUpActionsFinished()
}

const projectWithNoFontSize = `import * as React from 'react'
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

const projectWithFontSize = (fontSize: string) => `import * as React from 'react'
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
        fontSize: '${fontSize}'
      }}
      data-uid='39e'
    >
      hello
    </div>
  </Storyboard>
)
`
