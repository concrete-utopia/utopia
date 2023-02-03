import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../../event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../../ui-jsx.test-utils'

describe('adjust opacity with the keyboard', () => {
  describe('no opacity specified', () => {
    it('entering characters', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      pressKey('a')
      pressKey('b')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('') // nothing happens
    })

    it('entering character and digit', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      pressKey('u')
      pressKey('4')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.4')
    })

    it('entering digit and character', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      pressKey('4')
      pressKey('u')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.4')
    })

    it('entering 3-4', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      pressKey('3')
      pressKey('4')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.34')
    })

    it('entering 3-0', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      pressKey('3')
      pressKey('0')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.3')
    })

    it('entering 0-0', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      pressKey('0')
      pressKey('0')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0')
    })

    it('entering 1-0-0', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      pressKey('1')
      pressKey('0')
      pressKey('0')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('1')
    })

    it('enter 4-2-0', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      pressKey('4')
      pressKey('2')
      pressKey('0')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.2')
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

const project = `import * as React from 'react'
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
