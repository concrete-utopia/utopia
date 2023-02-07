import { assertNever } from '../../../../core/shared/utils'
import { shiftCmdModifier } from '../../../../utils/modifiers'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  pressKey,
} from '../../event-helpers.test-utils'
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

    it('increase font size, in a hierarchy', async () => {
      const editor = await renderTestEditorWithCode(projectWithHierarchy, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor, 'double')
      await doTestWithDelta(editor, { increaseBy: 1, decreaseBy: 0 })
      await editor.getDispatchFollowUpActionsFinished()
      expect(div.style.fontSize).toEqual('17px')
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

async function doSelect(editor: EditorRenderResult, type: 'single' | 'double' = 'single') {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('div')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  switch (type) {
    case 'single':
      await mouseClickAtPoint(canvasControlsLayer, divCorner)
      break
    case 'double':
      await mouseDoubleClickAtPoint(canvasControlsLayer, divCorner)
      break
    default:
      assertNever(type)
  }

  await editor.getDispatchFollowUpActionsFinished()
}

async function doTestWithDelta(
  editor: EditorRenderResult,
  delta: { decreaseBy: number; increaseBy: number },
) {
  for (let i = 0; i < delta.increaseBy; i++) {
    await pressKey('.', { modifiers: shiftCmdModifier })
  }

  for (let i = 0; i < delta.decreaseBy; i++) {
    await pressKey(',', { modifiers: shiftCmdModifier })
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

const projectWithHierarchy = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='3fc'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 205,
          height: 241,
        }}
        data-uid='28d'
      />
      <div
        data-testid='div'
        style={{
          backgroundColor: '#aaaaaa33',
          width: 160,
          height: 165,
        }}
        data-uid='05f'
      >
        hello there
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 115,
          height: 125,
        }}
        data-uid='f30'
      />
    </Scene>
    <Scene
      style={{
        width: 744,
        height: 1133,
        position: 'absolute',
        left: 1036,
        top: 128,
      }}
      data-label='My App'
      data-uid='2c5'
    >
      <App style={{}} data-uid='a28' />
    </Scene>
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
