import * as EP from '../../../../core/shared/element-path'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import type { FragmentLikeType } from './fragment-like-helpers'
import { AllFragmentLikeTypes } from './fragment-like-helpers'
import {
  getClosingFragmentLikeTag,
  getOpeningFragmentLikeTag,
  FragmentLikeElementUid,
} from './fragment-like-helpers.test-utils'

describe('adjust opacity with the keyboard', () => {
  describe('no opacity specified', () => {
    it('entering characters', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await pressKey('a')
      await pressKey('b')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('') // nothing happens
    })

    it('entering character and digit', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await pressKey('u')
      await pressKey('4')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.4')
    })

    it('entering digit and character', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await pressKey('4')
      await pressKey('u')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.4')
    })

    it('entering 3-4', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await pressKey('3')
      await pressKey('4')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.34')
    })

    it('entering 3-0', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await pressKey('3')
      await pressKey('0')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.3')
    })

    it('entering 0-0', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await pressKey('0')
      await pressKey('0')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0')
    })

    it('entering 1-0-0', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await pressKey('1')
      await pressKey('0')
      await pressKey('0')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('1')
    })

    it('enter 4-2-0', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId('div')
      await doSelect(editor)
      await pressKey('4')
      await pressKey('2')
      await pressKey('0')
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.opacity).toEqual('0.2')
    })
  })

  describe('retargets to group children', () => {
    AllFragmentLikeTypes.forEach((type) => {
      it(`applies opacity to ${type}`, async () => {
        const editor = await renderTestEditorWithCode(
          projectWithGroup(type),
          'await-first-dom-report',
        )
        await selectComponentsForTest(editor, [EP.fromString(`sb/${FragmentLikeElementUid}`)])

        await pressKey('3')
        await pressKey('0')
        await editor.getDispatchFollowUpActionsFinished()

        const aaa = editor.renderedDOM.getByTestId('aaa')
        const bbb = editor.renderedDOM.getByTestId('bbb')

        expect(aaa.style.opacity).toEqual('0.3')
        expect(bbb.style.opacity).toEqual('0.3')
      })
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

const projectWithGroup = (type: FragmentLikeType) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    ${getOpeningFragmentLikeTag(type)}
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 73,
          height: 109,
          left: 8,
          top: 210,
          position: 'absolute',
        }}
        data-uid='aaa'
        data-testid='aaa'
      >
        whaddup
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 207,
          height: 202,
          left: 8,
          top: 8,
          position: 'absolute',
        }}
        data-uid='aab'
        data-testid='bbb'
      >
        whaddup
      </div>
    ${getClosingFragmentLikeTag(type)}
  </Storyboard>
)
`
