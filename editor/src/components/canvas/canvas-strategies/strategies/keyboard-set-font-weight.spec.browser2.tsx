import { fromString } from '../../../../core/shared/element-path'
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

  describe('retargets to group children', () => {
    AllFragmentLikeTypes.forEach((type) => {
      it(`sets font weight in ${type}`, async () => {
        const editor = await renderTestEditorWithCode(
          projectWithGroup(type),
          'await-first-dom-report',
        )

        await selectComponentsForTest(editor, [fromString(`sb/${FragmentLikeElementUid}`)])
        await doTestWithDelta(editor, { increaseBy: 1, decreaseBy: 0 })

        const descriptiveLabel =
          editor.getEditorState().strategyState.currentStrategyDescriptiveLabel
        expect(descriptiveLabel).toEqual('Changing Font Weight (Children)')

        await editor.getDispatchFollowUpActionsFinished()

        const aaa = editor.renderedDOM.getByTestId('aaa')
        const bbb = editor.renderedDOM.getByTestId('bbb')

        expect(aaa.style.fontWeight).toEqual('500')
        expect(bbb.style.fontWeight).toEqual('500')
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

async function doTestWithDelta(
  editor: EditorRenderResult,
  delta: { decreaseBy: number; increaseBy: number },
) {
  for (const _ of Array(delta.increaseBy)) {
    await pressKey('.', { modifiers: { shift: false, cmd: true, alt: true, ctrl: false } })
  }

  for (const _ of Array(delta.decreaseBy)) {
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
