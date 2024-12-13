import * as EP from '../../core/shared/element-path'
import {
  expectSingleUndo2Saves,
  hoverControlWithCheck,
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../utils/utils.test-utils'
import { getSubduedPaddingControlTestID } from '../canvas/controls/select-mode/subdued-padding-control'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { renderTestEditorWithCode, renderTestEditorWithModel } from '../canvas/ui-jsx.test-utils'
import { TailwindProject } from './sections/flex-section.test-utils'
import {
  PackedLabelCopy,
  SpacedLabelCopy,
  SpacedPackedControlTestId,
} from './spaced-packed-control'

type SpacedPackedButtonLabel = typeof PackedLabelCopy | typeof SpacedLabelCopy

const StoryboardId = 'sb'
const SceneId = 'sc'
const ParentId = 'p'

describe('spaced - packed control', () => {
  it('set element to spaced layout', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString(`${StoryboardId}/${SceneId}/${ParentId}`)])

    const div = editor.renderedDOM.getByTestId(ParentId)
    expect(div.style.gap).toEqual('48px')

    await expectSingleUndo2Saves(editor, async () => {
      await clickButton(editor, SpacedLabelCopy)
    })
    expect(div.style.gap).toEqual('')
    expect(div.style.justifyContent).toEqual('space-between')
  })

  it('set element to packed layout', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString(`${StoryboardId}/${SceneId}/${ParentId}`)])

    const div = editor.renderedDOM.getByTestId(ParentId)

    await expectSingleUndo2Saves(editor, () => clickButton(editor, SpacedLabelCopy))
    await expectSingleUndo2Saves(editor, () => clickButton(editor, PackedLabelCopy))
    expect(div.style.justifyContent).toEqual('flex-start')
  })

  describe('Tailwind', () => {
    setFeatureForBrowserTestsUseInDescribeBlockOnly('Tailwind', true)
    it('set element to spaced layout', async () => {
      const editor = await renderTestEditorWithModel(
        TailwindProject('flex flex-row'),
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/scene/mydiv')])
      await expectSingleUndo2Saves(editor, () => clickButton(editor, SpacedLabelCopy))
      const div = editor.renderedDOM.getByTestId('mydiv')
      expect(div.className).toEqual(
        'top-10 left-10 w-64 h-64 bg-slate-100 absolute flex flex-row justify-between',
      )
    })
    it('set element to packed layout', async () => {
      const editor = await renderTestEditorWithModel(
        TailwindProject('flex flex-row'),
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/scene/mydiv')])
      await expectSingleUndo2Saves(editor, () => clickButton(editor, SpacedLabelCopy))
      await expectSingleUndo2Saves(editor, () => clickButton(editor, PackedLabelCopy))
      const div = editor.renderedDOM.getByTestId('mydiv')
      expect(div.className).toEqual(
        'top-10 left-10 w-64 h-64 bg-slate-100 absolute flex flex-row justify-start',
      )
    })
  })

  it('when spaced/packed control is hovered, padding hihglights are shown', async () => {
    const editor = await renderTestEditorWithCode(projectWithPadding, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/div')])
    await hoverControlWithCheck(editor, SpacedPackedControlTestId, async () => {
      const controls = [
        getSubduedPaddingControlTestID('top', 'hovered'),
        getSubduedPaddingControlTestID('bottom', 'hovered'),
        getSubduedPaddingControlTestID('left', 'hovered'),
        getSubduedPaddingControlTestID('right', 'hovered'),
      ].flatMap((id) => editor.renderedDOM.queryAllByTestId(id))

      expect(controls.length).toEqual(4)
    })
  })
})

async function clickButton(editor: EditorRenderResult, button: SpacedPackedButtonLabel) {
  const buttonElement = editor.renderedDOM.getByText(button)
  const buttonBounds = buttonElement.getBoundingClientRect()
  await mouseClickAtPoint(buttonElement, { x: buttonBounds.top + 10, y: buttonBounds.left + 10 })
}

const project = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='${StoryboardId}'>
    <Scene
      style={{
        width: 694,
        height: 759,
        position: 'absolute',
        left: 208,
        top: 127,
      }}
      data-label='Playground'
      data-uid='${SceneId}'
    >
      <div
        style={{
          position: 'absolute',
          left: 9,
          top: 93,
          width: 611,
          height: 433,
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'center',
          alignItems: 'center',
          border: '1px solid rgb(0, 0, 0, 1)',
          padding: '30px 30px 30px 30px',
          gap: 48,
        }}
        data-testid='${ParentId}'
        data-uid='${ParentId}'
      >
        <div
          style={{
            backgroundColor: '#49b6ff',
            width: 64,
            height: 64,
            contain: 'layout',
            borderRadius: 19,
          }}
          data-uid='996'
        />
        <div
          style={{
            backgroundColor: '#49b6ff',
            width: 101,
            height: 99,
            contain: 'layout',
            borderRadius: 51,
          }}
          data-uid='ff4'
        />
        <div
          style={{
            backgroundColor: '#49b6ff',
            contain: 'layout',
            height: 75,
            borderRadius: 22,
            width: 118,
          }}
          data-uid='aab'
        />
      </div>
    </Scene>
  </Storyboard>
)
`

const projectWithPadding = `import * as React from 'react'
import { Scene, Storyboard, FlexCol } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -189,
        top: 34,
        width: 326,
        height: 168,
        display: 'flex',
        padding: 20,
      }}
      data-uid='div'
    />
  </Storyboard>
)
`
