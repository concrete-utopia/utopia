import * as EP from '../../core/shared/element-path'
import { setFeatureEnabled } from '../../utils/feature-switches'
import { expectSingleUndoStep, selectComponentsForTest } from '../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { PackedLabelCopy, SpacedLabelCopy } from './spaced-packed-control'

type SpacedPackedButtonLabel = typeof PackedLabelCopy | typeof SpacedLabelCopy

const StoryboardId = 'sb'
const SceneId = 'sc'
const ParentId = 'p'

describe('spaced - packed control', () => {
  before(() => setFeatureEnabled('Nine block control', true))
  it('set element to spaced layout', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString(`${StoryboardId}/${SceneId}/${ParentId}`)])

    const div = editor.renderedDOM.getByTestId(ParentId)
    expect(div.style.gap).toEqual('48px')

    await expectSingleUndoStep(editor, async () => {
      await clickButton(editor, SpacedLabelCopy)
    })
    expect(div.style.gap).toEqual('')
    expect(div.style.justifyContent).toEqual('space-between')
  })

  it('set element to packed layout', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString(`${StoryboardId}/${SceneId}/${ParentId}`)])

    const div = editor.renderedDOM.getByTestId(ParentId)

    await expectSingleUndoStep(editor, () => clickButton(editor, SpacedLabelCopy))
    await expectSingleUndoStep(editor, () => clickButton(editor, PackedLabelCopy))
    expect(div.style.justifyContent).toEqual('flex-start')
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
