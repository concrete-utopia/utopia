import * as EP from '../../core/shared/element-path'
import { expectSingleUndo2Saves, selectComponentsForTest } from '../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import type { FlexDirection } from './common/css-utils'
import type { FlexAlignment } from './inspector-common'
import { ThreeBarControlTestId } from './three-bar-control'

const StoryboardId = 'sb'
const SceneId = 'sc'
const ParentId = 'p'

describe('three bar control', () => {
  describe('set align-items in flex-direcion: column', () => {
    it('align-items: start', async () => {
      const editor = await renderTestEditorWithCode(project('row'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'flex-start'
      const div = await doTest(editor, ThreeBarControlTestId(desiredAlignItems))
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })

    it('align-items: center', async () => {
      const editor = await renderTestEditorWithCode(project('row'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'center'
      const div = await doTest(editor, ThreeBarControlTestId(desiredAlignItems))
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })

    it('align-items: end', async () => {
      const editor = await renderTestEditorWithCode(project('row'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'flex-end'
      const div = await doTest(editor, ThreeBarControlTestId(desiredAlignItems))
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })
  })

  describe('set align-items in flex-direcion: row', () => {
    it('align-items: start', async () => {
      const editor = await renderTestEditorWithCode(project('column'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'flex-start'
      const div = await doTest(editor, ThreeBarControlTestId(desiredAlignItems))
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })

    it('align-items: center', async () => {
      const editor = await renderTestEditorWithCode(project('column'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'center'
      const div = await doTest(editor, ThreeBarControlTestId(desiredAlignItems))
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })

    it('align-items: end', async () => {
      const editor = await renderTestEditorWithCode(project('column'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'flex-end'
      const div = await doTest(editor, ThreeBarControlTestId(desiredAlignItems))
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })
  })
})

async function doTest(editor: EditorRenderResult, controlId: string): Promise<HTMLElement> {
  await selectComponentsForTest(editor, [EP.fromString(`${StoryboardId}/${SceneId}/${ParentId}`)])

  const div = editor.getRenderedCanvas().getByTestId(ParentId)
  const control = editor.renderedDOM.getByTestId(controlId)
  const controlBounds = control.getBoundingClientRect()
  await expectSingleUndo2Saves(editor, async () => {
    await mouseClickAtPoint(control, { x: controlBounds.top + 10, y: controlBounds.left + 10 })
  })
  return div
}

const project = (flexDirection: FlexDirection) => `import * as React from 'react'
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
          flexDirection: '${flexDirection}',
          justifyContent: 'space-between',
          alignItems: 'flex-start',
          border: '1px solid rgb(0, 0, 0, 1)',
          padding: '30px 30px 30px 30px',
        }}
        data-testid='${ParentId}'
        data-uid='${ParentId}'
      >
        <div
          style={{
            backgroundColor: '#49b6ff',
            width: 64,
            height: 107,
            contain: 'layout',
            borderRadius: 19,
          }}
          data-uid='996'
        />
        <div
          style={{
            backgroundColor: '#49b6ff',
            width: 101,
            height: 145,
            contain: 'layout',
            borderRadius: 51,
          }}
          data-uid='ff4'
        />
        <div
          style={{
            backgroundColor: '#49b6ff',
            contain: 'layout',
            height: 119,
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
