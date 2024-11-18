import type { ElementPath } from 'utopia-shared/src/types'
import * as EP from '../../core/shared/element-path'
import {
  expectSingleUndo2Saves,
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { renderTestEditorWithCode, renderTestEditorWithModel } from '../canvas/ui-jsx.test-utils'
import type { FlexDirection } from './common/css-utils'
import type { FlexAlignment } from './inspector-common'
import { AlignItemsClassMapping, TailwindProject } from './sections/flex-section.test-utils'
import { ThreeBarControlTestId } from './three-bar-control'

const StoryboardId = 'sb'
const SceneId = 'sc'
const ParentId = 'p'

describe('three bar control', () => {
  describe('set align-items in flex-direcion: column', () => {
    const ElementToSelect = EP.fromString(`${StoryboardId}/${SceneId}/${ParentId}`)

    it('align-items: start', async () => {
      const editor = await renderTestEditorWithCode(project('row'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'flex-start'
      const div = await doTest(
        editor,
        ThreeBarControlTestId(desiredAlignItems),
        ElementToSelect,
        ParentId,
      )
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })

    it('align-items: center', async () => {
      const editor = await renderTestEditorWithCode(project('row'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'center'
      const div = await doTest(
        editor,
        ThreeBarControlTestId(desiredAlignItems),
        ElementToSelect,
        ParentId,
      )
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })

    it('align-items: end', async () => {
      const editor = await renderTestEditorWithCode(project('row'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'flex-end'
      const div = await doTest(
        editor,
        ThreeBarControlTestId(desiredAlignItems),
        ElementToSelect,
        ParentId,
      )
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })
  })

  describe('set align-items in flex-direcion: row', () => {
    const ElementToSelect = EP.fromString(`${StoryboardId}/${SceneId}/${ParentId}`)

    it('align-items: start', async () => {
      const editor = await renderTestEditorWithCode(project('column'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'flex-start'
      const div = await doTest(
        editor,
        ThreeBarControlTestId(desiredAlignItems),
        EP.fromString(`${StoryboardId}/${SceneId}/${ParentId}`),
        ParentId,
      )
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })

    it('align-items: center', async () => {
      const editor = await renderTestEditorWithCode(project('column'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'center'
      const div = await doTest(
        editor,
        ThreeBarControlTestId(desiredAlignItems),
        ElementToSelect,
        ParentId,
      )
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })

    it('align-items: end', async () => {
      const editor = await renderTestEditorWithCode(project('column'), 'await-first-dom-report')
      const desiredAlignItems: FlexAlignment = 'flex-end'
      const div = await doTest(
        editor,
        ThreeBarControlTestId(desiredAlignItems),
        ElementToSelect,
        ParentId,
      )
      expect(div.style.alignItems).toEqual(desiredAlignItems)
    })
  })

  describe('Tailwind', () => {
    setFeatureForBrowserTestsUseInDescribeBlockOnly('Tailwind', true)

    const ElementToSelect = EP.fromString('sb/scene/mydiv')

    const AlignItems = ['flex-start', 'center', 'flex-end'] as const

    for (const alignItems of AlignItems) {
      it(`align-items: ${alignItems}`, async () => {
        const editor = await renderTestEditorWithModel(
          TailwindProject('flex flex-row justify-between'),
          'await-first-dom-report',
        )
        const div = await doTest(
          editor,
          ThreeBarControlTestId(alignItems),
          ElementToSelect,
          'mydiv',
        )
        expect(getComputedStyle(div).alignItems).toEqual(alignItems)
        expect(div.className).toEqual(
          `top-10 left-10 w-64 h-64 bg-slate-100 absolute flex flex-row justify-between ${AlignItemsClassMapping[alignItems]}`,
        )
      })
    }
  })
})

async function doTest(
  editor: EditorRenderResult,
  controlId: string,
  elementPath: ElementPath,
  testId: string,
): Promise<HTMLElement> {
  await selectComponentsForTest(editor, [elementPath])

  const div = editor.renderedDOM.getByTestId(testId)
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
