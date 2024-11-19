import * as EP from '../../core/shared/element-path'
import {
  expectSingleUndo2Saves,
  hoverControlWithCheck,
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { getSubduedPaddingControlTestID } from '../canvas/controls/select-mode/subdued-padding-control'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { renderTestEditorWithCode, renderTestEditorWithModel } from '../canvas/ui-jsx.test-utils'
import type { StartCenterEnd } from './inspector-common'
import { NineBlockControlTestId, NineBlockSectors, NineBlockTestId } from './nine-block-controls'
import {
  AlignItemsClassMapping,
  JustifyContentClassMapping,
  TailwindProject,
} from './sections/flex-section.test-utils'

describe('Nine-block control', () => {
  describe('in flex row', () => {
    for (const [justifyContent, alignItems] of NineBlockSectors) {
      it(`set ${justifyContent} and ${alignItems} via the nine-block control`, async () => {
        const editor = await renderTestEditorWithCode(projectFlexRow(), 'await-first-dom-report')
        const div = await doTest(editor, alignItems, justifyContent)
        expect(div.style.justifyContent).toEqual(justifyContent)
        expect(div.style.alignItems).toEqual(alignItems)
      })
    }
  })

  describe('in flex column', () => {
    for (const [justifyContent, alignItems] of NineBlockSectors) {
      it(`set ${justifyContent} and ${alignItems} via the nine-block control`, async () => {
        const editor = await renderTestEditorWithCode(projectFlexColumn(), 'await-first-dom-report')
        const div = await doTest(editor, justifyContent, alignItems)
        expect(div.style.justifyContent).toEqual(justifyContent)
        expect(div.style.alignItems).toEqual(alignItems)
      })
    }
  })

  it('when nine-block control is hovered, padding hihglights are shown', async () => {
    const editor = await renderTestEditorWithCode(projectWithPadding, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/div')])
    await hoverControlWithCheck(editor, NineBlockControlTestId, async () => {
      const controls = [
        getSubduedPaddingControlTestID('top', 'hovered'),
        getSubduedPaddingControlTestID('bottom', 'hovered'),
        getSubduedPaddingControlTestID('left', 'hovered'),
        getSubduedPaddingControlTestID('right', 'hovered'),
      ].flatMap((id) => editor.renderedDOM.queryAllByTestId(id))

      expect(controls.length).toEqual(4)
    })
  })

  describe('Tailwind', () => {
    setFeatureForBrowserTestsUseInDescribeBlockOnly('Tailwind', true)

    for (const [justifyContent, alignItems] of NineBlockSectors) {
      it(`set ${justifyContent} and ${alignItems} via the nine-block control`, async () => {
        const editor = await renderTestEditorWithModel(
          TailwindProject('flex flex-row'),
          'await-first-dom-report',
        )

        const div = await doTest(editor, alignItems, justifyContent)

        expect(getComputedStyle(div).justifyContent).toEqual(justifyContent)
        expect(getComputedStyle(div).alignItems).toEqual(alignItems)
        expect(div.className).toEqual(
          `top-10 left-10 w-64 h-64 bg-slate-100 absolute flex flex-row ${AlignItemsClassMapping[alignItems]} ${JustifyContentClassMapping[justifyContent]}`,
        )
      })
    }
  })
})

async function doTest(
  editor: EditorRenderResult,
  justifyContent: StartCenterEnd,
  alignItems: StartCenterEnd,
): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 5,
    y: divBounds.y + 4,
  }

  await mouseClickAtPoint(canvasControlsLayer, divCorner)

  const nineBlockControlSegment = editor.renderedDOM.getByTestId(
    NineBlockTestId(justifyContent, alignItems),
  )

  await expectSingleUndo2Saves(editor, async () => {
    await mouseClickAtPoint(nineBlockControlSegment, { x: 2, y: 2 })
  })

  return div
}

function projectFlexRow(): string {
  return `import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='0cd'>
      <div
        data-testid='mydiv'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 133,
          top: 228,
          width: 342,
          height: 368,
          display: 'flex',
        }}
        data-uid='5f9'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 195,
            height: 190,
          }}
          data-uid='9e4'
        />
      </div>
    </Storyboard>
  )
  `
}

function projectFlexColumn(): string {
  return `import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='0cd'>
      <div
        data-testid='mydiv'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 133,
          top: 228,
          width: 342,
          height: 368,
          display: 'flex',
          flexDirection: 'column',
        }}
        data-uid='5f9'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 195,
            height: 190,
          }}
          data-uid='9e4'
        />
      </div>
    </Storyboard>
  )
  `
}

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
