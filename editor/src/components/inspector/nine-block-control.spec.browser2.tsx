import { setFeatureEnabled } from '../../utils/feature-switches'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { expectSingleUndoStep, mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { StartCenterEnd } from './inspector-common'
import { NineBlockSectors, NineBlockTestId } from './nine-block-controls'

describe('Nine-block control', () => {
  before(() => {
    setFeatureEnabled('Nine block control', true)
  })

  after(() => {
    setFeatureEnabled('Nine block control', false)
  })

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
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner)

  const nineBlockControlSegment = editor.renderedDOM.getByTestId(
    NineBlockTestId(justifyContent, alignItems),
  )

  await expectSingleUndoStep(editor, async () =>
    mouseClickAtPoint(nineBlockControlSegment, { x: 2, y: 2 }),
  )

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
