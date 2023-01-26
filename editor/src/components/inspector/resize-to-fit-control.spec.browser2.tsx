import { setFeatureEnabled } from '../../utils/feature-switches'
import { wait } from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import {
  expectSingleUndoStep,
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
} from '../canvas/event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { MaxContent } from './inspector-common'
import { ResizeToFitControlTestId } from './resize-to-fit-control'

describe('Resize to fit control', () => {
  before(() => setFeatureEnabled('Nine block control', true))
  after(() => setFeatureEnabled('Nine block control', false))

  it('resizes to fit', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')
    const view = await selectView(editor)
    await expectSingleUndoStep(editor, () => clickResizeToFit(editor))

    expect(view.style.width).toEqual(MaxContent)
    expect(view.style.minWidth).toEqual('')
    expect(view.style.maxWidth).toEqual('')
    expect(view.style.height).toEqual(MaxContent)
    expect(view.style.minHeight).toEqual('')
    expect(view.style.maxHeight).toEqual('')
  })
})

const ViewTestId = 'view'

async function selectView(editor: EditorRenderResult): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const view = editor.renderedDOM.getByTestId(ViewTestId)
  const viewBounds = view.getBoundingClientRect()
  const viewCorner = {
    x: viewBounds.x + 50,
    y: viewBounds.y + 40,
  }

  mouseDoubleClickAtPoint(canvasControlsLayer, viewCorner)

  return view
}

async function clickResizeToFit(editor: EditorRenderResult) {
  const resizeToFitControl = editor.renderedDOM.getByTestId(ResizeToFitControlTestId)

  mouseClickAtPoint(resizeToFitControl, { x: 2, y: 2 })
}

const project = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { View } from 'utopia-api'

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
      <View
        data-testid='${ViewTestId}'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 181,
          top: 118,
          width: 325,
          height: 294,
          display: 'flex',
        }}
        data-uid='b51'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 159,
            height: 154,
            contain: 'layout',
          }}
          data-uid='aae'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 62,
            height: 101,
            contain: 'layout',
          }}
          data-uid='733'
        />
      </View>
    </Scene>
  </Storyboard>
)
`
