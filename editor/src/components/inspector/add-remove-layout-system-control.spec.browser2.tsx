import { setFeatureEnabled } from '../../utils/feature-switches'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { renderTestEditorWithCode, EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { AddRemoveLayouSystemControlTestId } from './add-remove-layout-system-control'

describe('add layout system', () => {
  before(() => {
    setFeatureEnabled('Nine block control', true)
  })

  after(() => {
    setFeatureEnabled('Nine block control', false)
  })

  it('add and then remove flex layout', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const div = await selectDiv(editor)
    await clickOn(editor)

    expect(div.style.display).toEqual('flex')

    await clickOn(editor)

    expect(div.style.flexDirection).toEqual('')
  })
})

async function selectDiv(editor: EditorRenderResult): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner)

  return div
}

async function clickOn(editor: EditorRenderResult) {
  const flexDirectionToggle = editor.renderedDOM.getByTestId(AddRemoveLayouSystemControlTestId())

  mouseClickAtPoint(flexDirectionToggle, { x: 2, y: 2 })
}

function project(): string {
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
