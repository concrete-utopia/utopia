import { cmdModifier } from '../../utils/modifiers'
import { wait } from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'

describe('Fill / Fixed / Hug control', () => {
  it('test', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const div = await selectDiv(editor)
    await wait(10000)
  })
})

async function selectDiv(editor: EditorRenderResult): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('mydiv')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + divBounds.width - 1,
    y: divBounds.y + 1,
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

  return div
}

function project() {
  return `import * as React from 'react'
    import { Storyboard } from 'utopia-api'
    
    export var storyboard = (
      <Storyboard data-uid='0cd'>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 133,
            top: 228,
            width: 342,
            height: 368,
            alignItems: 'flex-end',
            justifyContent: 'center',
            display: 'flex',
          }}
          data-uid='5f9'
        >
          <div
            data-testid='mydiv'
            style={{
              backgroundColor: '#aaaaaa33',
              width: 251,
              height: 263,
            }}
            data-uid='9e4'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 132,
                height: 143,
                contain: 'layout',
              }}
              data-uid='b01'
            />
          </div>
        </div>
      </Storyboard>
    )
`
}
