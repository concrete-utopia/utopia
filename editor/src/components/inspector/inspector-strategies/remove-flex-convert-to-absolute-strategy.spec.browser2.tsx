import { expectSingleUndo2Saves } from '../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint } from '../../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../../canvas/ui-jsx.test-utils'
import { removeLayout } from '../layout-systems.test-utils'

describe('remove-flex-convert-to-absolute strategy', () => {
  it('remove flex layout', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const root = await selectDiv(editor)

    await expectSingleUndo2Saves(editor, async () => {
      await removeLayout(editor)
    })

    expect(root.style.display).toEqual('')
    expect(root.style.alignItems).toEqual('')
    expect(root.style.justifyContent).toEqual('')
    expect(root.style.gap).toEqual('')
    expect(root.style.paddingTop).toEqual('40px')
    expect(root.style.paddingBottom).toEqual('60px')
    expect(root.style.width).toEqual('548px')
    expect(root.style.height).toEqual('462px')

    const left = editor.renderedDOM.getByTestId('left')
    expect(left.style.position).toEqual('absolute')
    expect(left.style.left).toEqual('0px')
    expect(left.style.top).toEqual('135px')
    expect(left.style.height).toEqual('173px')
    expect(left.style.width).toEqual('259px')

    const right = editor.renderedDOM.getByTestId('right')
    expect(right.style.position).toEqual('absolute')
    expect(right.style.left).toEqual('289px')
    expect(right.style.top).toEqual('40px')
    expect(right.style.height).toEqual('362px')
    expect(right.style.width).toEqual('259px')
  })
})

async function selectDiv(editor: EditorRenderResult): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('root')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  await mouseClickAtPoint(canvasControlsLayer, divCorner)

  return div
}

function project(): string {
  return `import * as React from 'react'
        import { Storyboard } from 'utopia-api'
        
        export var storyboard = (
          <Storyboard data-uid='0cd'>
          <div
          data-testid='root'
          style={{
            backgroundColor: '#3EA881FC',
            position: 'absolute',
            left: 369,
            top: 268,
            width: 548,
            display: 'flex',
            gap: 30,
            alignItems: 'center',
            justifyContent: 'center',
            paddingTop: '40px',
            paddingBottom: '60px',
          }}
          data-uid='3d4'
        >
          <div
          data-testid='left'
            style={{
              backgroundColor: '#E91C1CC4',
              width: '100%',
              height: 173,
              contain: 'layout',
            }}
            data-uid='7bc'
          />
          <div
          data-testid='right'
            style={{
              backgroundColor: '#2C49C9B3',
              width: '100%',
              height: 362,
              contain: 'layout',
            }}
            data-uid='6ff'
          />
        </div>
          </Storyboard>
        )
        `
}
