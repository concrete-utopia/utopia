import { expectSingleUndo2Saves } from '../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint } from '../../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../../canvas/ui-jsx.test-utils'
import { removeLayout } from '../layout-systems.test-utils'

describe('remove-flex-convert-to-absolute strategy', () => {
  it('remove flex layout', async () => {
    const editor = await renderTestEditorWithCode(flexProject(), 'await-first-dom-report')
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

  it('remove grid layout', async () => {
    const editor = await renderTestEditorWithCode(gridProject(), 'await-first-dom-report')

    const root = await selectDiv(editor)

    await expectSingleUndo2Saves(editor, async () => {
      await removeLayout(editor)
    })

    expect(root.style.display).toEqual('')
    expect(root.style.alignItems).toEqual('')
    expect(root.style.justifyContent).toEqual('')
    expect(root.style.gap).toEqual('')
    expect(root.style.paddingTop).toEqual('10px')
    expect(root.style.paddingBottom).toEqual('10px')
    expect(root.style.width).toEqual('540px')
    expect(root.style.height).toEqual('420px')

    const one = editor.renderedDOM.getByTestId('one')
    expect(one.style.position).toEqual('absolute')
    expect(one.style.left).toEqual('187px')
    expect(one.style.top).toEqual('147px')
    expect(one.style.height).toEqual('264px')
    expect(one.style.width).toEqual('167px')
    expect(one.style.gridColumn).toEqual('')
    expect(one.style.gridColumnStart).toEqual('')
    expect(one.style.gridColumnEnd).toEqual('')
    expect(one.style.gridRow).toEqual('')
    expect(one.style.gridRowStart).toEqual('')
    expect(one.style.gridRowEnd).toEqual('')

    const two = editor.renderedDOM.getByTestId('two')
    expect(two.style.position).toEqual('absolute')
    expect(two.style.left).toEqual('10px')
    expect(two.style.top).toEqual('10px')
    expect(two.style.height).toEqual('127px')
    expect(two.style.width).toEqual('520px')
    expect(two.style.gridColumn).toEqual('')
    expect(two.style.gridColumnStart).toEqual('')
    expect(two.style.gridColumnEnd).toEqual('')
    expect(two.style.gridRow).toEqual('')
    expect(two.style.gridRowStart).toEqual('')
    expect(two.style.gridRowEnd).toEqual('')
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

function flexProject(): string {
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

function gridProject(): string {
  return `
import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-uid='root'
      data-testid='root'
      style={{
        backgroundColor: '#fff',
        position: 'absolute',
        width: 540,
        height: 420,
        gap: 10,
        display: 'grid',
        gridTemplateColumns: '1fr 1fr 1fr',
        gridTemplateRows: '1fr 1fr 1fr',
        padding: 10,
      }}
    >
      <div
        data-uid='one'
        data-testid='one'
        style={{
          backgroundColor: '#f90',
          gridRow: '2 / 4',
          gridColumn: 2,
        }}
      />
      <div
        data-uid='two'
        data-testid='two'
        style={{
          backgroundColor: '#09f',
          gridRowStart: 1,
          gridRowEnd: 2,
          gridColumnStart: 1,
          gridColumnEnd: 4,
        }}
      />
    </div>
  </Storyboard>
)
`
}
