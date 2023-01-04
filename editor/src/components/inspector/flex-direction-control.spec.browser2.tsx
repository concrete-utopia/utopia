import { setFeatureEnabled } from '../../utils/feature-switches'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { FlexDirection } from './common/css-utils'
import { FlexDirectionToggleTestId } from './flex-direction-control'

describe('set flex direction', () => {
  before(() => {
    setFeatureEnabled('Nine block control', true)
  })

  after(() => {
    setFeatureEnabled('Nine block control', false)
  })

  it('set flex direction to row from not set', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const div = await selectDiv(editor)
    await clickOn(editor, 'row')

    expect(div.style.flexDirection).toEqual('row')

    await rightClickOn(editor, 'row')

    expect(div.style.flexDirection).toEqual('')
  })

  it('set flex direction to column from row', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const div = await selectDiv(editor)
    await clickOn(editor, 'row')

    expect(div.style.flexDirection).toEqual('row')

    await clickOn(editor, 'column')

    expect(div.style.flexDirection).toEqual('column')
  })

  it('set flex direction to column from not set', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const div = await selectDiv(editor)
    await clickOn(editor, 'column')

    expect(div.style.flexDirection).toEqual('column')

    await rightClickOn(editor, 'column')

    expect(div.style.flexDirection).toEqual('')
  })

  it('set flex direction to row from column', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const div = await selectDiv(editor)
    await clickOn(editor, 'column')

    expect(div.style.flexDirection).toEqual('column')

    await clickOn(editor, 'row')

    expect(div.style.flexDirection).toEqual('row')
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

async function clickOn(editor: EditorRenderResult, direction: FlexDirection) {
  const flexDirectionToggle = editor.renderedDOM.getByTestId(FlexDirectionToggleTestId(direction))

  mouseClickAtPoint(flexDirectionToggle, { x: 2, y: 2 })
}

async function rightClickOn(editor: EditorRenderResult, direction: FlexDirection) {
  const flexDirectionToggle = editor.renderedDOM.getByTestId(FlexDirectionToggleTestId(direction))

  mouseClickAtPoint(flexDirectionToggle, { x: 2, y: 2 }, { eventOptions: { button: 1 } })
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
