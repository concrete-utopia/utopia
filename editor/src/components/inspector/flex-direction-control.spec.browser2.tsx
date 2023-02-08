import { setFeatureEnabled } from '../../utils/feature-switches'
import { expectSingleUndoStep } from '../../utils/utils.test-utils'
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

    await expectSingleUndoStep(editor, async () => {
      await clickOn(editor, 'row')
    })

    expect(div.style.flexDirection).toEqual('row')

    await rightClickOn(editor, 'row')

    expect(div.style.flexDirection).toEqual('')
  })

  it('set flex direction to column from row', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const div = await selectDiv(editor)
    await clickOn(editor, 'row')

    expect(div.style.flexDirection).toEqual('row')

    await expectSingleUndoStep(editor, async () => {
      await clickOn(editor, 'column')
    })

    expect(div.style.flexDirection).toEqual('column')
  })

  it('set flex direction to column from not set', async () => {
    const editor = await renderTestEditorWithCode(project(), 'await-first-dom-report')
    const div = await selectDiv(editor)
    await clickOn(editor, 'column')

    expect(div.style.flexDirection).toEqual('column')

    await expectSingleUndoStep(editor, async () => {
      await rightClickOn(editor, 'column')
    })

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

  it('when updating flex direction, and children are set to fill container, cross axis sizings are swapped', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithFillContainerChildren(),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await clickOn(editor, 'column')

    expect(div.style.flexDirection).toEqual('column')

    const blue = editor.renderedDOM.getByTestId('blue')
    expect(blue.style.flex).toEqual('')
    expect(blue.style.height).toEqual('')
    expect(blue.style.width).toEqual('170px')
    expect(blue.style.flexGrow).toEqual('1')

    const red = editor.renderedDOM.getByTestId('red')
    expect(red.style.flex).toEqual('')
    expect(red.style.height).toEqual('')
    expect(red.style.width).toEqual('211px')
    expect(red.style.flexGrow).toEqual('1')

    const green = editor.renderedDOM.getByTestId('green')
    expect(green.style.flex).toEqual('')
    expect(green.style.height).toEqual('')
    expect(green.style.width).toEqual('188px')
    expect(green.style.flexGrow).toEqual('1')
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

  await mouseClickAtPoint(canvasControlsLayer, divCorner)

  return div
}

async function clickOn(editor: EditorRenderResult, direction: FlexDirection) {
  const flexDirectionToggle = editor.renderedDOM.getByTestId(FlexDirectionToggleTestId(direction))

  await mouseClickAtPoint(flexDirectionToggle, { x: 2, y: 2 })
}

async function rightClickOn(editor: EditorRenderResult, direction: FlexDirection) {
  const flexDirectionToggle = editor.renderedDOM.getByTestId(FlexDirectionToggleTestId(direction))

  await mouseClickAtPoint(flexDirectionToggle, { x: 2, y: 2 }, { eventOptions: { button: 1 } })
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

function projectWithFillContainerChildren(): string {
  return `import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='0cd'>
      <div
        data-testid='mydiv'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 38,
          top: 159,
          width: 999,
          height: 634,
          display: 'flex',
          flexDirection: 'row',
        }}
        data-uid='9f5'
      >
        <div
          data-testid='blue'
          style={{
            backgroundColor: '#00acff',
            height: 170,
            flexGrow: '1',
          }}
          data-uid='b5c'
        />
        <div
          data-testid='red'
          style={{
            backgroundColor: '#fe4400',
            height: 211,
            flexGrow: '1',
          }}
          data-uid='e8f'
        />
        <div
          data-testid='green'
          style={{
            backgroundColor: '#669f39',
            height: 188,
            flexGrow: '1',
          }}
          data-uid='e8d'
        />
      </div>
    </Storyboard>
  )
  `
}
