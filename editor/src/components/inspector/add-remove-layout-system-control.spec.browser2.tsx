import { setFeatureEnabled } from '../../utils/feature-switches'
import { shiftModifier } from '../../utils/modifiers'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import {
  expectSingleUndoStep,
  mouseClickAtPoint,
  pressKey,
} from '../canvas/event-helpers.test-utils'
import { renderTestEditorWithCode, EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { AddRemoveLayouSystemControlTestId } from './add-remove-layout-system-control'

describe('add layout system', () => {
  before(() => {
    setFeatureEnabled('Nine block control', true)
  })

  after(() => {
    setFeatureEnabled('Nine block control', false)
  })

  it('add and remove layout system via keyboard shortcut', async () => {
    const editor = await renderTestEditorWithCode(
      project({ width: '100px', height: '100px' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)

    expect(div.style.display).toEqual('')

    await expectSingleUndoStep(editor, async () => pressKey('a', { modifiers: shiftModifier }))

    expect(div.style.display).toEqual('flex')

    await expectSingleUndoStep(editor, async () => pressKey('a', { modifiers: shiftModifier }))
    expect(div.style.display).toEqual('')
  })

  it('add and then remove flex layout', async () => {
    const editor = await renderTestEditorWithCode(
      project({ width: '100px', height: '100px' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await expectSingleUndoStep(editor, () => clickOn(editor))

    expect(div.style.display).toEqual('flex')

    await expectSingleUndoStep(editor, () => clickOn(editor))

    expect(div.style.display).toEqual('')
  })

  it('adding flex layout converts child `width` to `flexGrow`', async () => {
    const editor = await renderTestEditorWithCode(
      project({ width: '100%', height: '100px' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await expectSingleUndoStep(editor, () => clickOn(editor))

    expect(div.style.display).toEqual('flex')

    const child = editor.renderedDOM.getByTestId('child')
    expect(child.style.width).toEqual('')
    expect(child.style.flexGrow).toEqual('1')
  })

  it('adding flex layout converts child `height` to `flexGrow` if flexDirection is set', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithFlexDirectionColumn({ width: '111px', height: '100%' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await expectSingleUndoStep(editor, () => clickOn(editor))

    expect(div.style.display).toEqual('flex')

    const child = editor.renderedDOM.getByTestId('child')
    expect(child.style.height).toEqual('')
    expect(child.style.flexGrow).toEqual('1')
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

function project({ width, height }: { width: string; height: string }): string {
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
              data-testid='child'
              style={{
                backgroundColor: '#aaaaaa33',
                width: '${width}',
                height: '${height}'
              }}
              data-uid='9e4'
            />
          </div>
        </Storyboard>
      )
      `
}

function projectWithFlexDirectionColumn({
  width,
  height,
}: {
  width: string
  height: string
}): string {
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
              flexDirection: 'column'
            }}
            data-uid='5f9'
          >
            <div
              data-testid='child'
              style={{
                backgroundColor: '#aaaaaa33',
                width: '${width}',
                height: '${height}'
              }}
              data-uid='9e4'
            />
          </div>
        </Storyboard>
      )
      `
}
