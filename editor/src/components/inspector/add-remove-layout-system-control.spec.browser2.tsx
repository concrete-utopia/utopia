import { shiftModifier } from '../../utils/modifiers'
import { expectSingleUndo2Saves } from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { addFlexLayout, addGridLayout, removeFlexLayout } from './layout-systems.test-utils'

describe('add flex layout', () => {
  it('add and remove layout system via keyboard shortcut', async () => {
    const editor = await renderTestEditorWithCode(
      project({ width: '100px', height: '100px' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)

    expect(div.style.display).toEqual('')

    await expectSingleUndo2Saves(editor, async () => {
      await pressKey('a', { modifiers: shiftModifier })
    })

    expect(div.style.display).toEqual('flex')

    await expectSingleUndo2Saves(editor, async () => {
      await pressKey('a', { modifiers: shiftModifier })
    })
    expect(div.style.display).toEqual('')
  })

  it('add and then remove flex layout', async () => {
    const editor = await renderTestEditorWithCode(
      project({ width: '100px', height: '100px' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await expectSingleUndo2Saves(editor, async () => {
      await addFlexLayout(editor)
    })

    expect(div.style.display).toEqual('flex')

    await expectSingleUndo2Saves(editor, async () => {
      await removeFlexLayout(editor)
    })

    expect(div.style.display).toEqual('')
  })

  it('adding (vertical) flex layout converts child `height` to `flexGrow`', async () => {
    const editor = await renderTestEditorWithCode(
      project({ width: '100px', height: '100%' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await expectSingleUndo2Saves(editor, async () => {
      await addFlexLayout(editor)
    })

    expect(div.style.display).toEqual('flex')
    expect(div.style.flexDirection).toEqual('column')

    const child = editor.renderedDOM.getByTestId('child')
    expect(child.style.height).toEqual('')
    expect(child.style.flexGrow).toEqual('1')
    expect(child.style.width).toEqual('100px')
  })

  it('adding flex layout converts child `height` to `flexGrow` if flexDirection is set', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithFlexDirectionColumn({ width: '111px', height: '100%' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await expectSingleUndo2Saves(editor, async () => {
      await addFlexLayout(editor)
    })

    expect(div.style.display).toEqual('flex')

    const child = editor.renderedDOM.getByTestId('child')
    expect(child.style.height).toEqual('')
    expect(child.style.flexGrow).toEqual('1')
  })

  it('adding flex layout removes absolute props and sets explicit width and height', async () => {
    const editor = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import { Storyboard } from 'utopia-api'

      export var storyboard = (
        <Storyboard data-uid='0cd'>
          <div
            data-testid='mydiv'
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 120,
              height: 140,
            }}
            data-uid='5f9'
          >
            <div
              data-testid='child'
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 10,
                left: 10,
                bottom: 10,
                right: 10,
              }}
              data-uid='9e4'
            />
          </div>
        </Storyboard>
      )`,
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await addFlexLayout(editor)

    expect(div.style.display).toEqual('flex')

    const child = editor.renderedDOM.getByTestId('child')
    expect(child.style.position).toEqual('')
    expect(child.style.top).toEqual('')
    expect(child.style.left).toEqual('')
    expect(child.style.bottom).toEqual('')
    expect(child.style.right).toEqual('')
    expect(child.style.contain).toEqual('layout')
    expect(child.style.width).toEqual('100px')
    expect(child.style.height).toEqual('120px')
  })

  it('adding flex layout does not add contain layout to static positioned child', async () => {
    const editor = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import { Storyboard } from 'utopia-api'

      export var storyboard = (
        <Storyboard data-uid='0cd'>
          <div
            data-testid='mydiv'
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 120,
              height: 140,
            }}
            data-uid='5f9'
          >
            <div
              data-testid='child'
              style={{
                backgroundColor: '#aaaaaa33',
                width: 100,
                height: 120,
              }}
              data-uid='9e4'
            />
          </div>
        </Storyboard>
      )`,
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await addFlexLayout(editor)

    expect(div.style.display).toEqual('flex')

    const child = editor.renderedDOM.getByTestId('child')

    // Ensure contain layout was not added
    expect(child.style.contain).toEqual('')

    // Might as well check the other props are correct whilst we're here
    expect(child.style.position).toEqual('')
    expect(child.style.top).toEqual('')
    expect(child.style.left).toEqual('')
    expect(child.style.bottom).toEqual('')
    expect(child.style.right).toEqual('')
    expect(child.style.width).toEqual('100px')
    expect(child.style.height).toEqual('120px')
  })
})

describe('add grid layout', () => {
  it('can convert to a grid', async () => {
    const editor = await renderTestEditorWithCode(
      project({ width: '100px', height: '100px' }),
      'await-first-dom-report',
    )
    const div = await selectDiv(editor)
    await expectSingleUndo2Saves(editor, async () => {
      await addGridLayout(editor)
    })

    expect(div.style.display).toEqual('grid')
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
