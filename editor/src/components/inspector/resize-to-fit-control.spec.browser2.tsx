import { cmdModifier } from '../../utils/modifiers'
import { expectSingleUndo2Saves } from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { MaxContent } from './inspector-common'
import {
  ResizeToFillControlTestId,
  ResizeToFitControlTestId,
  ResizeToFixedControlTestId,
} from './resize-to-fit-control'

describe('Resize to fit control', () => {
  it('resizes to fit and back to fixed', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(projectSnippet),
      'await-first-dom-report',
    )
    const view = await selectView(editor, 'middle')
    await expectSingleUndo2Saves(editor, async () => {
      await clickResizeTo(editor, ResizeToFitControlTestId)
    })

    expect(view.style.width).toEqual(MaxContent)
    expect(view.style.minWidth).toEqual('')
    expect(view.style.maxWidth).toEqual('')
    expect(view.style.height).toEqual(MaxContent)
    expect(view.style.minHeight).toEqual('')
    expect(view.style.maxHeight).toEqual('')
    expect(view.style.flex).toEqual('')
    expect(view.style.flexShrink).toEqual('')
    expect(view.style.flexGrow).toEqual('')
    expect(view.style.flexBasis).toEqual('')

    const control = editor.renderedDOM.getByTestId(ResizeToFitControlTestId)
    expect(control.style.opacity).toEqual('0.5')

    await expectSingleUndo2Saves(editor, async () => {
      await clickResizeTo(editor, ResizeToFixedControlTestId)
    })

    expect(view.style.width).toEqual('765px')
    expect(view.style.minWidth).toEqual('')
    expect(view.style.maxWidth).toEqual('')
    expect(view.style.height).toEqual('343px')
    expect(view.style.minHeight).toEqual('')
    expect(view.style.maxHeight).toEqual('')
    expect(view.style.flex).toEqual('')
    expect(view.style.flexShrink).toEqual('')
    expect(view.style.flexGrow).toEqual('')
    expect(view.style.flexBasis).toEqual('')

    expect(control.style.opacity).toEqual('')
  })

  it('resizes to fill and back to fixed', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(projectSnippet),
      'await-first-dom-report',
    )
    const view = await selectView(editor, 'middle')
    await expectSingleUndo2Saves(editor, async () => {
      await clickResizeTo(editor, ResizeToFillControlTestId)
    })

    expect(view.style.flex).toEqual('')
    expect(view.style.flexGrow).toEqual('1')
    expect(view.style.flexShrink).toEqual('')
    expect(view.style.flexBasis).toEqual('')
    expect(view.style.minWidth).toEqual('')
    expect(view.style.width).toEqual('')
    expect(view.style.maxWidth).toEqual('')
    expect(view.style.height).toEqual('100%')
    expect(view.style.minHeight).toEqual('')
    expect(view.style.maxHeight).toEqual('')

    const control = editor.renderedDOM.getByTestId(ResizeToFillControlTestId)
    expect(control.style.opacity).toEqual('0.5')

    await expectSingleUndo2Saves(editor, async () => {
      await clickResizeTo(editor, ResizeToFixedControlTestId)
    })

    expect(view.style.width).toEqual('765px')
    expect(view.style.minWidth).toEqual('')
    expect(view.style.maxWidth).toEqual('')
    expect(view.style.height).toEqual('343px')
    expect(view.style.minHeight).toEqual('')
    expect(view.style.maxHeight).toEqual('')
    expect(view.style.flex).toEqual('')
    expect(view.style.flexShrink).toEqual('')
    expect(view.style.flexGrow).toEqual('')
    expect(view.style.flexBasis).toEqual('')

    expect(control.style.opacity).toEqual('')
  })

  it('resizes to fit, with shortcut', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(projectSnippet),
      'await-first-dom-report',
    )
    const view = await selectView(editor, 'middle')
    await expectSingleUndo2Saves(editor, async () => {
      await pressKey('r', { modifiers: { alt: true, cmd: true, shift: true, ctrl: false } })
    })

    expect(view.style.width).toEqual(MaxContent)
    expect(view.style.minWidth).toEqual('')
    expect(view.style.maxWidth).toEqual('')
    expect(view.style.height).toEqual(MaxContent)
    expect(view.style.minHeight).toEqual('')
    expect(view.style.maxHeight).toEqual('')
    expect(view.style.flex).toEqual('')
    expect(view.style.flexShrink).toEqual('')
    expect(view.style.flexGrow).toEqual('')
    expect(view.style.flexBasis).toEqual('')
  })

  it('when container is set to hug on one axis, it is resized to fit', async () => {
    const editor = await renderTestEditorWithCode(projectOneAxisOnHug, 'await-first-dom-report')
    const view = await selectView(editor, ViewTestId)

    await expectSingleUndo2Saves(editor, async () => {
      await clickResizeTo(editor, ResizeToFitControlTestId)
    })

    expect(view.style.width).toEqual(MaxContent)
    expect(view.style.minWidth).toEqual('')
    expect(view.style.maxWidth).toEqual('')
    expect(view.style.height).toEqual(MaxContent)
    expect(view.style.minHeight).toEqual('')
    expect(view.style.maxHeight).toEqual('')
    expect(view.style.flex).toEqual('')
    expect(view.style.flexShrink).toEqual('')
    expect(view.style.flexGrow).toEqual('')
    expect(view.style.flexBasis).toEqual('')
  })
})

const ViewTestId = 'view'

async function selectView(editor: EditorRenderResult, testId: string): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const view = editor.renderedDOM.getByTestId(testId)
  const viewBounds = view.getBoundingClientRect()
  const viewCorner = {
    x: viewBounds.x + 5,
    y: viewBounds.y + 4,
  }

  await mouseClickAtPoint(canvasControlsLayer, viewCorner, { modifiers: cmdModifier })

  return view
}

async function clickResizeTo(editor: EditorRenderResult, testId: string) {
  const resizeToFitControl = editor.renderedDOM.getByTestId(testId)

  await mouseClickAtPoint(resizeToFitControl, { x: 2, y: 2 })
}

const projectSnippet = `<div
style={{
  backgroundColor: '#aaaaaa33',
  width: 863,
  height: 441,
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
  padding: '49px 49px 49px 49px',
  contain: 'layout',
}}
data-uid='outermost'
data-testid='outermost'
>
<div
  style={{
    backgroundColor: '#aaaaaa33',
    height: '100%',
    contain: 'layout',
    display: 'flex',
    flexDirection: 'row',
    padding: '54.5px 77px 54.5px 77px',
    flexGrow: 1,
    alignItems: 'center',
    justifyContent: 'center',
  }}
  data-uid='middle'
  data-testid='middle'
>
  <div
    style={{
      backgroundColor: '#aaaaaa33',
      height: '100%',
      contain: 'layout',
      flexGrow: 1,
    }}
    data-uid='innermost'
    data-testid='innermost'
  />
</div>
</div>
`

const projectOneAxisOnHug = `import * as React from 'react'
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
          width: 'max-content',
          height: 294,
          display: 'flex',
          alignItems: 'center',
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
