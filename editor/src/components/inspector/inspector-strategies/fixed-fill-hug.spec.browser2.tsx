import { assertNever } from '../../../core/shared/utils'
import { setFeatureEnabled } from '../../../utils/feature-switches'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, mouseDoubleClickAtPoint } from '../../canvas/event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../../canvas/ui-jsx.test-utils'
import { FlexDirection } from '../common/css-utils'
import { FillContainerLabel, FixedLabel, HugContentsLabel } from '../fill-hug-fixed-control'

describe('Fixed / Fill / Hug control', () => {
  before(() => setFeatureEnabled('Nine block control', true))
  after(() => setFeatureEnabled('Nine block control', false))

  describe('fill container', () => {
    it('set width to fill container in flex row', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithWidth('row'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(div.style.width).toEqual('')
      expect(div.style.minWidth).toEqual('')
      expect(div.style.maxWidth).toEqual('')
      expect(div.style.flexGrow).toEqual('1')
    })

    it('set width to fill container in flex column', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithWidth('column'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(div.style.minWidth).toEqual('')
      expect(div.style.maxWidth).toEqual('')
      expect(div.style.width).toEqual('100%')
      expect(div.style.flexGrow).toEqual('')
    })

    it('set height to fill container in flex row', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithHeight('row'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(div.style.minHeight).toEqual('')
      expect(div.style.maxHeight).toEqual('')
      expect(div.style.height).toEqual('100%')
      expect(div.style.flexGrow).toEqual('')
    })

    it('set height to fill container in flex column', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithHeight('column'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(div.style.minHeight).toEqual('')
      expect(div.style.maxHeight).toEqual('')
      expect(div.style.height).toEqual('')
      expect(div.style.flexGrow).toEqual('1')
    })
  })

  describe('hug contents', () => {
    it('hug contents along the horizontal axis', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithWidth('row'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'parent')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(div.style.width).toEqual('212px') // TODO: this should be `min-content` because that's what the action is setting
      expect(div.style.minWidth).toEqual('')
      expect(div.style.maxWidth).toEqual('')
    })

    it('hug contents along the vertical axis', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithHeight('row'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'parent')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(div.style.height).toEqual('144px') // TODO: this should be `min-content` because that's what the action is setting
      expect(div.style.minHeight).toEqual('')
      expect(div.style.maxHeight).toEqual('')
    })
  })
})

async function select(
  editor: EditorRenderResult,
  testId: 'child' | 'parent',
): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('child')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  if (testId === 'child') {
    mouseDoubleClickAtPoint(canvasControlsLayer, divCorner)
  } else if (testId === 'parent') {
    mouseClickAtPoint(canvasControlsLayer, divCorner)
  } else {
    assertNever(testId)
  }

  return div
}

const projectWithWidth = (flexDirection: FlexDirection) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='parent'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 333,
        top: 314,
        minWidth: 100,
        maxWidth: 1000,
        width: 415,
        height: 449,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-uid='c47'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          width: 212,
          height: 144,
          contain: 'layout',
        }}
        data-uid='945'
      />
    </div>
  </Storyboard>
)
`

const projectWithHeight = (flexDirection: FlexDirection) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='parent'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 333,
        top: 314,
        minHeight: 100,
        maxHeight: 1000,
        width: 415,
        height: 449,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-uid='c47'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          width: 212,
          height: 144,
          contain: 'layout',
        }}
        data-uid='945'
      />
    </div>
  </Storyboard>
)
`
