import { assertNever } from '../../../core/shared/utils'
import { setFeatureEnabled } from '../../../utils/feature-switches'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, mouseDoubleClickAtPoint } from '../../canvas/event-helpers.test-utils'
import { EditorRenderResult, renderTestEditorWithCode } from '../../canvas/ui-jsx.test-utils'
import { FlexDirection } from '../common/css-utils'
import { FillContainerLabel, FixedLabel, HugContentsLabel } from '../fill-hug-fixed-control'
import { MaxContent } from '../inspector-common'

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

      expect(div.style.width).toEqual(MaxContent)
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

      expect(div.style.height).toEqual(MaxContent)
      expect(div.style.minHeight).toEqual('')
      expect(div.style.maxHeight).toEqual('')
    })
  })

  describe('setting to fixed size deletes all interfering props in flex row', () => {
    it('set to fixed size along the horizontal axis deletes all interfering props in flex row', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithWidthAndFlexGrow('row'),
        'await-first-dom-report',
      )
      const child = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FixedLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(child.style.width).toEqual('700px')
      expect(child.style.height).toEqual('149px')
      expect(child.style.minWidth).toEqual('')
      expect(child.style.maxWidth).toEqual('')
      expect(child.style.flex).toEqual('')
      expect(child.style.flexBasis).toEqual('')
      expect(child.style.flexGrow).toEqual('')
      expect(child.style.flexShrink).toEqual('')
    })

    it('set to fixed size along the vertical axis deletes all interfering props in flex column', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithHeightAndFlexGrow('column'),
        'await-first-dom-report',
      )
      const child = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FixedLabel))[1]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(child.style.width).toEqual('229px')
      expect(child.style.height).toEqual('759px')
      expect(child.style.minHeight).toEqual('')
      expect(child.style.maxHeight).toEqual('')
      expect(child.style.flex).toEqual('')
      expect(child.style.flexBasis).toEqual('')
      expect(child.style.flexGrow).toEqual('')
      expect(child.style.flexShrink).toEqual('')
    })
  })
})

async function select(
  editor: EditorRenderResult,
  testId: 'child' | 'parent',
): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId(testId)
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
  <Storyboard>
    <Scene
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          minWidth: 100,
          maxWidth: 1000,
          width: 229,
          height: 149,
          contain: 'layout',
        }}
      />
    </Scene>
  </Storyboard>
)

`

const projectWithHeight = (flexDirection: FlexDirection) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          minHeight: 100,
          maxHeight: 1000,
          width: 229,
          height: 149,
          contain: 'layout',
        }}
      />
    </Scene>
  </Storyboard>
)
`

const projectWithWidthAndFlexGrow = (
  flexDirection: FlexDirection,
) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard>
    <Scene
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          minWidth: 100,
          maxWidth: 1000,
          flexGrow: 1,
          height: 149,
          contain: 'layout',
        }}
      />
    </Scene>
  </Storyboard>
)

`

const projectWithHeightAndFlexGrow = (
  flexDirection: FlexDirection,
) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          minHeight: 100,
          maxHeight: 1000,
          width: 229,
          flexGrow: 1,
          contain: 'layout',
        }}
      />
    </Scene>
  </Storyboard>
)
`
