import { assertNever } from '../../../core/shared/utils'
import { setFeatureEnabled } from '../../../utils/feature-switches'
import { wait } from '../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  pressKey,
} from '../../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCodeWithoutUIDs,
  renderTestEditorWithCode,
} from '../../canvas/ui-jsx.test-utils'
import { FlexDirection } from '../common/css-utils'
import {
  FillContainerLabel,
  FillFixedHugControlId,
  FixedLabel,
  HugContentsLabel,
} from '../fill-hug-fixed-control'
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

    it('edit fill container value in flex', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFlexChildInFill,
        'await-first-dom-report',
      )
      const child = await select(editor, 'child')

      expect(child.style.flexGrow).toEqual('1')
      const control = editor.renderedDOM.getByTestId(FillFixedHugControlId('width'))
      mouseClickAtPoint(control, { x: 5, y: 5 })

      // await wait(5000)

      pressKey('3', { targetElement: control })

      // await wait(5000)
      pressKey('Enter', { targetElement: control })

      // await wait(5000)

      expect(child.style.flexGrow).toEqual('3')
    })

    it('edit fill container value in flow', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithChildInFlowLayout,
        'await-first-dom-report',
      )
      const child = await select(editor, 'child')

      expect(child.style.width).toEqual('100%')
      const control = editor.renderedDOM.getByTestId(FillFixedHugControlId('width'))
      mouseClickAtPoint(control, { x: 5, y: 5 })

      // await wait(5000)

      pressKey('5', { targetElement: control })
      pressKey('0', { targetElement: control })

      // await wait(5000)
      pressKey('Enter', { targetElement: control })

      // await wait(5000)

      expect(child.style.width).toEqual('50%')
    })

    it('set width to fill container on absolute positioned element', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          left: 10,
          top: 10,
          width: 100,
          height: 100,
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          top: 10,
          height: 100,
          width: '100%',
        `),
      )
    })

    it('set width to fill container on absolute positioned element with height already set to fill', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          left: 10,
          width: 100,
          height: '100%',
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`
        height: '100%',
        contain: 'layout',
        width: '100%',
        `),
      )
    })

    it('set height to fill container on absolute positioned element', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          left: 10,
          top: 10,
          width: 100,
          height: 100,
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          left: 10,
          width: 100,
          height: '100%',
        `),
      )
    })

    it('set height to fill container on absolute positioned element with width already set to fill', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          top: 10,
          width: '100%',
          height: 100,
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[1]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`
          width: '100%',
          contain: 'layout',
          height: '100%',
        `),
      )
    })

    it('set height to fill container on static positioned element with width already set to fill', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          top: 10,
          width: '100%',
          height: 100,
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[1]
      mouseClickAtPoint(button, { x: 5, y: 5 })

      // Should not add contain: layout
      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`width: '100%', height: '100%'`),
      )
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

    describe('Convert children to fixed size when setting to hug contents to avoid parent container collapsing', () => {
      it('child is set to fill container on the horizontal axis', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToHorizontalFill,
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.height).toEqual('149px')
        expect(child.style.width).toEqual('')
        expect(child.style.flexGrow).toEqual('1')

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

        mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        mouseClickAtPoint(button, { x: 5, y: 5 })

        expect(parent.style.height).toEqual('759px')
        expect(child.style.height).toEqual('149px')
        expect(parent.style.width).toEqual(MaxContent)
        expect(child.style.width).toEqual('700px')
      })
      it('child is set to fill container on the vertical axis', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToVerticalFill,
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual('149px')
        expect(child.style.height).toEqual('')
        expect(child.style.flexGrow).toEqual('1')

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

        mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        mouseClickAtPoint(button, { x: 5, y: 5 })

        expect(parent.style.width).toEqual('700px')
        expect(child.style.width).toEqual('149px')
        expect(parent.style.height).toEqual(MaxContent)
        expect(child.style.height).toEqual('759px')
      })

      it('child is set to fixed size on the horizontal axis', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToFixed('row'),
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual('302px')
        expect(child.style.height).toEqual('141px')

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

        mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        mouseClickAtPoint(button, { x: 5, y: 5 })

        expect(parent.style.width).toEqual(MaxContent)
        expect(child.style.width).toEqual('302px')
        expect(parent.style.height).toEqual('759px')
        expect(child.style.height).toEqual('141px')
      })

      it('child is set to fixed size on the vertical axis, no conversion happens', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToFixed('column'),
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual('302px')
        expect(child.style.height).toEqual('141px')

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

        mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        mouseClickAtPoint(button, { x: 5, y: 5 })

        expect(parent.style.width).toEqual('700px')
        expect(child.style.width).toEqual('302px')
        expect(parent.style.height).toEqual(MaxContent)
        expect(child.style.height).toEqual('141px')
      })

      it('child is set to hug contents on the horizontal axis, no conversion happens', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToHugContents('row'),
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual(MaxContent)
        expect(child.style.height).toEqual(MaxContent)

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

        mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        mouseClickAtPoint(button, { x: 5, y: 5 })

        expect(parent.style.width).toEqual(MaxContent)
        expect(child.style.width).toEqual(MaxContent)
        expect(parent.style.height).toEqual('751px')
        expect(child.style.height).toEqual(MaxContent)
      })

      it('child is set to hug contents on the vertical axis, no conversion happens', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToHugContents('column'),
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual(MaxContent)
        expect(child.style.height).toEqual(MaxContent)

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

        mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        mouseClickAtPoint(button, { x: 5, y: 5 })

        expect(parent.style.width).toEqual('508px')
        expect(child.style.width).toEqual(MaxContent)
        expect(parent.style.height).toEqual(MaxContent)
        expect(child.style.height).toEqual(MaxContent)
      })
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
    x: divBounds.x + 5,
    y: divBounds.y + 4,
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

const projectWithChildSetToHorizontalFill = `import * as React from 'react'
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
        display: 'flex'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          flexGrow: 1,
          height: 149,
          contain: 'layout',
        }}
      />
    </Scene>
  </Storyboard>
)
`

const projectWithChildSetToVerticalFill = `import * as React from 'react'
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
        flexDirection: 'column'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          flexGrow: 1,
          width: 149,
          contain: 'layout',
        }}
      />
    </Scene>
  </Storyboard>
)
`

const projectWithChildSetToFixed = (flexDirection: FlexDirection) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='33d'>
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
      data-uid='26c'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          height: 141,
          contain: 'layout',
          width: 302,
        }}
        data-uid='744'
      />
    </Scene>
  </Storyboard>
)
`

const projectWithChildSetToHugContents = (
  flexDirection: FlexDirection,
) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      data-testid='parent'
      style={{
        height: 751,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}',
        width: 508,
      }}
      data-label='Playground'
      data-uid='26c'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          contain: 'layout',
          display: 'flex',
          width: 'max-content',
          height: 'max-content',
        }}
        data-uid='744'
      >
        <div
          style={{
            backgroundColor: '#ffa19c',
            contain: 'layout',
            height: 208,
            width: 165.5,
          }}
          data-uid='0b9'
        />
        <div
          style={{
            backgroundColor: '#c4ded1',
            contain: 'layout',
            height: 208,
            width: 165.5,
          }}
          data-uid='741'
        />
      </div>
    </Scene>
  </Storyboard>
)
`

const projectWithFlexChildInFill = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 200,
        top: 38,
        width: 533,
        height: 354,
        display: 'flex',
        padding: '30px 50px 30px 50px',
        gap: 55,
        flexDirection: 'row',
      }}
      data-uid='6b7'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          flexGrow: 1,
        }}
        data-uid='a9d'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          flexGrow: 1,
        }}
        data-uid='aaa'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          flexGrow: 1,
        }}
        data-uid='aab'
      />
    </div>
  </Storyboard>
)
`

const projectWithChildInFlowLayout = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { Playground } from '/src/playground.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 300,
        top: 38,
        width: 533,
        height: 354,
        padding: '30px 50px 30px 50px',
      }}
      data-uid='6b7'
    >
      <div
        data-testid='div'
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          width: '100%',
        }}
        data-uid='a9d'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          width: 65,
        }}
        data-uid='aaa'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          width: 65,
        }}
        data-uid='aab'
      />
    </div>
  </Storyboard>
)
`

const absoluteProjectWithInjectedStyle = (stylePropsAsString: string) =>
  formatTestProjectCode(`
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard>
    <Scene
      data-testid='parent'
      style={{
        position: 'absolute',
        left: 0,
        top: 0,
        width: 500,
        height: 500,
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{${stylePropsAsString}}}
      />
    </Scene>
  </Storyboard>
)`)
