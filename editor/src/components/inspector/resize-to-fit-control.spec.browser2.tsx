/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectNoAction"] }] */
import { cmdModifier } from '../../utils/modifiers'
import {
  expectNoAction,
  expectSingleUndo2Saves,
  selectComponentsForTest,
} from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { getPrintedUiJsCode } from '../canvas/ui-jsx.test-utils'
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
import * as EP from '../../core/shared/element-path'

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

    expect(view.style.width).toEqual('154px')
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

    for (let count: number = 0; count < 2; count++) {
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

      await expectSingleUndo2Saves(editor, async () => {
        await pressKey('r', { modifiers: { alt: true, cmd: true, shift: true, ctrl: false } })
      })

      expect(view.style.width).toEqual('154px')
      expect(view.style.minWidth).toEqual('')
      expect(view.style.maxWidth).toEqual('')
      expect(view.style.height).toEqual('343px')
      expect(view.style.minHeight).toEqual('')
      expect(view.style.maxHeight).toEqual('')
      expect(view.style.flex).toEqual('')
      expect(view.style.flexShrink).toEqual('')
      expect(view.style.flexGrow).toEqual('')
      expect(view.style.flexBasis).toEqual('')
    }
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
  describe('for groups', () => {
    describe('targeting children of groups that are not groups themselves', () => {
      it('resize to fit works as usual', async () => {
        const editor = await renderTestEditorWithCode(projectWithGroup, 'await-first-dom-report')
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group/child-1`)])
        await expectSingleUndo2Saves(editor, async () => {
          await clickResizeTo(editor, ResizeToFitControlTestId)
        })
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { Group } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <Group
        data-uid='group'
        data-testid='group'
        style={{
          position: 'absolute',
          left: 0,
          top: 0,
          width: 600,
          height: 600,
        }}
      >
        <div
          style={{
            position: 'absolute',
            backgroundColor: '#aaaaaa33',
            left: 0,
            top: 0,
            width: 'max-content',
            height: 'max-content',
          }}
          data-uid='child-1'
        >
          <div
            style={{ width: 600, height: 600 }}
            data-uid='grandchild-1'
          />
        </div>
        <div
          style={{
            position: 'absolute',
            backgroundColor: '#aaaaaa33',
            left: 250,
            top: 250,
            width: 50,
            height: 50,
          }}
          data-uid='child-2'
        />
      </Group>
    </Scene>
  </Storyboard>
)
`)
      })
      it('resize to fill is disabled', async () => {
        const editor = await renderTestEditorWithCode(projectWithGroup, 'await-first-dom-report')
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group/child-1`)])
        await expectNoAction(editor, async () => {
          await clickResizeTo(editor, ResizeToFillControlTestId)
        })
      })
      it('set fixed sized works as it does normally', async () => {
        const editor = await renderTestEditorWithCode(projectWithGroup, 'await-first-dom-report')
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group/child-1`)])
        await expectSingleUndo2Saves(editor, async () => {
          await clickResizeTo(editor, ResizeToFixedControlTestId)
        })
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { Group } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <Group
        data-uid='group'
        data-testid='group'
        style={{
          position: 'absolute',
          left: 0,
          top: 0,
          width: 300,
          height: 400,
        }}
      >
        <div
          style={{
            position: 'absolute',
            backgroundColor: '#aaaaaa33',
            left: 0,
            top: 0,
            width: 50,
            height: 50,
          }}
          data-uid='child-1'
        >
          <div
            style={{ width: 600, height: 600 }}
            data-uid='grandchild-1'
          />
        </div>
        <div
          style={{
            position: 'absolute',
            backgroundColor: '#aaaaaa33',
            left: 250,
            top: 250,
            width: 50,
            height: 50,
          }}
          data-uid='child-2'
        />
      </Group>
    </Scene>
  </Storyboard>
)
`)
      })
    })
    describe('targeting children of groups that are groups', () => {
      it('resize to fit is disabled', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithNestedGroup,
          'await-first-dom-report',
        )
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group-1/group-2`)])
        await expectNoAction(editor, async () => {
          await clickResizeTo(editor, ResizeToFitControlTestId)
        })
      })
      it('resize to fill is disabled', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithNestedGroup,
          'await-first-dom-report',
        )
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group-1/group-2`)])
        await expectNoAction(editor, async () => {
          await clickResizeTo(editor, ResizeToFillControlTestId)
        })
      })
      it('set fixed sized works as it does normally', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithNestedGroup,
          'await-first-dom-report',
        )
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group-1/group-2`)])
        await expectSingleUndo2Saves(editor, async () => {
          await clickResizeTo(editor, ResizeToFixedControlTestId)
        })
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { Group } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <Group
        data-uid='group-1'
        data-testid='group-1'
        style={{
          position: 'absolute',
          left: 0,
          top: 0,
          width: 300,
          height: 400,
        }}
      >
        <Group
          data-uid='group-2'
          data-testid='group-2'
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            width: 300,
            height: 400,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              left: 0,
              top: 0,
              width: 50,
              height: 50,
            }}
            data-uid='grandchild-1'
          />
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#aaaaaa33',
              left: 250,
              top: 350,
              width: 50,
              height: 50,
            }}
            data-uid='grandchild-2'
          />
        </Group>
      </Group>
    </Scene>
  </Storyboard>
)
`)
      })
    })
    describe('targeting groups with non-group children', () => {
      it('resize to fit is disabled', async () => {
        const editor = await renderTestEditorWithCode(projectWithGroup, 'await-first-dom-report')
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group`)])
        await expectNoAction(editor, async () => {
          await clickResizeTo(editor, ResizeToFitControlTestId)
        })
      })
      it('resize to fill is disabled', async () => {
        const editor = await renderTestEditorWithCode(projectWithGroup, 'await-first-dom-report')
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group`)])
        await expectNoAction(editor, async () => {
          await clickResizeTo(editor, ResizeToFillControlTestId)
        })
      })
      it('set fixed sized converts to a frame', async () => {
        const editor = await renderTestEditorWithCode(projectWithGroup, 'await-first-dom-report')
        await selectComponentsForTest(editor, [EP.fromString(`storyboard/scene/group`)])
        await expectSingleUndo2Saves(editor, async () => {
          await clickResizeTo(editor, ResizeToFixedControlTestId)
        })
        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { Group } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <div
        data-uid='group'
        data-testid='group'
        style={{
          position: 'absolute',
          left: 0,
          top: 0,
          width: 300,
          height: 400,
        }}
      >
        <div
          style={{
            position: 'absolute',
            backgroundColor: '#aaaaaa33',
            left: 0,
            top: 0,
            width: 50,
            height: 50,
          }}
          data-uid='child-1'
        >
          <div
            style={{ width: 600, height: 600 }}
            data-uid='grandchild-1'
          />
        </div>
        <div
          style={{
            position: 'absolute',
            backgroundColor: '#aaaaaa33',
            left: 250,
            top: 250,
            width: 50,
            height: 50,
          }}
          data-uid='child-2'
        />
      </div>
    </Scene>
  </Storyboard>
)
`)
      })
    })
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

const projectWithGroup = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { Group } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <Group
        data-uid='group'
        data-testid='group'
        style={{
          position: 'absolute',
          left: 0,
          top: 0,
          width: 300,
          height: 400,
        }}
      >
        <div
          style={{
            position: 'absolute',
            backgroundColor: '#aaaaaa33',
            left: 0,
            top: 0,
            width: 50,
            height: 50,
          }}
          data-uid='child-1'
        >
          <div
            style={{ width: 600, height: 600 }}
            data-uid='grandchild-1'
          />
        </div>
        <div
          style={{
            position: 'absolute',
            backgroundColor: '#aaaaaa33',
            left: 250,
            top: 250,
            width: 50,
            height: 50,
          }}
          data-uid='child-2'
        />
      </Group>
    </Scene>
  </Storyboard>
)
`

const projectWithNestedGroup = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { Group } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <Group
        data-uid='group-1'
        data-testid='group-1'
        style={{
          position: 'absolute',
          left: 0,
          top: 0,
          width: 300,
          height: 400,
        }}
      >
        <Group
          data-uid='group-2'
          data-testid='group-2'
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            width: 300,
            height: 400,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              left: 0,
              top: 0,
              width: 50,
              height: 50,
            }}
            data-uid='grandchild-1'
          />
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#aaaaaa33',
              left: 250,
              top: 350,
              width: 50,
              height: 50,
            }}
            data-uid='grandchild-2'
          />
        </Group>
      </Group>
    </Scene>
  </Storyboard>
)
`
