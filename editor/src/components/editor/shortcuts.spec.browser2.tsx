import { safeIndex } from '../../core/shared/array-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import type { CanvasRectangle } from '../../core/shared/math-utils'
import { canvasRectangle } from '../../core/shared/math-utils'
import { altCmdModifier, cmdModifier, ctrlModifier, shiftModifier } from '../../utils/modifiers'
import {
  expectNoAction,
  expectSingleUndo2Saves,
  selectComponentsForTest,
  wait,
} from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { keyDown, mouseClickAtPoint, pressKey } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  makeTestProjectCodeWithSnippetWithoutUIDs,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../canvas/ui-jsx.test-utils'
import { selectComponents } from './actions/meta-actions'

const TestIdOne = 'one'
const TestIdTwo = 'two'
const StoryBoardId = 'StoryBoardId'
const ParentId = 'ParentId'
const backgroundColor = '#384C5CAB'

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectBoundsToEqual", "expectNoAction"] }] */

// for some reason, ctrl + c does not trigger the eyedropper in tests
// maybe for security since the event is programmatically triggered?
describe('shortcuts', () => {
  xdescribe('eyedropper', () => {
    it('use eyedropper to set background color', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId(TestIdOne)
      const divBounds = div.getBoundingClientRect()
      const divCorner = {
        x: divBounds.x + 50,
        y: divBounds.y + 40,
      }

      await mouseClickAtPoint(canvasControlsLayer, divCorner)

      expect(editor.getEditorState().editor.selectedViews.length).toEqual(1)

      keyDown('c', { modifiers: ctrlModifier })

      const div2 = editor.renderedDOM.getByTestId(TestIdTwo)
      const div2Bounds = div2.getBoundingClientRect()
      const div2Corner = {
        x: div2Bounds.x + 50,
        y: div2Bounds.y + 40,
      }

      await mouseClickAtPoint(canvasControlsLayer, div2Corner)

      await editor.getDispatchFollowUpActionsFinished()

      //   expect(div.style.backgroundColor).toEqual(backgroundColor)
      expect(div2.style.backgroundColor).toEqual(backgroundColor)
    })
  })

  describe('x', () => {
    it('when `position: absolute` is set on the selected element, positioning props are removed', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId(TestIdOne)

      await selectComponentsForTest(editor, [EP.fromString(`${StoryBoardId}/${TestIdOne}`)])

      await expectSingleUndo2Saves(editor, async () => {
        await pressKey('x')
      })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.position).toEqual('')
      expect(div.style.top).toEqual('')
      expect(div.style.bottom).toEqual('')
      expect(div.style.left).toEqual('')
      expect(div.style.right).toEqual('')
    })
    it('when `position: absolute` is set on the selected element, do not remove positioning props if the parent is a group', async () => {
      const editor = await renderTestEditorWithCode(projectWithGroup, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId(TestIdOne)

      await selectComponentsForTest(editor, [EP.fromString(`${StoryBoardId}/group/${TestIdOne}`)])

      await expectNoAction(editor, async () => {
        await pressKey('x')
      })
      await editor.getDispatchFollowUpActionsFinished()
      const currentToastMessages = editor.getEditorState().editor.toasts.map((toast) => {
        return toast.message
      })
      expect(currentToastMessages).toEqual(['Cannot remove absolute position for group children.'])

      expect(div.style.position).toEqual('absolute')
      expect(div.style.left).toEqual('67px')
      expect(div.style.top).toEqual('128px')
      expect(div.style.width).toEqual('161px')
      expect(div.style.height).toEqual('171px')
    })

    it('when the selected element participates in the layout, absolute positioning props are added to the selected element', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithChildInFlexLayout,
        'await-first-dom-report',
      )

      const div = editor.renderedDOM.getByTestId(TestIdOne)

      await selectComponentsForTest(editor, [
        EP.fromString(`${StoryBoardId}/${ParentId}/${TestIdOne}`),
      ])

      await expectSingleUndo2Saves(editor, async () => {
        await pressKey('x')
      })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.position).toEqual('absolute')
      expect(div.style.top).toEqual('47px')
      expect(div.style.left).toEqual('50px')
      expect(div.style.width).toEqual('270px')
      expect(div.style.height).toEqual('296px')
      expect(div.style.contain).toEqual('layout')
    })

    it('does not convert spans to zero-sized elements', async () => {
      const editor = await renderTestEditorWithCode(projectWithSpan, 'await-first-dom-report')

      const div = editor.renderedDOM.getByTestId(TestIdOne)

      await selectComponentsForTest(editor, [EP.fromString('sb/span')])

      await expectSingleUndo2Saves(editor, async () => {
        await pressKey('x')
      })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.position).toEqual('')
      expect(div.style.display).toEqual('inline-block')
      expect(div.style.width).toEqual('182px')
      expect(div.style.height).toEqual('130px')

      await expectSingleUndo2Saves(editor, async () => {
        await pressKey('x')
      })
      await editor.getDispatchFollowUpActionsFinished()
      expect(div.style.position).toEqual('absolute')
      expect(div.style.display).toEqual('inline-block')
      expect(div.style.width).toEqual('182px')
      expect(div.style.height).toEqual('130px')
      expect(div.style.top).toEqual('0px')
      expect(div.style.left).toEqual('0px')
    })

    it('pressing x when a fragment is selected does nothing', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFragmentLikeElements,
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/fragment')])

      await expectNoAction(editor, async () => {
        await pressKey('x')
      })
    })

    it('pressing x on an absolute positioned container with only absolute children converts it to a static div', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 646,
        top: 72,
        width: 605,
        height: 190,
      }}
      data-uid='wrapper'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 162,
          top: 49,
          width: 358,
          height: 100,
        }}
        data-uid='container'
        data-testid='container'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 65,
            top: 34,
            width: 77,
            height: 35,
          }}
          data-testid='child1'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 186,
            top: 34,
            width: 94,
            height: 55,
          }}
          data-testid='child2'
        />
      </div>
     </div>`),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:wrapper/container`),
      ])

      await expectSingleUndo2Saves(editor, async () => {
        await pressKey('x')
      })

      const div = editor.renderedDOM.getByTestId('container')
      expect(div.style.position).toEqual('')
      expect(div.style.top).toEqual('')
      expect(div.style.bottom).toEqual('')
      expect(div.style.left).toEqual('')
      expect(div.style.right).toEqual('')

      expectBoundsToEqual(
        editor,
        'child1',
        canvasRectangle({ x: 65, y: 34, width: 77, height: 35 }),
      )

      expectBoundsToEqual(
        editor,
        'child2',
        canvasRectangle({ x: 186, y: 34, width: 94, height: 55 }),
      )
    })
  })

  describe('jump to parent', () => {
    it('with esc', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithChildInFlexLayout,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [
        EP.fromString(`${StoryBoardId}/${ParentId}/${TestIdOne}`),
      ])

      await pressKey('esc')
      await editor.getDispatchFollowUpActionsFinished()
      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        `${StoryBoardId}/${ParentId}`,
      ])
    })

    it('with backslash', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithChildInFlexLayout,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [
        EP.fromString(`${StoryBoardId}/${ParentId}/${TestIdOne}`),
      ])

      await pressKey('\\')
      await editor.getDispatchFollowUpActionsFinished()
      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        `${StoryBoardId}/${ParentId}`,
      ])
    })

    it('with shift + enter', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithChildInFlexLayout,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [
        EP.fromString(`${StoryBoardId}/${ParentId}/${TestIdOne}`),
      ])

      await pressKey('enter', { modifiers: shiftModifier })
      await editor.getDispatchFollowUpActionsFinished()
      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        `${StoryBoardId}/${ParentId}`,
      ])
    })
  })

  describe('duplicate', () => {
    it('duplicate element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
      <div data-uid='container'>
        <span data-uid='text'>Hello there</span>
      </div>
      `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/text`),
      ])

      await expectSingleUndo2Saves(editor, () => pressKey('d', { modifiers: cmdModifier }))

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div data-uid='container'>
        <span data-uid='text'>Hello there</span>
        <span data-uid='text.1'>Hello there</span>
      </div>`),
      )
    })

    it('duplicate element in true branch of a conditional', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='container'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                <span data-uid='text'>Hello there</span>
               ) : 'Test' 
             }
           </div>`,
        ),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/conditional/text`,
        ),
      ])

      await expectSingleUndo2Saves(editor, () => pressKey('d', { modifiers: cmdModifier }))

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='container'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                <React.Fragment>
                  <span data-uid='text.1'>Hello there</span>
                  <span data-uid='text'>Hello there</span>
                </React.Fragment>
               ) : 'Test' 
             }
           </div>`,
        ),
      )
    })

    it('duplicate element in false branch of a conditional', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='container'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                'Test'
               ) : <span data-uid='text'>Hello there</span>
             }
           </div>`,
        ),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/conditional/text`,
        ),
      ])

      await expectSingleUndo2Saves(editor, () => pressKey('d', { modifiers: cmdModifier }))

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='container'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                'Test'
               ) : <React.Fragment>
               <span data-uid='text.1'>Hello there</span>
               <span data-uid='text'>Hello there</span>
             </React.Fragment>
             }
           </div>`,
        ),
      )
    })

    it('duplicate slot in a conditional', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='container'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                null
               ) : <span data-uid='text'>Hello there</span>
             }
           </div>`,
        ),
        'await-first-dom-report',
      )

      const initialUiCode = getPrintedUiJsCode(editor.getEditorState())

      const slot = editor.renderedDOM.getByText('Empty')
      await mouseClickAtPoint(slot, { x: 5, y: 5 })

      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa/app-entity:container/conditional/d8401989bd0073a339250358a6ae16a1',
      ])

      await expectNoAction(editor, () => pressKey('d', { modifiers: cmdModifier }))

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialUiCode)
    })
  })
})

const project = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

    export var storyboard = (
  <Storyboard data-uid='${StoryBoardId}'>
    <div
      data-testid='${TestIdOne}'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 67,
        top: 128,
        width: 161,
        height: 171,
      }}
      data-uid='${TestIdOne}'
    />
    <div
      data-testid='${TestIdTwo}'
      style={{
        backgroundColor: '${backgroundColor}',
        position: 'absolute',
        left: 183,
        top: 350,
        width: 173,
        height: 222,
      }}
      data-uid='${TestIdTwo}'
    />
  </Storyboard>
)
`

const projectWithChildInFlexLayout = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='${StoryBoardId}'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 67,
        top: 128,
        width: 340,
        height: 363,
        display: 'flex',
        padding: '47px 20px 20px 50px',
      }}
      data-uid='${ParentId}'
      data-testid='${ParentId}'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          contain: 'layout',
          height: '100%',
          flexGrow: 1
        }}
        data-testid='${TestIdOne}'
        data-uid='${TestIdOne}'
      />
    </div>
  </Storyboard>
)
`

const projectWithSpan = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <span
      style={{
        position: 'absolute',
        wordBreak: 'break-word',
        left: 172,
        top: 189,
        width: 182,
        height: 130,
      }}
      data-testid='${TestIdOne}'
      data-uid='span'
    >
      hello there
    </span>
  </Storyboard>
)
`

const projectWithFragmentLikeElements = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <React.Fragment data-uid='fragment'>
      <div
        style={{
          position: 'absolute',
          backgroundColor: '#aaaaaa33',
          width: 207,
          height: 202,
          left: 500,
          top: 175,
        }}
        data-uid='aff'
      />
      <div
        style={{
          position: 'absolute',
          backgroundColor: '#aaaaaa33',
          width: 73,
          height: 109,
          left: 500,
          top: 377,
        }}
        data-uid='4ec'
      />
    </React.Fragment>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 101,
        top: 176,
      }}
      data-uid='group'
      data-testid='group'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 73,
          height: 109,
          left: 0,
          top: 202,
          position: 'absolute',
        }}
        data-uid='0bf'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 207,
          height: 202,
          left: 0,
          top: 0,
          position: 'absolute',
        }}
        data-uid='755'
      />
    </div>
  </Storyboard>
)
`

const projectWithGroup = `import * as React from 'react'
import { Scene, Storyboard, Group } from 'utopia-api'

    export var storyboard = (
  <Storyboard data-uid='${StoryBoardId}'>
    <Group data-uid='group' data-testid='group'>
      <div
        data-testid='${TestIdOne}'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 67,
          top: 128,
          width: 161,
          height: 171,
        }}
        data-uid='${TestIdOne}'
      />
      <div
        data-testid='${TestIdTwo}'
        style={{
          backgroundColor: '${backgroundColor}',
          position: 'absolute',
          left: 183,
          top: 350,
          width: 173,
          height: 222,
        }}
        data-uid='${TestIdTwo}'
      />
    </Group>
  </Storyboard>
)
`

describe('global shortcuts to set properties', () => {
  it('cmd + b toggles text to bold', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    await expectSingleUndo2Saves(renderResult, async () => {
      await pressKey('b', { modifiers: cmdModifier })
    })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontWeight: 'bold' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + b unsets font weight if it was bold', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontWeight: 'bold' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    await expectSingleUndo2Saves(renderResult, async () => {
      await pressKey('b', { modifiers: cmdModifier })
    })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div style={{}} data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + i toggles text to italic', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    await expectSingleUndo2Saves(renderResult, async () => {
      await pressKey('i', { modifiers: cmdModifier })
    })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontStyle: 'italic' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + i unsets font style if it was italic', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontStyle: 'italic' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    await expectSingleUndo2Saves(renderResult, async () => {
      await pressKey('i', { modifiers: cmdModifier })
    })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{}}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + u toggles text to underline', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    await expectSingleUndo2Saves(renderResult, async () => {
      await pressKey('u', { modifiers: cmdModifier })
    })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ textDecoration: 'underline' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + u unsets text decoration if it was underlined', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ textDecoration: 'underline' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    await expectSingleUndo2Saves(renderResult, async () => {
      await pressKey('u', { modifiers: cmdModifier })
    })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{}}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('alt + cmd + v to paste style properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.2, color: 'hotpink' }}
            data-uid='ccc'
          >hello</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const copyPropertiesFrom = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
    )
    await renderResult.dispatch(selectComponents([copyPropertiesFrom], false), true)

    // copy style properties first
    await pressKey('c', { modifiers: altCmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)
    await renderResult.dispatch(selectComponents([target], false), true)

    // paste style properties
    await pressKey('v', { modifiers: altCmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{  width: 200, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='ccc'
          >hello</div>
        </div>`,
      ),
    )
  })
})

describe('group selection', () => {
  // x-ed when cmd+g was changed so it wraps in a fragment
  xdescribe('grouping in container', () => {
    it('wraps flex row children in a container with flex row set', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboardChildren(`<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 200,
          top: 138,
          width: 513,
          height: 364,
          display: 'flex',
          gap: 42,
        }}
        data-uid='e5b'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 139,
            height: 130,
            contain: 'layout',
          }}
          data-uid='6de'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 119,
            height: 213,
            contain: 'layout',
          }}
          data-uid='8f4'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 99,
            height: 132,
            contain: 'layout',
          }}
          data-uid='0e8'
        />
      </div>`),
        'await-first-dom-report',
      )

      await doGroup(editor)

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithStoryboardChildrenNoUids(`<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 200,
          top: 138,
          width: 513,
          height: 364,
          display: 'flex',
          gap: 42,
        }}
      >
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            contain: 'layout',
            gap: 42,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 139,
              height: 130,
              contain: 'layout',
            }}
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 119,
              height: 213,
              contain: 'layout',
            }}
          />
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 99,
            height: 132,
            contain: 'layout',
          }}
        />
      </div>`),
      )
    })
    it('wraps flex column children in a container with flex column set', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboardChildren(`<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 200,
          top: 138,
          width: 513,
          height: 364,
          display: 'flex',
          gap: 42,
          flexDirection: 'column'
        }}
        data-uid='e5b'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 139,
            height: 130,
            contain: 'layout',
          }}
          data-uid='6de'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 119,
            height: 213,
            contain: 'layout',
          }}
          data-uid='8f4'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 99,
            height: 132,
            contain: 'layout',
          }}
          data-uid='0e8'
        />
      </div>`),
        'await-first-dom-report',
      )
      await doGroup(editor)
      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithStoryboardChildrenNoUids(`<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 200,
          top: 138,
          width: 513,
          height: 364,
          display: 'flex',
          gap: 42,
          flexDirection: 'column',
        }}
      >
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            contain: 'layout',
            gap: 42,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 139,
              height: 130,
              contain: 'layout',
            }}
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 119,
              height: 213,
              contain: 'layout',
            }}
          />
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 99,
            height: 132,
            contain: 'layout',
          }}
        />
      </div>`),
      )
    })
    it('wraps flow children in a simple unstyled div', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithStoryboardChildren(`<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 200,
          top: 138,
          width: 429,
          height: 548,
        }}
        data-uid='e5b'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 139,
            height: 130,
            contain: 'layout',
          }}
          data-uid='6de'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 119,
            height: 213,
            contain: 'layout',
          }}
          data-uid='8f4'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 99,
            height: 132,
            contain: 'layout',
          }}
          data-uid='0e8'
        />
      </div>`),
        'await-first-dom-report',
      )

      await doGroup(editor)

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithStoryboardChildrenNoUids(`<div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 200,
          top: 138,
          width: 429,
          height: 548,
        }}
      >
        <div>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 139,
              height: 130,
              contain: 'layout',
            }}
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 119,
              height: 213,
              contain: 'layout',
            }}
          />
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 99,
            height: 132,
            contain: 'layout',
          }}
        />
      </div>`),
      )
    })
  })

  describe('CMD + G to Group', () => {
    it('wraps selected elements in a Group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='container'>
          <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 62,
            top: 60,
            width: 75,
            height: 91,
          }}
          data-uid='aaa'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 151,
            top: 86,
            width: 48,
            height: 48,
          }}
          data-uid='bbb'
        />
          </div>`,
        ),
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/aaa`),
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/bbb`),
      ])

      await expectSingleUndo2Saves(renderResult, async () =>
        pressKey('g', { modifiers: cmdModifier }),
      )

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
          <Group style={{ position: 'absolute', left: 62, top: 60, width: 137, height: 91 }}>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 75,
                height: 91,
              }}
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 89,
                top: 26,
                width: 48,
                height: 48,
              }}
            />
          </Group>
        </div>`,
        ),
      )
    })

    it('wraps selected elements with percentage dimensions in a Group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='container'>
          <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: '5%',
            top: '10%',
            width: '50%',
            height: '20%',
          }}
          data-uid='aaa'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: '10%',
            top: '15%',
            width: '70%',
            height: '80%',
          }}
          data-uid='bbb'
        />
          </div>`,
        ),
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/aaa`),
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/bbb`),
      ])

      await expectSingleUndo2Saves(renderResult, async () =>
        pressKey('g', { modifiers: cmdModifier }),
      )

      const toasts = renderResult.getEditorState().editor.toasts
      expect(toasts).toHaveLength(1)
      const firstToast = safeIndex(toasts, 0)
      expect(firstToast?.id).toEqual('percentage-pin-replaced')

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
          <Group style={{ position: 'absolute', left: 20, top: 40, width: 300, height: 340 }}>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 200,
                height: 80,
              }}
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 20,
                top: 20,
                width: 280,
                height: 320,
              }}
            />
          </Group>
        </div>`,
        ),
      )
    })

    it('if Group is not imported, it is added to the imports after the Group has been inserted', async () => {
      const editor = await renderTestEditorWithCode(
        `import { Scene, Storyboard } from 'utopia-api'
      import { App } from '/src/app.js'
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: -114,
              top: 498,
              width: 35,
              height: 197,
            }}
            data-uid='aaa'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: -31,
              top: 568,
              width: 97,
              height: 247,
            }}
            data-uid='bbb'
          />
        </Storyboard>
      )
      `,
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString(`sb/aaa`), EP.fromString(`sb/bbb`)])

      await expectSingleUndo2Saves(editor, async () => pressKey('g', { modifiers: cmdModifier }))

      // note the added `import * as React`
      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState()))
        .toEqual(`import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { Group } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <Group
      style={{
        position: 'absolute',
        left: -114,
        top: 498,
        width: 180,
        height: 317,
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 35,
          height: 197,
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 83,
          top: 70,
          width: 97,
          height: 247,
        }}
      />
    </Group>
  </Storyboard>
)
`)
    })

    it('cannot wrap empty groups', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa'>
            <Group data-uid='group' />
          </div>
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/group`),
      ])

      await pressKey('g', { modifiers: cmdModifier })
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(`
          <div>
            <Group />
          </div>
        `),
      )
      expect(renderResult.getEditorState().editor.toasts.length).toEqual(1)
      expect(renderResult.getEditorState().editor.toasts[0].message).toEqual(
        'Empty Groups cannot be wrapped into Groups',
      )
    })
  })
})

async function doGroup(editor: EditorRenderResult) {
  await selectComponentsForTest(editor, [EP.fromString(`sb/e5b/6de`), EP.fromString(`sb/e5b/8f4`)])
  await expectSingleUndo2Saves(editor, async () => pressKey('g', { modifiers: cmdModifier }))
}

function expectBoundsToEqual(editor: EditorRenderResult, testId: string, bounds: CanvasRectangle) {
  const element = editor.renderedDOM.getByTestId(testId)
  expect(element.style.position).toEqual('absolute')
  expect(element.style.top).toEqual(`${bounds.y}px`)
  expect(element.style.left).toEqual(`${bounds.x}px`)
  expect(element.style.width).toEqual(`${bounds.width}px`)
  expect(element.style.height).toEqual(`${bounds.height}px`)
}

function expectElementToBeSizelessDiv(editor: EditorRenderResult, testId: string) {
  const element = editor.renderedDOM.getByTestId(testId)
  expect(element.style.position).toEqual('')
  expect(element.style.width).toEqual('')
  expect(element.style.height).toEqual('')
}

function makeTestProjectCodeWithStoryboardChildren(storyboardChildren: string): string {
  const code = `
    import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'

    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid='sb'>
          ${storyboardChildren}
        </Storyboard>
      )
    }
  `

  return formatTestProjectCode(code)
}

function makeTestProjectCodeWithStoryboardChildrenNoUids(storyboardChildren: string): string {
  const code = `
    import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'

    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard>
          ${storyboardChildren}
        </Storyboard>
      )
    }
  `

  return formatTestProjectCode(code)
}
