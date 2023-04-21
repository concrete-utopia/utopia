import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { canvasRectangle, CanvasRectangle } from '../../core/shared/math-utils'
import { altCmdModifier, cmdModifier, ctrlModifier, shiftModifier } from '../../utils/modifiers'
import {
  expectNoAction,
  expectSingleUndo2Saves,
  selectComponentsForTest,
  wait,
} from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { keyDown, mouseClickAtPoint, pressKey } from '../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
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

      await wait(5000)

      keyDown('c', { modifiers: ctrlModifier })

      await wait(5000)

      const div2 = editor.renderedDOM.getByTestId(TestIdTwo)
      const div2Bounds = div2.getBoundingClientRect()
      const div2Corner = {
        x: div2Bounds.x + 50,
        y: div2Bounds.y + 40,
      }

      await mouseClickAtPoint(canvasControlsLayer, div2Corner)

      await editor.getDispatchFollowUpActionsFinished()

      await wait(10000)

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
        projectWithContentAffectingElements,
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/fragment')])

      await expectNoAction(editor, async () => {
        await pressKey('x')
      })
    })

    it('pressing x when an unsized simple div is selected sizes it to wrap its children', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithContentAffectingElements,
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/group')])

      await expectSingleUndo2Saves(editor, async () => {
        await pressKey('x')
      })

      const groupContainer = editor.renderedDOM.getByTestId('group')
      expect(groupContainer.style.width).toEqual('207px')
      expect(groupContainer.style.height).toEqual('311px')
    })

    it('pressing x converts a sizeless div into a absolutely positioned container, sized to the AABB if its children', async () => {
      const editor = await renderTestEditorWithCode(
        `
      import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='group' data-testid='group'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 232,
          top: 305,
          width: 363,
          height: 167,
        }}
        data-uid='515'
        data-testid='c1'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 316,
          top: 504,
          width: 163,
          height: 98,
        }}
        data-uid='df9'
        data-testid='c2'
      />
    </div>
  </Storyboard>
)
`,
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/group')])

      await expectSingleUndo2Saves(editor, async () => {
        await pressKey('x')
      })

      expectBoundsToEqual(
        editor,
        'group',
        canvasRectangle({ x: 232, y: 305, width: 363, height: 297 }),
      )

      expectBoundsToEqual(editor, 'c1', canvasRectangle({ x: 0, y: 0, width: 363, height: 167 }))

      expectBoundsToEqual(editor, 'c2', canvasRectangle({ x: 84, y: 199, width: 163, height: 98 }))
    })

    it('pressing x on an absolute positioned container with only absolute children converts it back into a sizeless div', async () => {
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

      expectElementToBeSizelessDiv(editor, 'container')

      expectBoundsToEqual(
        editor,
        'child1',
        canvasRectangle({ x: 227, y: 83, width: 77, height: 35 }),
      )

      expectBoundsToEqual(
        editor,
        'child2',
        canvasRectangle({ x: 348, y: 83, width: 94, height: 55 }),
      )
    })

    describe('pressing x in nested groups only affects the selected group and its absolutely positoned children', () => {
      it('converting the outermost group doesnt convert the intermediate groups', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithNestedGroups,
          'await-first-dom-report',
        )
        await selectComponentsForTest(editor, [EP.fromString('sb/outermost-group')])

        await expectSingleUndo2Saves(editor, async () => {
          await pressKey('x')
        })

        expectBoundsToEqual(
          editor,
          'outermost-group',
          canvasRectangle({ x: 232, y: 305, width: 924, height: 309 }),
        )

        expectElementToBeSizelessDiv(editor, 'middle-group')
        expectElementToBeSizelessDiv(editor, 'group')

        expectBoundsToEqual(
          editor,
          'group-child-1',
          canvasRectangle({
            x: 0,
            y: 0,
            width: 363,
            height: 167,
          }),
        )

        expectBoundsToEqual(
          editor,
          'group-child-2',
          canvasRectangle({
            x: 84,
            y: 199,
            width: 163,
            height: 98,
          }),
        )

        expectBoundsToEqual(
          editor,
          'outermost-group-child',
          canvasRectangle({
            x: 537,
            y: 64,
            width: 387,
            height: 245,
          }),
        )
      })

      it('converting a nested group doesnt convert the enclosing groups', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithNestedGroups,
          'await-first-dom-report',
        )
        await selectComponentsForTest(editor, [
          EP.fromString('sb/outermost-group/middle-group/group'),
        ])

        await expectSingleUndo2Saves(editor, async () => {
          await pressKey('x')
        })

        expectBoundsToEqual(
          editor,
          'group',
          canvasRectangle({ x: 232, y: 305, width: 363, height: 297 }),
        )

        expectBoundsToEqual(
          editor,
          'group-child-1',
          canvasRectangle({
            x: 0,
            y: 0,
            width: 363,
            height: 167,
          }),
        )

        expectBoundsToEqual(
          editor,
          'group-child-2',
          canvasRectangle({
            x: 84,
            y: 199,
            width: 163,
            height: 98,
          }),
        )

        expectElementToBeSizelessDiv(editor, 'middle-group')
        expectElementToBeSizelessDiv(editor, 'outermost-group')
      })

      it('converting back to group with nested groups', async () => {
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
        <div data-testid='nested-group'>
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

        expectElementToBeSizelessDiv(editor, 'container')
        expectElementToBeSizelessDiv(editor, 'nested-group')

        expectBoundsToEqual(
          editor,
          'child1',
          canvasRectangle({ x: 227, y: 83, width: 77, height: 35 }),
        )

        expectBoundsToEqual(
          editor,
          'child2',
          canvasRectangle({ x: 348, y: 83, width: 94, height: 55 }),
        )
      })
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

const projectWithContentAffectingElements = `import * as React from 'react'
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

const projectWithNestedGroups = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='outermost-group' data-testid='outermost-group'>
      <div data-uid='middle-group' data-testid='middle-group'>
        <div data-uid='group' data-testid='group'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 232,
              top: 305,
              width: 363,
              height: 167,
            }}
            data-uid='515'
            data-testid='group-child-1'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 316,
              top: 504,
              width: 163,
              height: 98,
            }}
            data-uid='df9'
            data-testid='group-child-2'
          />
        </div>
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 769,
          top: 369,
          width: 387,
          height: 245,
        }}
        data-uid='321'
        data-testid='outermost-group-child'
      />
    </div>
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

  describe('grouping in fragment', () => {
    it('wraps selected elements in a fragment', async () => {
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

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='container'>
          <React.Fragment>
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
          </React.Fragment>
        </div>`,
        ),
      )
    })

    it('if react is not imported, it is added to the imports after the fragment has been inserted', async () => {
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
      expect(getPrintedUiJsCode(editor.getEditorState()))
        .toEqual(`import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import * as React from 'react'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <React.Fragment>
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
    </React.Fragment>
  </Storyboard>
)
`)
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
