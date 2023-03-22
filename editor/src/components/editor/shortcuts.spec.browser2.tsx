import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { altCmdModifier, cmdModifier, ctrlModifier, shiftModifier } from '../../utils/modifiers'
import {
  expectNoAction,
  expectSingleUndoStep,
  selectComponentsForTest,
  setFeatureForBrowserTests,
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

      await expectSingleUndoStep(editor, async () => {
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

      await expectSingleUndoStep(editor, async () => {
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

      await expectSingleUndoStep(editor, async () => {
        await pressKey('x')
      })
      await editor.getDispatchFollowUpActionsFinished()

      expect(div.style.position).toEqual('')
      expect(div.style.display).toEqual('inline-block')
      expect(div.style.width).toEqual('182px')
      expect(div.style.height).toEqual('130px')

      await expectSingleUndoStep(editor, async () => {
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

    // eslint-disable-next-line jest/expect-expect
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

      await expectSingleUndoStep(editor, async () => {
        await pressKey('x')
      })

      const groupContainer = editor.renderedDOM.getByTestId('group')
      expect(groupContainer.style.width).toEqual('207px')
      expect(groupContainer.style.height).toEqual('311px')
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

    await expectSingleUndoStep(renderResult, async () => {
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

    await expectSingleUndoStep(renderResult, async () => {
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

    await expectSingleUndoStep(renderResult, async () => {
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

    await expectSingleUndoStep(renderResult, async () => {
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

    await expectSingleUndoStep(renderResult, async () => {
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

    await expectSingleUndoStep(renderResult, async () => {
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

async function doGroup(editor: EditorRenderResult) {
  await selectComponentsForTest(editor, [EP.fromString(`sb/e5b/6de`), EP.fromString(`sb/e5b/8f4`)])
  await expectSingleUndoStep(editor, async () => pressKey('g', { modifiers: cmdModifier }))
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
