import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import { windowPoint, WindowPoint } from '../../../core/shared/math-utils'
import { cmdModifier, Modifiers } from '../../../utils/modifiers'
import { PrettierConfig } from 'utopia-vscode-common'
import * as Prettier from 'prettier/standalone'
import {
  BakedInStoryboardVariableName,
  BakedInStoryboardUID,
} from '../../../core/model/scene-utils'
import { getCursorForOverlay } from '../controls/select-mode/cursor-overlay'
import { CSSCursor } from '../canvas-types'
import { NO_OP } from '../../../core/shared/utils'
import { mouseClickAtPoint, mouseDragFromPointWithDelta } from '../event-helpers.test-utils'

interface CheckCursor {
  cursor: CSSCursor | null
}

function dragElement(
  renderResult: EditorRenderResult,
  targetTestId: string,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  checkCursor: CheckCursor | null,
) {
  const targetElement = renderResult.renderedDOM.getByTestId(targetTestId)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

  const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  const midDragCallback =
    checkCursor == null
      ? NO_OP
      : () => {
          expect(getCursorForOverlay(renderResult.getEditorState().editor)).toEqual(
            checkCursor.cursor,
          )
        }

  mouseClickAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers: modifiers,
    midDragCallback: midDragCallback,
  })
}

function getChildrenHiderProjectCode(shouldHide: boolean): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
export const ChildrenHider = (props) => {
  return (
    <div data-uid='33d' style={{ ...props.style }}>
      {props.shouldHide ? null : props.children}
    </div>
  )
}
export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='${TestSceneUID}'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 700,
        }}
        data-uid='outer-div'
      >
        <ChildrenHider
          style={{
            backgroundColor: '#0091FFAA',
            position: 'absolute',
            left: 41,
            top: 37,
            width: 338,
            height: 144,
            gap: 10,
          }}
          data-uid='children-hider'
          shouldHide={${shouldHide}}
        >
        </ChildrenHider>
        <div
          style={{
            backgroundColor: '#0091FFAA',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 190,
            top: 211,
          }}
          data-uid='child-to-reparent'
          data-testid='child-to-reparent'
        >
          drag me
        </div>
      </div>
    </Scene>
  </Storyboard>
)`
}

describe('Absolute Reparent Strategy', () => {
  it('reparents to the canvas root', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 120 }}
            data-uid='bbb'
            data-testid='bbb'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <div style={{width: '100%', height: '100%'}} data-uid='aaa' />
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
        <div
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: -960, top: -950, width: 200, height: 120 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
  it('does not reparent to an element with only text children', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 50, width: 200, height: 100 }}
            data-uid='bbb'
            data-testid='bbb'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 150, width: 200, height: 100 }}
            data-uid='ccc'
          >
            Can't drop here
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 0, y: 150 })
    dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    return (
      <div style={{width: '100%', height: '100%'}} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 200, width: 200, height: 100 }}
          data-uid='bbb'
          data-testid='bbb'
        />
        <div
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 150, width: 200, height: 100 }}
          data-uid='ccc'
        >
          Can't drop here
        </div>
      </div>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      ),
    )
  })
  it('referencing a value from outside the element prevents reparenting', async () => {
    function createCodeForProject(left: number, top: number) {
      return Prettier.format(
        `
  import * as React from 'react'
  import { Scene, Storyboard, View } from 'utopia-api'

  export var App = (props) => {
    const elementWidth = 200
    return (
      <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${left}, top: ${top}, width: elementWidth, height: 120 }}
          data-uid='bbb'
          data-testid='bbb'
        />
      </div>
    )
  }

  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid='${BakedInStoryboardUID}'>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
        >
          <App
            data-uid='${TestAppUID}'
            style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
          />
        </Scene>
      </Storyboard>
    )
  }
`,
        PrettierConfig,
      )
    }

    const renderResult = await renderTestEditorWithCode(
      createCodeForProject(40, 50),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: -1000, y: -1000 })
    dragElement(renderResult, 'bbb', dragDelta, cmdModifier, null)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(createCodeForProject(40, 50))
  })

  it('renders correctly with ChildrenHider set to hide children', async () => {
    const renderResult = await renderTestEditorWithCode(
      getChildrenHiderProjectCode(true),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 0, y: -150 })
    dragElement(renderResult, 'child-to-reparent', dragDelta, cmdModifier, {
      cursor: CSSCursor.NotPermitted,
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
      'utopia-storyboard-uid',
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/outer-div',
      'utopia-storyboard-uid/scene-aaa/outer-div/children-hider',
    ])
  })
  it('renders correctly with ChildrenHider set to show children', async () => {
    const renderResult = await renderTestEditorWithCode(
      getChildrenHiderProjectCode(false),
      'await-first-dom-report',
    )

    const dragDelta = windowPoint({ x: 0, y: -150 })
    dragElement(renderResult, 'child-to-reparent', dragDelta, cmdModifier, {
      cursor: CSSCursor.Move,
    })

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
      'utopia-storyboard-uid',
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/outer-div',
      'utopia-storyboard-uid/scene-aaa/outer-div/children-hider',
      'utopia-storyboard-uid/scene-aaa/outer-div/children-hider/child-to-reparent',
    ])
  })
})
