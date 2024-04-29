import { screen } from '@testing-library/react'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { createTestProjectWithMultipleFiles } from '../../sample-projects/sample-project-utils.test-utils'
import {
  selectComponentsForTest,
  expectSingleUndo2Saves,
  searchInFloatingMenu,
} from '../../utils/utils.test-utils'
import {
  FOR_TESTS_setNextGeneratedUid,
  FOR_TESTS_setNextGeneratedUids,
} from '../../core/model/element-template-utils.test-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import * as EP from '../../core/shared/element-path'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDragFromPointToPoint,
  mouseMoveToPoint,
  pressKey,
} from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  TestAppUID,
  TestScenePath,
  TestSceneUID,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithComponentInnards,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../canvas/ui-jsx.test-utils'
import {
  CanvasToolbarEditButtonID,
  InsertConditionalButtonTestId,
  InsertMenuButtonTestId,
  PlayModeButtonTestId,
  WrapInDivButtonTestId,
} from './canvas-toolbar'
import { StoryboardFilePath, PlaygroundFilePath, navigatorEntryToKey } from './store/editor-state'
import { cmdModifier } from '../../utils/modifiers'

function slightlyOffsetWindowPointBecauseVeryWeirdIssue(point: { x: number; y: number }) {
  // FIXME when running in headless chrome, the result of getBoundingClientRect will be slightly
  // offset for some unknown reason, meaning the inserted element will be 1 pixel of in each dimension
  return { x: point.x - 0.001, y: point.y - 0.001 }
}

describe('canvas toolbar', () => {
  it('can toggle play mode off by pressing edit button', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
  </div>`),
      'await-first-dom-report',
    )
    expect(editor.getEditorState().editor.mode.type).toEqual('select')

    const playButton = editor.renderedDOM.getByTestId(PlayModeButtonTestId)
    const playButtonCenter = getDomRectCenter(playButton.getBoundingClientRect())
    await mouseClickAtPoint(playButton, playButtonCenter)
    expect(editor.getEditorState().editor.mode.type).toEqual('live')

    const editButton = editor.renderedDOM.getByTestId(CanvasToolbarEditButtonID)
    const editButtonCenter = getDomRectCenter(editButton.getBoundingClientRect())
    await mouseClickAtPoint(editButton, editButtonCenter)
    expect(editor.getEditorState().editor.mode.type).toEqual('select')
  })

  it('can insert conditionals via the canvas toolbar', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div
      data-uid='aaa'
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
      }}
    >
      <div
        data-uid='bbb'
        data-testid='bbb'
        style={{
          position: 'absolute',
          left: 10,
          top: 10,
          width: 380,
          height: 180,
          backgroundColor: '#d3d3d3',
        }}
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 5,
            top: 5,
            width: 1000,
            height: 1000,
          }}
          data-uid='ddd'
        />
      </div>
      <div
        data-uid='ccc'
        style={{
          position: 'absolute',
          left: 10,
          top: 200,
          width: 380,
          height: 190,
          backgroundColor: '#FF0000',
        }}
      />
    </div>
      `),
      'await-first-dom-report',
    )
    const targetElement = editor.renderedDOM.getByTestId('bbb')
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 5,
      y: targetElementBounds.y + 5,
    })
    const endPoint = slightlyOffsetWindowPointBecauseVeryWeirdIssue({
      x: targetElementBounds.x + 1005,
      y: targetElementBounds.y + 1005,
    })

    FOR_TESTS_setNextGeneratedUids(['new-div'])

    const insertMenuButton = editor.renderedDOM.getByTestId(InsertMenuButtonTestId)
    await mouseClickAtPoint(
      insertMenuButton,
      getDomRectCenter(insertMenuButton.getBoundingClientRect()),
    )

    const insertConditionalButton = editor.renderedDOM.getByTestId(InsertConditionalButtonTestId)
    const insertConditionalButtonRect = insertConditionalButton.getBoundingClientRect()
    await mouseClickAtPoint(insertConditionalButton, getDomRectCenter(insertConditionalButtonRect))

    // Move before starting dragging
    await mouseMoveToPoint(canvasControlsLayer, startPoint)

    // Highlight should show the candidate parent
    expect(editor.getEditorState().editor.highlightedViews.map(EP.toUid)).toEqual(['bbb'])

    FOR_TESTS_setNextGeneratedUids([
      'skip1',
      'skip2',
      'skip3',
      'skip4',
      'skip5',
      'skip6',
      'skip7',
      'false-branch',
    ])

    // Drag from inside bbb to inside ccc
    await mouseDragFromPointToPoint(canvasControlsLayer, startPoint, endPoint)

    await editor.getDispatchFollowUpActionsFinished()

    // Check that the inserted element is a child of bbb
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
          <div
            data-uid='aaa'
            style={{
              width: '100%',
              height: '100%',
              backgroundColor: '#FFFFFF',
              position: 'relative',
            }}
          >
            <div
              data-uid='bbb'
              data-testid='bbb'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 380,
                height: 180,
                backgroundColor: '#d3d3d3',
              }}
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 5,
                  top: 5,
                  width: 1000,
                  height: 1000,
                }}
                data-uid='ddd'
              />
              {true ? (
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 5,
                    top: 5,
                    width: 1000,
                    height: 1000,
                  }}
                  data-uid='new-div'
                />
                ) : (
                  <div
                    style={{
                      position: 'absolute',
                      left: 5,
                      top: 5,
                      width: 1000,
                      height: 1000,
                    }}
                    data-uid='fal'
                  >
                    False branch
                  </div>
                )}
            </div>
            <div
              data-uid='ccc'
              style={{
                position: 'absolute',
                left: 10,
                top: 200,
                width: 380,
                height: 190,
                backgroundColor: '#FF0000',
              }}
            />
          </div>
        `),
    )
  })

  it('can insert a conditional via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
  </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await insertViaAddElementPopup(editor, 'cond')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
    {true ? null : null}
  </div>`),
    )
  })

  it('can insert a conditional into a group via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<Group
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
  </Group>`),
      'await-first-dom-report',
    )

    await expectSingleUndo2Saves(editor, async () => {
      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
      ])
      await insertViaAddElementPopup(editor, 'conditional')
    })

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<Group
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
    {true ? null : null}
  </Group>`),
    )
  })

  it('can insert a div via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
  </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    FOR_TESTS_setNextGeneratedUids(['reserved', 'new-div'])

    await insertViaAddElementPopup(editor, 'div')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
    <div
        style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            width: 100,
            height: 100,
            top: 0,
            left: 0,
        }}
        data-uid='new-div'
    />

  </div>`),
    )
  })

  it('can insert a div with no element selected via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
  </div>`),
      'await-first-dom-report',
    )

    FOR_TESTS_setNextGeneratedUids(['reserved', 'new-div'])

    await insertViaAddElementPopup(editor, 'div')

    expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/a3d',
      'regular-utopia-storyboard-uid/new-div',
    ])
  })

  it('can insert a span with sample text', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
  </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    FOR_TESTS_setNextGeneratedUids(['reserved', 'sample-text'])

    await insertViaAddElementPopup(editor, 'sampl')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
    <span data-uid='sample-text' style={{ top: 0, left: 0, position: 'absolute' }}>Sample text</span>
  </div>`),
    )
  })

  it('can insert a span with a variable text', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithComponentInnards(`
      const myText = 'Hello world'
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
      </div>
    )`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    FOR_TESTS_setNextGeneratedUids(['reserved', 'sample-text'])

    await insertViaAddElementPopup(editor, 'myTex')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithComponentInnards(`
      const myText = 'Hello world'
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
        <span data-uid='sample-text' style={{ top: 0, left: 0, position: 'absolute' }}>{myText}</span>
  </div>
  )`),
    )
  })

  it('can insert a span with a variable stringified content', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithComponentInnards(`
      const myObj = { test: 'test' }
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
      </div>
    )`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    FOR_TESTS_setNextGeneratedUids(['reserved', 'sample-text'])

    await insertViaAddElementPopup(editor, 'myObj.test')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithComponentInnards(`
      const myObj = { test: 'test' }
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
        <span data-uid='sample-text' style={{ top: 0, left: 0, position: 'absolute' }}>{myObj.test}</span>
        </div>
    )`),
    )
  })

  it('can insert a conditional variable via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithComponentInnards(`
      const myCondition = true
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
      </div>
    )`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await insertViaAddElementPopup(editor, 'myConditio')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithComponentInnards(`
      const myCondition = true
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
        {myCondition ? null : null}
      </div>
    )`),
    )
  })

  it('can insert an array via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithComponentInnards(`
      const myArray = ['one', 'two', 'three']
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
      </div>
    )`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await insertViaAddElementPopup(editor, 'myArra')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithComponentInnards(`
      const myArray = ['one', 'two', 'three']
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
        <React.Fragment>{myArray.map((item) => (<span>{item}</span>))}</React.Fragment>
      </div>
    )`),
    )
  })

  it('can insert an image via the floating insert menu', async () => {
    FOR_TESTS_setNextGeneratedUids(['myImage'])
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithComponentInnards(`
      const myImage = 'test.png?raw=true'
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
      </div>
    )`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await insertViaAddElementPopup(editor, 'myImag')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithComponentInnards(`
      const myImage = 'test.png?raw=true'
      return (
        <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 57,
          top: 168,
          width: 247,
          height: 402,
        }}
        data-uid='container'
      >
        <div data-uid='a3d' />
        <img src={myImage} data-uid='ele' style={{top:0, left: 0, position: 'absolute'}}/>
      </div>
    )`),
    )
  })

  it('can insert map via the floating insert menu', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#fefefe',
      position: 'absolute',
      left: 46,
      top: 95,
      width: 604,
      height: 256,
      display: 'flex',
      gap: 10,
    }}
    data-uid='flex-row'
  />`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:flex-row`),
    ])

    await insertViaAddElementPopup(editor, 'List')

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'
import { Placeholder } from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        backgroundColor: '#fefefe',
        position: 'absolute',
        left: 46,
        top: 95,
        width: 604,
        height: 256,
        display: 'flex',
        gap: 10,
      }}
      data-uid='flex-row'
    >
      {[1, 2, 3].map(() => (
        <Placeholder data-uid='pla' />
      ))}
    </div>
  )
}

export var storyboard = (props) => {
  return (
    <Storyboard data-uid='utopia-storyboard-uid'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='scene-aaa'
      >
        <App
          data-uid='app-entity'
          style={{
            position: 'absolute',
            bottom: 0,
            left: 0,
            right: 0,
            top: 0,
          }}
        />
      </Scene>
    </Storyboard>
  )
}
`)
  })

  it('can search for and insert default exported component', async () => {
    const editor = await renderTestEditorWithModel(
      createTestProjectWithMultipleFiles({
        [StoryboardFilePath]: `
        import * as React from 'react'
        import { Scene, Storyboard } from 'utopia-api'
        import { Playground } from '/src/playground.js'
        
        export var storyboard = (
          <Storyboard data-uid='sb'>
            <Scene
              style={{
                width: 700,
                height: 759,
                position: 'absolute',
                left: 212,
                top: 128,
              }}
              data-label='Playground'
              data-uid='scene-1'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 136,
                  top: 77,
                  width: 275,
                  height: 303,
                }}
                data-uid='insert-target'
            />
            </Scene>
          </Storyboard>
        )
        `,
        [PlaygroundFilePath]: `            
        export default function DefaultExportedComponent() {
          return (
            <div
              style={{
                height: '100%',
                width: '100%',
                contain: 'layout',
              }}
              data-uid='pg-root'
            >
              <div
                style={{
                  height: 300,
                  position: 'absolute',
                  width: 300,
                  left: 154,
                  top: 134,
                  backgroundColor: '#ff7262',
                }}
                data-uid='pg-container'
              />
            </div>
          )
        }
        
        `,
      }),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('sb/scene-1/insert-target')])

    await pressKey('a')
    await searchInFloatingMenu(editor, 'DefaultExportedComp')

    expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState()))
      .toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { Playground } from '/src/playground.js'
import DefaultExportedComponent from '/src/playground.js'

export var storyboard = (
  <Storyboard>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 136,
          top: 77,
          width: 275,
          height: 303,
        }}
      >
        <DefaultExportedComponent
          style={{ top: 0, left: 0, position: 'absolute' }}
        />
      </div>
    </Scene>
  </Storyboard>
)
`)
  })

  it('can insert two elements one after the other', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
  </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    FOR_TESTS_setNextGeneratedUids(['reserved', 'new-img'])
    await insertViaAddElementPopup(editor, 'img')

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    FOR_TESTS_setNextGeneratedUids(['reserved', 'new-img-2'])
    await insertViaAddElementPopup(editor, 'img')

    expect(editor.getActionsCausingDuplicateUIDs()).toHaveLength(0)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 57,
      top: 168,
      width: 247,
      height: 402,
    }}
    data-uid='container'
  >
    <div data-uid='a3d' />
    <img
      style={{
        width: '64px',
        height: '64px',
        position: 'absolute',
        top: 0,
        left: 0,
      }}
      src='/editor/utopia-logo-white-fill.png?hash=nocommit'
      data-uid='new-img'
    />
    <img
      style={{
        width: '64px',
        height: '64px',
        position: 'absolute',
        top: 0,
        left: 0,
      }}
      src='/editor/utopia-logo-white-fill.png?hash=nocommit'
      data-uid='new-img-2'
    />
  </div>`),
    )
  })

  describe('add element to conditional', () => {
    it(`when the root of a conditional is selected, element is added as a sibling`, async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? null : <div>"Hello there"</div>
        }
        </div>
        `),
        'await-first-dom-report',
      )

      const slot = editor.renderedDOM.getByText('Conditional')
      await mouseClickAtPoint(slot, { x: 5, y: 5 })

      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa/app-entity:container/conditional',
      ])

      await insertViaAddElementPopup(editor, 'img')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          {
            // @utopia/uid=conditional
            [].length === 0 ? null : (
              <div data-uid='33d'>"Hello there"</div>
            )
          }
          <img
            style={{
              width: '64px',
              height: '64px',
              position: 'absolute',
              top: 0,
              left: 0,
            }}
            src='/editor/utopia-logo-white-fill.png?hash=nocommit'
            data-uid='ele'
          />
        </div>
      `),
      )
    })
    it('add element to true branch of a conditional', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? null : <span data-uid='hello'>Hello there</span>
        }
        </div>
        `),
        'await-first-dom-report',
      )

      FOR_TESTS_setNextGeneratedUids(['reserved for fragment wrapper', 'newly-added-img'])

      await clickEmptySlot(editor)
      await expectSingleUndo2Saves(editor, () => insertViaAddElementPopup(editor, 'img'))

      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa/app-entity:container/conditional/newly-added-img',
      ])

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? (
            <img
              style={{
                width: '64px',
                height: '64px',
                position: 'absolute',
                top: 0,
                left: 0,
              }}
              src='/editor/utopia-logo-white-fill.png?hash=nocommit'
              data-uid='newly-added-img'
            />
          ) : (
            <span data-uid='hello'>Hello there</span>
          )
        }
        </div>
      `),
      )
    })

    it('add element to false branch of a conditional', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? <span data-uid='hello'>Hello there</span> : null
        }
        </div>
        `),
        'await-first-dom-report',
      )

      await clickEmptySlot(editor) // This click will add an override
      FOR_TESTS_setNextGeneratedUids(['reserved for fragment wrapper', 'newly-added-img'])
      await expectSingleUndo2Saves(editor, () => insertViaAddElementPopup(editor, 'img'))

      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa/app-entity:container/conditional/newly-added-img',
      ])

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div data-uid='container'>
      {
        // @utopia/uid=conditional
        // @utopia/conditional=false
        [].length === 0 ? (
          <span data-uid='hello'>Hello there</span>
        ) : (
          <img
            style={{
              width: '64px',
              height: '64px',
              position: 'absolute',
              top: 0,
              left: 0,
            }}
            src='/editor/utopia-logo-white-fill.png?hash=nocommit'
            data-uid='newly-added-img'
          />
        )
      }
      </div>
      `),
      )
    })

    it('adding an element in conditional slot next to an element that does not support children', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
        {/* @utopia/uid=conditional */
          true ?  (
          <img
            style={{
              width: '64px',
              height: '64px',
              position: 'absolute',
            }}
            src='/editor/icons/favicons/favicon-128.png?hash=3334bc1ac8ae28310d92d7ad97c4b466428cd1e7'
            data-uid='img'
            data-label='img'
          />
        ) : null}
        </div>
        `),
        'await-first-dom-report',
      )

      const slot = editor.renderedDOM.getByText('img')
      await mouseClickAtPoint(slot, { x: 5, y: 5 })

      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa/app-entity:container/conditional/img',
      ])

      FOR_TESTS_setNextGeneratedUids(['reserved for fragment wrapper', 'newly-added-img'])

      await expectSingleUndo2Saves(editor, () => insertViaAddElementPopup(editor, 'img'))

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
        {
          /* @utopia/uid=conditional */
          true ? (
            <React.Fragment>
              <img
                style={{
                  width: '64px',
                  height: '64px',
                  position: 'absolute',
                }}
                src='/editor/utopia-logo-white-fill.png?hash=nocommit'
                data-uid='newly-added-img'
              />
              <img
                style={{
                  width: '64px',
                  height: '64px',
                  position: 'absolute',
                }}
                src='/editor/icons/favicons/favicon-128.png?hash=3334bc1ac8ae28310d92d7ad97c4b466428cd1e7'
                data-uid='img'
                data-label='img'
              />
            </React.Fragment>
          ) : (
            null
          )
        }
        </div>
      `),
      )
    })

    it('add element to element in conditional slot - supports children', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
        {
          // @utopia/uid=ae8
          true ? <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 77,
              top: 235,
              width: 96,
              height: 115,
            }}
            data-uid='52b'
          /> : null
        }
        </div>
        `),
        'await-first-dom-report',
      )

      const slot = editor.renderedDOM.getByTestId(
        'NavigatorItemTestId-regular_utopia_storyboard_uid/scene_aaa/app_entity:container/ae8/52b',
      )
      await mouseClickAtPoint(slot, { x: 5, y: 5 })

      FOR_TESTS_setNextGeneratedUids(['reserved for fragment wrapper', 'newly-added-img'])

      await expectSingleUndo2Saves(editor, () => insertViaAddElementPopup(editor, 'img'))

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div data-uid='container'>
      {
        // @utopia/uid=ae8
        true ? (
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 77,
              top: 235,
              width: 96,
              height: 115,
            }}
            data-uid='52b'
          >
            <img
              style={{
                width: '64px',
                height: '64px',
                position: 'absolute',
                top: 0,
                left: 0,
              }}
              src='/editor/utopia-logo-white-fill.png?hash=nocommit'
              data-uid='newly-added-img'
            />
          </div>
        ) : null}
      </div>
      `),
      )
    })
  })
  describe('Floating menu converts element', () => {
    it('can convert an element to a fragment', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa'>
          <div data-uid='bbb' style={{backgroundColor: 'blue'}}>
            <div data-uid='ccc'>hello</div>
            <div data-uid='ddd'>hello2</div>
          </div>
        </div>
      `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`),
      ])

      await convertViaAddElementPopup(editor, 'fragment')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa'>
          <React.Fragment>
            <div data-uid='ccc'>hello</div>
            <div data-uid='ddd'>hello2</div>
          </React.Fragment>
        </div>
      `),
      )
    })
    it('can convert a fragment to a div element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa'>
          <React.Fragment data-uid='bbb'>
            <div data-uid='ccc'>hello</div>
            <div data-uid='ddd'>hello2</div>
          </React.Fragment>
        </div>
      `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`),
      ])

      await convertViaAddElementPopup(editor, 'div')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa'>
          <div data-uid='bbb'>
            <div data-uid='ccc'>hello</div>
            <div data-uid='ddd'>hello2</div>
          </div>
        </div>
      `),
      )
    })
  })
  describe('Imports React when inserts/wraps in fragment', () => {
    async function setup() {
      const editor = await renderTestEditorWithModel(
        createTestProjectWithMultipleFiles({
          [StoryboardFilePath]: `
          import * as React from 'react'
          import { Scene, Storyboard } from 'utopia-api'
          import { Playground } from '/src/playground.js'
          
          export var storyboard = (
            <Storyboard data-uid='sb'>
              <Scene
                style={{
                  width: 700,
                  height: 759,
                  position: 'absolute',
                  left: 212,
                  top: 128,
                }}
                data-label='Playground'
                data-uid='scene-1'
              >
                <Playground style={{}} data-uid='playground' />
              </Scene>
            </Storyboard>
          )
          `,
          [PlaygroundFilePath]: `            
          export var Playground = () => {
            return (
              <div
                style={{
                  height: '100%',
                  width: '100%',
                  contain: 'layout',
                }}
                data-uid='pg-root'
              >
                <div
                  style={{
                    height: 300,
                    position: 'absolute',
                    width: 300,
                    left: 154,
                    top: 134,
                    backgroundColor: '#ff7262',
                  }}
                  data-uid='pg-container'
                />
              </div>
            )
          }
          
          `,
        }),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString('sb/scene-1/playground:pg-root/pg-container'),
      ])

      return editor
    }

    it('when wrapping into fragment', async () => {
      const editor = await setup()

      await pressKey('w')
      await searchInFloatingMenu(editor, 'fragm')

      expect(getPrintedUiJsCode(editor.getEditorState(), PlaygroundFilePath))
        .toEqual(`import * as React from 'react'
export var Playground = () => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='pg-root'
    >
      <React.Fragment>
        <div
          style={{
            height: 300,
            position: 'absolute',
            width: 300,
            left: 154,
            top: 134,
            backgroundColor: '#ff7262',
          }}
          data-uid='pg-container'
        />
      </React.Fragment>
    </div>
  )
}
`)
    })

    it('when inserting a fragment', async () => {
      const editor = await setup()

      await pressKey('a')
      await searchInFloatingMenu(editor, 'fragm')

      expect(getPrintedUiJsCode(editor.getEditorState(), PlaygroundFilePath))
        .toEqual(`import * as React from 'react'
export var Playground = () => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='pg-root'
    >
      <div
        style={{
          height: 300,
          position: 'absolute',
          width: 300,
          left: 154,
          top: 134,
          backgroundColor: '#ff7262',
        }}
        data-uid='pg-container'
      >
        <React.Fragment />
      </div>
    </div>
  )
}
`)
    })

    it('when converting into fragment', async () => {
      const editor = await setup()

      await pressKey('s')
      await searchInFloatingMenu(editor, 'fragm')

      expect(getPrintedUiJsCode(editor.getEditorState(), PlaygroundFilePath))
        .toEqual(`import * as React from 'react'
export var Playground = () => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='pg-root'
    >
      <React.Fragment />
    </div>
  )
}
`)
    })
  })

  describe('groups', () => {
    it('can wrap elements in a group', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 57,
              top: 168,
              width: 247,
              height: 402,
            }}
            data-uid='container'
          >
            <div data-uid='target' />
          </div>
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/target`),
      ])

      FOR_TESTS_setNextGeneratedUid('new-group')

      await wrapViaAddElementPopup(editor, 'group')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 57,
              top: 168,
              width: 247,
              height: 402,
            }}
            data-uid='container'
          >
            <Group
              style={{ position: 'absolute', left: 0, top: 0 }}
              data-uid='new-group'
            >
              <div data-uid='target' />
            </Group>
          </div>
      `),
      )
    })
    it('cannot insert groups because they are not available for insert', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 57,
              top: 168,
              width: 247,
              height: 402,
            }}
            data-uid='container'
          >
            <div data-uid='target' />
          </div>
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/target`),
      ])

      await insertViaAddElementPopup(editor, 'group')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 57,
              top: 168,
              width: 247,
              height: 402,
            }}
            data-uid='container'
          >
            <div data-uid='target' />
          </div>
      `),
      )
    })
  })
})

async function clickEmptySlot(editor: EditorRenderResult) {
  const slot = editor.renderedDOM.getByText('Empty')
  await mouseClickAtPoint(slot, { x: 5, y: 5 })
}

async function insertViaAddElementPopup(editor: EditorRenderResult, query: string) {
  await pressKey('a')
  await searchInFloatingMenu(editor, query)
}

async function wrapViaAddElementPopup(editor: EditorRenderResult, query: string) {
  await pressKey('g')
  await searchInFloatingMenu(editor, query)
}

async function convertViaAddElementPopup(editor: EditorRenderResult, query: string) {
  await pressKey('s')
  await searchInFloatingMenu(editor, query)
}
