import { act, fireEvent, queryByAttribute } from '@testing-library/react'
import {
  FOR_TESTS_setNextGeneratedUid,
  FOR_TESTS_setNextGeneratedUids,
} from '../../../core/model/element-template-utils.test-utils'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { expectSingleUndo2Saves, selectComponentsForTest } from '../../../utils/utils.test-utils'
import { mouseClickAtPoint, pressKey } from '../event-helpers.test-utils'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { FloatingMenuTestId } from './floating-insert-menu'
import { expectNoAction } from '../../../utils/utils.test-utils'
import { PlaygroundFilePath, StoryboardFilePath } from '../../editor/store/editor-state'
import { createTestProjectWithMultipleFiles } from '../../../sample-projects/sample-project-utils.test-utils'

describe('Floating insert menu', () => {
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
            position: 'absolute'
        }}
        data-uid='new-div'
    />

  </div>`),
    )
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
    <span data-uid='sample-text'>Sample text</span>
  </div>`),
    )
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
        <DefaultExportedComponent />
      </div>
    </Scene>
  </Storyboard>
)
`)
  })

  describe('add element to conditional', () => {
    it(`can't add element to the root of a conditional`, async () => {
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

      const initialCode = getPrintedUiJsCode(editor.getEditorState())

      const slot = editor.renderedDOM.getByText('Conditional')
      await mouseClickAtPoint(slot, { x: 5, y: 5 })

      expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'utopia-storyboard-uid/scene-aaa/app-entity:container/conditional',
      ])
      // await wait(1000000)

      await expectNoAction(editor, () => insertViaAddElementPopup(editor, 'img'))

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialCode)
      expectChildrenNotSupportedToastToBePresent(editor)
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
              }}
              src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
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
            }}
            src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
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
                src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
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
              }}
              src='/editor/icons/favicons/favicon-128.png?hash=nocommit'
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

      await pressKey('c')
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
  await pressKey('c')
  await searchInFloatingMenu(editor, query)
}

async function searchInFloatingMenu(editor: EditorRenderResult, query: string) {
  const floatingMenu = editor.renderedDOM.getByTestId(FloatingMenuTestId)
  const searchBox = queryByAttribute('type', floatingMenu, 'text')!

  await act(() => {
    fireEvent.focus(searchBox)
    fireEvent.change(searchBox, { target: { value: query } })
    fireEvent.blur(searchBox)
    fireEvent.keyDown(searchBox, { key: 'Enter', keyCode: 13, metaKey: true })
  })
}

function expectChildrenNotSupportedToastToBePresent(editor: EditorRenderResult) {
  expect(editor.getEditorState().editor.toasts.length).toEqual(1)
  expect(editor.getEditorState().editor.toasts[0].level).toEqual('INFO')
  expect(editor.getEditorState().editor.toasts[0].message).toEqual(
    'Selected element does not support children',
  )
  expect(editor.getEditorState().editor.toasts[0].persistent).toEqual(false)
}
