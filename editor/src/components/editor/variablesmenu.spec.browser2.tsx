import { act, fireEvent, screen } from '@testing-library/react'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  TestAppUID,
  TestSceneUID,
  getPrintedUiJsCode,
  makeTestProjectCodeWithComponentInnards,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithProjectContent,
} from '../canvas/ui-jsx.test-utils'
import * as Prettier from 'prettier'
import * as EP from '../../core/shared/element-path'
import { setPanelVisibility, setRightMenuTab } from './actions/action-creators'
import { RightMenuTab } from './store/editor-state'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { forceNotNull } from '../../core/shared/optional-utils'
import { InsertMenuFilterTestId } from './insertmenu'
import { codeFile } from '../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from '../assets'
import { contentsToTree } from '../assets'
import { PrettierConfig } from 'utopia-vscode-common'
import { pressKey } from '../canvas/event-helpers.test-utils'
import { notice } from '../common/notice'

function getInsertItems() {
  return screen.queryAllByTestId(/^insert-item-/gi)
}

function openVariablesMenu(renderResult: EditorRenderResult) {
  return renderResult.dispatch(
    [setPanelVisibility('rightmenu', true), setRightMenuTab(RightMenuTab.Variables)],
    true,
  )
}

describe('variables menu', () => {
  describe('filter search', () => {
    it('can filter by variable name', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithComponentInnards(`
          const myObj = { test: 'test', num: 5, image: 'img.png' }
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

      await openVariablesMenu(editor)

      expect(getInsertItems().length).toEqual(4)

      document.execCommand('insertText', false, 'myObj.im')

      expect(getInsertItems().length).toEqual(1)
      expect(getInsertItems()[0].innerText).toEqual('image')
    })

    describe('no match', () => {
      it('shows all elements', async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`<div />`),
          'await-first-dom-report',
        )

        await openVariablesMenu(renderResult)

        expect(getInsertItems().length).toEqual(0)
      })
    })

    it('does not show insertables that cannot be inserted', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithComponentInnards(`
          const myObj = { test: 'test', num: 5, image: 'img.png' }
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

      await openVariablesMenu(editor)

      expect(getInsertItems().length).toEqual(0)
    })
  })

  describe('props', () => {
    it('shows and inserts props in scope', async () => {
      const editor = await renderTestEditorWithProjectContent(
        makeTestProjectContents(),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
      ])

      await openVariablesMenu(editor)

      document.execCommand('insertText', false, 'imgProp')
      expect(getInsertItems().length).toEqual(1)
      expect(getInsertItems()[0].innerText).toEqual('imgProp')

      const filterBox = await screen.findByTestId(InsertMenuFilterTestId)
      forceNotNull('the filter box must not be null', filterBox)

      await pressKey('Enter', { targetElement: filterBox })

      expect(getPrintedUiJsCode(editor.getEditorState(), '/src/app.js')).toEqual(
        Prettier.format(
          `
        import * as React from 'react'
        export function App({objProp, imgProp, unusedProp}) {
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
            <img src={imgProp} style={{width: 100, height: 100, top:0, left: 0, position: 'absolute'}} data-uid='ele'/>
          </div>
          )
        }
    `,
          PrettierConfig,
        ),
      )
    })
    it('shows and does not insert scoped properties when not possible', async () => {
      const editor = await renderTestEditorWithProjectContent(
        makeMappingFunctionTestProjectContents(),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/676/020~~~1`,
        ),
      ])

      await openVariablesMenu(editor)

      document.execCommand('insertText', false, 'value')
      expect(getInsertItems().length).toEqual(1)
      expect(getInsertItems()[0].innerText).toEqual('value')

      const filterBox = await screen.findByTestId(InsertMenuFilterTestId)
      forceNotNull('the filter box must not be null', filterBox)

      await pressKey('Enter', { targetElement: filterBox })

      expect(getPrintedUiJsCode(editor.getEditorState(), '/src/app.js')).toEqual(
        Prettier.format(mappingFunctionAppJS, PrettierConfig),
      )
      expect(editor.getEditorState().editor.toasts).toEqual([
        notice(
          'Cannot find a suitable parent',
          'INFO',
          false,
          'to-insert-does-not-support-children',
        ),
      ])
    })
    xit('shows and inserts scoped properties when possible', async () => {
      // Test disabled because it uncovered an issue with an exception being triggered for a frame on the canvas.
      // But it has been seemingly impossible to capture the exception in a way that permits the test to continue.
      // Solution appears to be to fix the flickering of an error in subsequent work.
      const editor = await renderTestEditorWithProjectContent(
        makeMappingFunctionTestProjectContents(),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/1ac/586~~~1`,
        ),
      ])

      await openVariablesMenu(editor)

      document.execCommand('insertText', false, 'index')
      expect(getInsertItems().length).toEqual(1)
      expect(getInsertItems()[0].innerText).toEqual('index')

      const filterBox = await screen.findByTestId(InsertMenuFilterTestId)
      forceNotNull('the filter box must not be null', filterBox)

      await pressKey('Enter', { targetElement: filterBox })

      expect(getPrintedUiJsCode(editor.getEditorState(), '/src/app.js')).toEqual(
        Prettier.format(
          `
          import * as React from 'react'
          export function App({objProp, imgProp, unusedProp}) {
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
              {[1, 2].map((value, index) => {
                return (
                  <img
                    src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
                    alt='Utopia logo'
                    style={{ width: 118, height: 150 }}
                  />
                )
              })}
              {[{ thing: 1 }, { thing: 2 }, { thing: 3 }].map(
                (someValue, index) => {
                  return (
                    <div
                    >
                      <img
                        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
                        alt='Utopia logo'
                        style={{ width: 118, height: 150 }}
                      />
                    </div>
                  )
                },
              )}
            </div>
            )
          }`,
          PrettierConfig,
        ),
      )
    })
  })

  describe('insertion', () => {
    it('inserts an image from within an object', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithComponentInnards(`
          const myObj = { test: 'test', num: 5, image: 'img.png' }
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

      await openVariablesMenu(editor)

      document.execCommand('insertText', false, 'myObj.image')

      const filterBox = await screen.findByTestId(InsertMenuFilterTestId)
      forceNotNull('the filter box must not be null', filterBox)

      await pressKey('Enter', { targetElement: filterBox })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithComponentInnards(`
        const myObj = { test: 'test', num: 5, image: 'img.png' }
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
          <img src={myObj.image} style={{width: 100, height: 100, top:0, left: 0, position: 'absolute'}} data-uid='ele'/>
          </div>
      )`),
      )
    })
    it('inserts a text', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithComponentInnards(`
          const myText = ''
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

      await openVariablesMenu(editor)

      document.execCommand('insertText', false, 'myText')

      const filterBox = await screen.findByTestId(InsertMenuFilterTestId)
      forceNotNull('the filter box must not be null', filterBox)

      await pressKey('Enter', { targetElement: filterBox })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithComponentInnards(`
        const myText = ''
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
          <span style={{ width: 100, height: 100, top: 0, left: 0, position: 'absolute' }} data-uid='ele'>{myText}</span>
          </div>
      )`),
      )
    })
  })
})

function makeTestProjectContents(): ProjectContentTreeRoot {
  return contentsToTree({
    ['/package.json']: codeFile(
      `
{
  "name": "Utopia Project",
  "version": "0.1.0",
  "utopia": {
    "main-ui": "utopia/storyboard.js",
    "html": "public/index.html",
    "js": "src/index.js"
  },
  "dependencies": {
    "react": "16.13.1",
    "react-dom": "16.13.1",
    "utopia-api": "0.4.1",
    "non-existant-dummy-library": "8.0.27",
    "@heroicons/react": "1.0.1",
    "@emotion/react": "11.9.3"
  }
}`,
      null,
    ),
    ['/src/app.js']: codeFile(
      `
    import * as React from 'react'
    export function App({objProp, imgProp, unusedProp}) {
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
      )
    }
    `,
      null,
    ),
    ['/utopia/storyboard.js']: codeFile(
      `
    import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'
    import { App } from '/src/app.js'

    export var storyboard = (
      <Storyboard data-uid='utopia-storyboard-uid' data-testid='utopia-storyboard-uid'>
        <Scene
          style={{
            width: 744,
            height: 1133,
            display: 'flex',
            flexDirection: 'column',
            left: -52,
            top: 9,
            position: 'absolute',
          }}
          data-label='Main Scene'
          data-uid='scene-aaa'
          data-testid='scene-aaa'
        >
        <App objProp={{key1: 'key1'}} imgProp='test.png' nonExistentProp='non' data-uid='app-entity' data-testid='app-entity' />
        </Scene>
      </Storyboard>
)`,
      null,
    ),
  })
}

const mappingFunctionAppJS: string = `
    import * as React from 'react'
    export function App({objProp, imgProp, unusedProp}) {
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
        {[1, 2].map((value, index) => {
          return (
            <img
              src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
              alt='Utopia logo'
              style={{ width: 118, height: 150 }}
            />
          )
        })}
        {[{ thing: 1 }, { thing: 2 }, { thing: 3 }].map(
          (someValue, index) => {
            return (
              <div
              >
                <img
                  src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
                  alt='Utopia logo'
                  style={{ width: 118, height: 150 }}
                />
              </div>
            )
          },
        )}
      </div>
      )
    }
    `

function makeMappingFunctionTestProjectContents(): ProjectContentTreeRoot {
  return contentsToTree({
    ['/package.json']: codeFile(
      `
{
  "name": "Utopia Project",
  "version": "0.1.0",
  "utopia": {
    "main-ui": "utopia/storyboard.js",
    "html": "public/index.html",
    "js": "src/index.js"
  },
  "dependencies": {
    "react": "16.13.1",
    "react-dom": "16.13.1",
    "utopia-api": "0.4.1",
    "non-existant-dummy-library": "8.0.27",
    "@heroicons/react": "1.0.1",
    "@emotion/react": "11.9.3"
  }
}`,
      null,
    ),
    ['/src/app.js']: codeFile(mappingFunctionAppJS, null),
    ['/utopia/storyboard.js']: codeFile(
      `
    import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'
    import { App } from '/src/app.js'

    export var storyboard = (
      <Storyboard data-uid='utopia-storyboard-uid' data-testid='utopia-storyboard-uid'>
        <Scene
          style={{
            width: 744,
            height: 1133,
            display: 'flex',
            flexDirection: 'column',
            left: -52,
            top: 9,
            position: 'absolute',
          }}
          data-label='Main Scene'
          data-uid='scene-aaa'
          data-testid='scene-aaa'
        >
        <App objProp={{key1: 'key1'}} imgProp='test.png' nonExistentProp='non' data-uid='app-entity' data-testid='app-entity' />
        </Scene>
      </Storyboard>
)`,
      null,
    ),
  })
}
