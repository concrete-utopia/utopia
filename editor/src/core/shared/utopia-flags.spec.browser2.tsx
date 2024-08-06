import {
  TestAppUID,
  TestSceneUID,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { toggleDataCanCondense } from '../../components/editor/actions/action-creators'
import { BakedInStoryboardUID } from '../model/scene-utils'
import * as EP from './element-path'

const TestProject = makeTestProjectCodeWithSnippet(
  `<div
style={{
  height: '100%',
  width: '100%',
  contain: 'layout',
}}
data-uid='app-root'
>
  <div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 53,
      top: 99,
      width: 261,
      height: 250,
    }}
    data-uid='jsxelem'
  />
  // @utopia/uid=conditional
  {1 == 1 ? (
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 377,
        top: 138,
        width: 247,
        height: 198,
      }}
      data-uid='div'
    />
  ) : null}
 </div>`,
)

const JsxElemPath = EP.elementPath([
  [BakedInStoryboardUID, TestSceneUID, TestAppUID],
  ['app-root', 'jsxelem'],
])

const ConditionalPath = EP.elementPath([
  [BakedInStoryboardUID, TestSceneUID, TestAppUID],
  ['app-root', 'jsxelem'],
])

describe('Utopia prop or comment flags', () => {
  it('Storing and removing can-condense on an element in a prop', async () => {
    const renderResult = await renderTestEditorWithCode(TestProject, 'await-first-dom-report')

    await renderResult.dispatch([toggleDataCanCondense([JsxElemPath])], true)

    // jsxelem contains data-can-condense prop
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 53,
            top: 99,
            width: 261,
            height: 250,
          }}
          data-uid='jsxelem'
          data-can-condense
        />
        // @utopia/uid=conditional
        {1 == 1 ? (
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 377,
              top: 138,
              width: 247,
              height: 198,
            }}
            data-uid='div'
          />
        ) : null}
       </div>`,
      ),
    )

    await renderResult.dispatch([toggleDataCanCondense([JsxElemPath])], true)

    // jsxelem does not contain data-can-condense prop
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 53,
            top: 99,
            width: 261,
            height: 250,
          }}
          data-uid='jsxelem'
        />
        // @utopia/uid=conditional
        {1 == 1 ? (
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 377,
              top: 138,
              width: 247,
              height: 198,
            }}
            data-uid='div'
          />
        ) : null}
       </div>`,
      ),
    )
  })
  it('Storing and removing can-condense on a conditional in a comment', async () => {
    const renderResult = await renderTestEditorWithCode(TestProject, 'await-first-dom-report')

    await renderResult.dispatch([toggleDataCanCondense([ConditionalPath])], true)

    // conditional contains can-condense comment
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 53,
            top: 99,
            width: 261,
            height: 250,
          }}
          data-uid='jsxelem'
          data-can-condense
        />
        // @utopia/uid=conditional
        {1 == 1 ? (
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 377,
              top: 138,
              width: 247,
              height: 198,
            }}
            data-uid='div'
          />
        ) : null}
       </div>`,
      ),
    )

    await renderResult.dispatch([toggleDataCanCondense([JsxElemPath])], true)

    // conditional does not contain can-condense comment
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 53,
            top: 99,
            width: 261,
            height: 250,
          }}
          data-uid='jsxelem'
        />
        // @utopia/uid=conditional
        {1 == 1 ? (
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 377,
              top: 138,
              width: 247,
              height: 198,
            }}
            data-uid='div'
          />
        ) : null}
       </div>`,
      ),
    )
  })
})
