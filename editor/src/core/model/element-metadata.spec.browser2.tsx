/// <reference types="karma-viewport" />

import { renderTestEditorWithCode } from '../../components/canvas/ui-jsx.test-utils'
import { Either, isRight } from '../shared/either'
import { elementPath, toString } from '../shared/element-path'
import { canvasRectangle, localRectangle } from '../shared/math-utils'
import { elementOnlyHasTextChildren } from './element-template-utils'
import { FOR_TESTS_setNextGeneratedUids } from './element-template-utils.test-utils'

describe('Frame calculation for fragments', () => {
  // Components with root fragments do not appear in the DOM, so the dom walker does not find them, and they
  // do not appear in the dom metadata.
  // To make sure they still have globalFrame and localFrame in the jsxMetadata, we calculate the frames from
  // their descendants which are rendered in the dom (see mergeComponentMetadata function)

  it('Frames of component with root fragment is union of its children', async () => {
    const fragmentComponentPath = 'story/scene/app:frag'

    const renderResult = await renderTestEditorWithCode(
      TestProjectWithFragment,
      'await-first-dom-report',
    )

    expect(
      renderResult.getEditorState().editor.jsxMetadata[fragmentComponentPath].localFrame,
    ).toEqual(
      localRectangle({
        x: 64,
        y: 114,
        height: 363,
        width: 210,
      }),
    )
    expect(
      renderResult.getEditorState().editor.jsxMetadata[fragmentComponentPath].globalFrame,
    ).toEqual(
      canvasRectangle({
        x: 101,
        y: 179,
        height: 363,
        width: 210,
      }),
    )
  })
  it('Frames of component with root fragment and map inside is union of its children', async () => {
    const fragmentComponentPath = 'story/scene/app:frag'

    const renderResult = await renderTestEditorWithCode(
      TestProjectWithFragmentAndMap,
      'await-first-dom-report',
    )

    expect(
      renderResult.getEditorState().editor.jsxMetadata[fragmentComponentPath].localFrame,
    ).toEqual(
      localRectangle({
        x: 64,
        y: 114,
        height: 90,
        width: 10,
      }),
    )
    expect(
      renderResult.getEditorState().editor.jsxMetadata[fragmentComponentPath].globalFrame,
    ).toEqual(
      canvasRectangle({
        x: 101,
        y: 179,
        height: 90,
        width: 10,
      }),
    )
  })
  it('Conditionals have metadata and their frame is the frame of the active branch', async () => {
    FOR_TESTS_setNextGeneratedUids(['foo1', 'foo2', 'foo3', 'foo4', 'foo5', 'foo6', 'cond']) // ugly, but the conditional is the 5th uid which is generated

    const condComponentPath = 'story/scene/app:root/cond'
    const trueBranchComponentPath = 'story/scene/app:root/cond/truebranch'

    const renderResult = await renderTestEditorWithCode(
      TestProjectWithConditional,
      'await-first-dom-report',
    )

    expect(renderResult.getEditorState().editor.jsxMetadata[condComponentPath].globalFrame).toEqual(
      renderResult.getEditorState().editor.jsxMetadata[trueBranchComponentPath].globalFrame,
    )
    expect(renderResult.getEditorState().editor.jsxMetadata[condComponentPath].localFrame).toEqual(
      renderResult.getEditorState().editor.jsxMetadata[trueBranchComponentPath].localFrame,
    )
  })
})

function asRight<L, R>(e: Either<L, R>): R {
  if (isRight(e)) {
    return e.value
  }
  throw new Error('found left')
}

describe('elementHasTextOnlyChildren', () => {
  it('element containing only text is considered to have only text children', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    const elem =
      editor.getEditorState().editor.jsxMetadata[toString(elementPath([['stb', 'hello']]))]
    const isText = elementOnlyHasTextChildren(asRight(elem.element))
    expect(isText).toEqual(true)
  })
  it('element containing text and <br /> tags is considered to have only text children', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    const elem =
      editor.getEditorState().editor.jsxMetadata[toString(elementPath([['stb', 'hibr']]))]
    const isText = elementOnlyHasTextChildren(asRight(elem.element))
    expect(isText).toEqual(true)
  })
  it('element containing <br /> tags is considered to have only text children', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    const elem =
      editor.getEditorState().editor.jsxMetadata[toString(elementPath([['stb', 'brbrbr']]))]
    const isText = elementOnlyHasTextChildren(asRight(elem.element))
    expect(isText).toEqual(true)
  })
  it('element containing divs is not considered to have only text children', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    const elem = editor.getEditorState().editor.jsxMetadata[toString(elementPath([['stb']]))]
    const isText = elementOnlyHasTextChildren(asRight(elem.element))
    expect(isText).toEqual(false)
  })
})

const TestProjectWithFragment = `
import * as React from 'react'
import { Scene, Storyboard } from "utopia-api";

export var App = (props) => {
  return (
    <Frag data-uid='frag'>
      <div
        style={{
          left: 64,
          top: 114,
          width: 210,
          height: 199,
          position: 'absolute',
          backgroundColor: '#d3d3d3',
        }}
      >
        <div
          style={{
            left: 33,
            top: 91,
            width: 97,
            height: 94,
            position: 'absolute',
            backgroundColor: '#FFFFFF',
            border: '0px solid rgb(0, 0, 0, 1)',
          }}
        />
      </div>
      <div
        style={{
          left: 95,
          top: 356,
          width: 148,
          height: 121,
          position: 'absolute',
          backgroundColor: '#F76565',
        }}
      >
        <div
          style={{
            left: 35,
            top: 29,
            width: 48,
            height: 48,
          }}
          data-uid='3bb'
        />
      </div>
    </Frag>
  )
}

const Frag = (props) => {
  return <>{props.children}</>
}

export var storyboard = (
  <Storyboard data-uid='story'>
    <Scene
      style={{
        position: 'absolute',
        left: 37,
        top: 65,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
);
`

const TestProjectWithFragmentAndMap = `
import * as React from 'react'
import { Scene, Storyboard } from "utopia-api";

export var App = (props) => {
  const arr = [0, 1, 2, 3, 4]
  return (
    <Frag data-uid='frag'>
      {arr.map((idx) => {
        return (
          <div
            style={{
              left: 64,
              top: 114 + idx * 20,
              width: 10,
              height: 10,
              position: 'absolute',
              backgroundColor: '#d3d3d3',
            }}
          />
        )
      })}
    </Frag>
  )
}

const Frag = (props) => {
  return <>{props.children}</>
}

export var storyboard = (
  <Storyboard data-uid='story'>
    <Scene
      style={{
        position: 'absolute',
        left: 37,
        top: 65,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
);
`

const TestProjectWithConditional = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        left: 64,
        top: 114,
        width: 210,
        height: 199,
        position: 'absolute',
        backgroundColor: '#d3d3d3',
      }}
      data-uid='root'
    >
      {[].length === 0 ? (
        <div
          style={{
            left: 33,
            top: 91,
            width: 97,
            height: 94,
            position: 'absolute',
            backgroundColor: '#FFFFFF',
            border: '0px solid rgb(0, 0, 0, 1)',
          }}
          data-uid='truebranch'
        />
      ) : null}
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='story'>
    <Scene
      style={{
        position: 'absolute',
        left: 37,
        top: 65,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)
`

const projectWithText = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='stb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 108,
        top: 133,
        width: 154,
        height: 96,
      }}
      data-uid='hello'
    >
      Hello
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 412,
        top: 139,
        width: 198,
        height: 99,
      }}
      data-uid='hibr'
    >
      hi here
      <br />
      <br />
      <br />
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 213,
        top: 303,
        width: 270,
        height: 128,
      }}
      data-uid='brbrbr'
    >
      <br />
      <br />
      <br />
      <br />
    </div>
  </Storyboard>
)
`
