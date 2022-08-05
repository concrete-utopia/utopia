/// <reference types="karma-viewport" />

import { renderTestEditorWithCode } from '../../components/canvas/ui-jsx.test-utils'
import { canvasRectangle, localRectangle } from '../shared/math-utils'

describe('Frame calculation for fragments', () => {
  // Components with root fragments do not appear in the DOM, so the dom walker does not find them, and they
  // do not appear in the dom metadata.
  // To make sure they still have globalFrame and localFrame in the jsxMetadata, we calculate the frames from
  // their descendants which are rendered in the dom (see mergeComponentMetadata function)

  before(() => {
    viewport.set(2200, 1000)
  })

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
