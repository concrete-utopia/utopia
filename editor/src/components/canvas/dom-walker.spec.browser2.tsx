/* eslint-disable jest/expect-expect */
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { objectMap } from '../../core/shared/object-utils'
import { renderTestEditorWithCode, TestAppUID, TestSceneUID } from './ui-jsx.test-utils'
import { disableStoredStateforTests } from '../editor/stored-state'
import * as EP from '../../core/shared/element-path'
import { selectComponents } from '../editor/actions/meta-actions'
import {
  canvasRectangle,
  infinityCanvasRectangle,
  infinityLocalRectangle,
  localRectangle,
  MaybeInfinityCanvasRectangle,
  MaybeInfinityLocalRectangle,
  zeroRectIfNullOrInfinity,
} from '../../core/shared/math-utils'
import { MapLike } from 'typescript'
import { duplicateSelected } from '../editor/actions/action-creators'

disableStoredStateforTests()

describe('DOM Walker tests', () => {
  it('Test Project metadata contains entry for all elements', async () => {
    const renderResult = await renderTestEditorWithCode(TestProject, 'await-first-dom-report')
    const metadata = renderResult.getEditorState().editor.jsxMetadata

    const expectedKeys = [
      BakedInStoryboardUID,
      `${BakedInStoryboardUID}/${TestSceneUID}`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/488`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/bbb~~~1`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/bbb~~~2`,
    ]
    expect(Object.keys(metadata)).toEqual(expectedKeys)
  })

  it('Test Project metadata contains global frames for all elements', async () => {
    const renderResult = await renderTestEditorWithCode(TestProject, 'await-first-dom-report')
    const metadata = renderResult.getEditorState().editor.jsxMetadata

    const expectedGlobalFrames: MapLike<MaybeInfinityCanvasRectangle> = {
      [BakedInStoryboardUID]: infinityCanvasRectangle,
      [`${BakedInStoryboardUID}/${TestSceneUID}`]: canvasRectangle({
        x: 0,
        y: 0,
        width: 375,
        height: 812,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}`]: canvasRectangle({
        x: 0,
        y: 0,
        width: 375,
        height: 812,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c`]: canvasRectangle({
        x: 0,
        y: 0,
        width: 375,
        height: 812,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0`]: canvasRectangle({
        x: 55,
        y: 98,
        width: 266,
        height: 124,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/488`]: canvasRectangle({
        x: 126,
        y: 125,
        width: 125,
        height: 70,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/bbb~~~1`]: canvasRectangle({
        x: 55,
        y: 98,
        width: 266,
        height: 0,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/bbb~~~2`]: canvasRectangle({
        x: 55,
        y: 98,
        width: 266,
        height: 0,
      }),
    }

    const resultGlobalFrames = objectMap((element) => element.globalFrame, metadata)
    expect(resultGlobalFrames).toEqual(expectedGlobalFrames)
  })

  it('Test Project metadata contains local frames for all elements', async () => {
    const renderResult = await renderTestEditorWithCode(TestProject, 'await-first-dom-report')
    const metadata = renderResult.getEditorState().editor.jsxMetadata

    const expectedLocalFrames: MapLike<MaybeInfinityLocalRectangle> = {
      [BakedInStoryboardUID]: infinityLocalRectangle,
      [`${BakedInStoryboardUID}/${TestSceneUID}`]: localRectangle({
        x: 0,
        y: 0,
        width: 375,
        height: 812,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}`]: localRectangle({
        x: 0,
        y: 0,
        width: 375,
        height: 812,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c`]: localRectangle({
        x: 0,
        y: 0,
        width: 375,
        height: 812,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0`]: localRectangle({
        x: 55,
        y: 98,
        width: 266,
        height: 124,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/488`]: localRectangle({
        x: 71,
        y: 27,
        width: 125,
        height: 70,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/bbb~~~1`]: localRectangle({
        x: 0,
        y: 0,
        width: 266,
        height: 0,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/bbb~~~2`]: localRectangle({
        x: 0,
        y: 0,
        width: 266,
        height: 0,
      }),
    }

    const resultLocalFrames = objectMap((element) => element.localFrame, metadata)
    expect(resultLocalFrames).toEqual(expectedLocalFrames)
  })

  it('Test Project metadata contains computedStyle only for selected view', async () => {
    const renderResult = await renderTestEditorWithCode(TestProject, 'await-first-dom-report')

    const target = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0`
    await renderResult.dispatch(selectComponents([EP.fromString(target)], false), true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const metadata = renderResult.getEditorState().editor.jsxMetadata

    Object.keys(metadata).forEach((pathAsString) => {
      const computedStyle = metadata[pathAsString].computedStyle
      const computedStyleIsEmpty = computedStyle == null || Object.keys(computedStyle).length === 0
      if (pathAsString === target) {
        expect(computedStyleIsEmpty).toEqual(false)
      } else {
        expect(computedStyleIsEmpty).toEqual(true)
      }
    })
  })

  it('Handles path invalidation when no scene is present', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectWithoutScene,
      'await-first-dom-report',
    )
    const metadataBeforeUpdate = renderResult.getEditorState().editor.jsxMetadata
    const globalFramesBeforeUpdate = objectMap(
      (element) => zeroRectIfNullOrInfinity(element.globalFrame),
      metadataBeforeUpdate,
    )

    const target = `${BakedInStoryboardUID}/flex-container/aaa`
    await renderResult.dispatch(selectComponents([EP.fromString(target)], false), true)
    await renderResult.dispatch([duplicateSelected()], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const metadataAfterUpdate = renderResult.getEditorState().editor.jsxMetadata
    const globalFramesAfterUpdate = objectMap(
      (element) => zeroRectIfNullOrInfinity(element.globalFrame),
      metadataAfterUpdate,
    )

    // Duplicating the element should have caused the rendered frames of the previously existing elements to shrink

    expect(
      globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/aaa`].width,
    ).toBeLessThan(globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/aaa`].width)
    expect(
      globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/bbb`].width,
    ).toBeLessThan(globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/bbb`].width)
    expect(
      globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/ccc`].width,
    ).toBeLessThan(globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/ccc`].width)
  })
})

describe('Capturing closest offset parent', () => {
  it('offset comes from inside the parent component', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import { Scene, Storyboard } from 'utopia-api'
      
      export var App = (props) => {
        return (
          <div data-uid='app-root'>
            <div
              data-uid='inner-div'
              style={{ position: 'absolute', top: 100 }}
            >
              <div data-uid='immediate-parent'>
                {props.children}
              </div>
            </div>
          </div>
        )
      }
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            data-uid='scene'
            style={{
              position: 'absolute',
              width: 375,
              height: 812,
            }}
          >
            <App data-uid='app'>
              <div
                data-uid='child'
                style={{
                  position: 'absolute',
                  width: 200,
                  height: 200,
                  backgroundColor: '#d3d3d3',
                }}
              />
            </App>
          </Scene>
        </Storyboard>
      )
      `,
      'await-first-dom-report',
    )

    const metadataOfInnerElement =
      renderResult.getEditorState().editor.jsxMetadata['sb/scene/app/child']
    const expectedClosestOffsetParent = 'sb/scene/app:app-root/inner-div'
    expect(
      EP.toString(metadataOfInnerElement.specialSizeMeasurements.closestOffsetParentPath),
    ).toBe(expectedClosestOffsetParent)
  })
})

const TestProject = `
import * as React from 'react'
import {
  View,
  Scene,
  Storyboard,
} from 'utopia-api'
export var App = (props) => {
  return (
    <View style={{ ...props.style, backgroundColor: '#FFFFFF'}} data-uid={'05c'}>
      <View
        style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 55, top: 98, width: 266, height: 124  }}
        data-uid={'ef0'}
      >
        <View
          style={{ backgroundColor: '#DDDDDD', position: 'absolute', left: 71, top: 27, width: 125, height: 70 }}
          data-uid={'488'}
        />
        {[1, 2].map(n => {
          return <div data-uid={'bbb'} />
        })}
      </View>
    </View>
  )
}
export var storyboard = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
        data-uid={'${TestSceneUID}'}
      >
        <App
          data-uid='${TestAppUID}' 
          style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
        />
      </Scene>
    </Storyboard>
  )
}
`

const TestProjectWithoutScene = `
import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 10,
          top: 10,
          width: 400,
          height: 100,
          display: 'flex',
        }}
        data-uid='flex-container'
      >
        <div
          style={{
            backgroundColor: '#00FFFB33',
            flexGrow: 1,
            height: 100,
            contain: 'layout',
          }}
          data-uid='aaa'
        />
        <div
          style={{
            backgroundColor: '#B300FF33',
            flexGrow: 1,
            height: 100,
            contain: 'layout',
          }}
          data-uid='bbb'
        />
        <div
          style={{
            backgroundColor: '#55FF0033',
            flexGrow: 1,
            height: 100,
            contain: 'layout',
          }}
          data-uid='ccc'
        />
      </div>
    </Storyboard>
  )
}
`
