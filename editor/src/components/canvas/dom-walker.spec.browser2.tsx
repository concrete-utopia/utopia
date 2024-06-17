/* eslint-disable jest/expect-expect */
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { objectMap } from '../../core/shared/object-utils'
import { renderTestEditorWithCode, TestAppUID, TestSceneUID } from './ui-jsx.test-utils'
import { disableStoredStateforTests } from '../editor/stored-state'
import * as EP from '../../core/shared/element-path'
import * as PP from '../../core/shared/property-path'
import { selectComponents } from '../editor/actions/meta-actions'
import type {
  MaybeInfinityCanvasRectangle,
  MaybeInfinityLocalRectangle,
} from '../../core/shared/math-utils'
import {
  canvasPoint,
  canvasRectangle,
  infinityCanvasRectangle,
  infinityLocalRectangle,
  localRectangle,
  offsetPoint,
  zeroRectIfNullOrInfinity,
} from '../../core/shared/math-utils'
import type { MapLike } from 'typescript'
import {
  clearSelection,
  duplicateSelected,
  setProp_UNSAFE,
  switchEditorMode,
} from '../editor/actions/action-creators'
import { emptyComments, jsExpressionValue } from '../../core/shared/element-template'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { slightlyOffsetPointBecauseVeryWeirdIssue, wait } from '../../utils/utils.test-utils'
import { mouseClickAtPoint, mouseDownAtPoint, mouseMoveToPoint } from './event-helpers.test-utils'
import { EditorModes } from '../editor/editor-modes'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

disableStoredStateforTests()

describe('DOM Walker', () => {
  it('Test Project metadata contains entry for all elements', async () => {
    const renderResult = await renderTestEditorWithCode(TestProject, 'await-first-dom-report')
    const metadata = renderResult.getEditorState().editor.jsxMetadata

    const expectedKeys = [
      BakedInStoryboardUID,
      `${BakedInStoryboardUID}/${TestSceneUID}`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/488`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63/bbb~~~1`,
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63/bbb~~~2`,
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
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63`]: canvasRectangle({
        x: 55,
        y: 98,
        width: 266,
        height: 0,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63/bbb~~~1`]:
        canvasRectangle({
          x: 55,
          y: 98,
          width: 266,
          height: 0,
        }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63/bbb~~~2`]:
        canvasRectangle({
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
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63`]: localRectangle({
        x: 0,
        y: 0,
        width: 266,
        height: 0,
      }),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63/bbb~~~1`]: localRectangle(
        {
          x: 0,
          y: 0,
          width: 266,
          height: 0,
        },
      ),
      [`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:05c/ef0/d63/bbb~~~2`]: localRectangle(
        {
          x: 0,
          y: 0,
          width: 266,
          height: 0,
        },
      ),
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

    const target = `${BakedInStoryboardUID}/flex-container/child-1`
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
      globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/child-1`].width,
    ).toBeLessThan(globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/child-1`].width)
    expect(
      globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/bbb`].width,
    ).toBeLessThan(globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/bbb`].width)
    expect(
      globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/ccc`].width,
    ).toBeLessThan(globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/ccc`].width)
  })

  it('Handles path invalidation caused by the observers when no scene is present', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectWithoutScene,
      'await-first-dom-report',
    )
    const metadataBeforeUpdate = renderResult.getEditorState().editor.jsxMetadata
    const globalFramesBeforeUpdate = objectMap(
      (element) => zeroRectIfNullOrInfinity(element.globalFrame),
      metadataBeforeUpdate,
    )

    const targetString = `${BakedInStoryboardUID}/flex-container`
    const targetElement = EP.fromString(targetString)
    await renderResult.dispatch(selectComponents([targetElement], false), true)
    await renderResult.dispatch(
      [
        setProp_UNSAFE(
          targetElement,
          PP.create('style', 'left'),
          jsExpressionValue(5, emptyComments),
        ),
      ],
      true,
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    const metadataAfterUpdate = renderResult.getEditorState().editor.jsxMetadata
    const globalFramesAfterUpdate = objectMap(
      (element) => zeroRectIfNullOrInfinity(element.globalFrame),
      metadataAfterUpdate,
    )

    // Adjusting the left value of the parent should have shifted all of the children to the left

    expect(
      globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/child-1`].x,
    ).toBeLessThan(globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/child-1`].x)
    expect(globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/bbb`].x).toBeLessThan(
      globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/bbb`].x,
    )
    expect(globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/ccc`].x).toBeLessThan(
      globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/ccc`].x,
    )
  })

  it('Selective dom walking includes children', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectWithoutScene,
      'await-first-dom-report',
    )
    const metadataBeforeUpdate = renderResult.getEditorState().editor.jsxMetadata
    const globalFramesBeforeUpdate = objectMap(
      (element) => zeroRectIfNullOrInfinity(element.globalFrame),
      metadataBeforeUpdate,
    )

    const targetString = `${BakedInStoryboardUID}/flex-container`
    const targetElement = EP.fromString(targetString)
    await renderResult.dispatch(selectComponents([targetElement], false), true)

    // Drag to resize, which will trigger the selective dom walker on the target only during the drag
    const resizeControl = renderResult.renderedDOM.getByTestId(`resize-control-0-0.5`)
    const resizeControlBounds = resizeControl.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const startPoint = canvasPoint(
      slightlyOffsetPointBecauseVeryWeirdIssue({
        x: resizeControlBounds.x + resizeControlBounds.width / 2,
        y: resizeControlBounds.y + resizeControlBounds.height / 2,
      }),
    )

    const endPoint = offsetPoint(startPoint, canvasPoint({ x: -10, y: 0 }))

    await mouseMoveToPoint(resizeControl, startPoint)
    await mouseDownAtPoint(resizeControl, startPoint)
    await mouseMoveToPoint(canvasControlsLayer, endPoint, { eventOptions: { buttons: 1 } })

    await renderResult.getDispatchFollowUpActionsFinished()

    const metadataAfterUpdate = renderResult.getEditorState().editor.jsxMetadata
    const globalFramesAfterUpdate = objectMap(
      (element) => zeroRectIfNullOrInfinity(element.globalFrame),
      metadataAfterUpdate,
    )

    // Adjusting the left value of the parent should have shifted all of the children to the left

    expect(
      globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/child-1`].x,
    ).toBeLessThan(globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/child-1`].x)
    expect(globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/bbb`].x).toBeLessThan(
      globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/bbb`].x,
    )
    expect(globalFramesAfterUpdate[`${BakedInStoryboardUID}/flex-container/ccc`].x).toBeLessThan(
      globalFramesBeforeUpdate[`${BakedInStoryboardUID}/flex-container/ccc`].x,
    )
  })

  it('clears all of the invalidated paths including svgs', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectWithSVG,
      'await-first-dom-report',
    )
    await renderResult.getDispatchFollowUpActionsFinished()
    // This is here to ensure that the DOM walker runs and clears the
    // entries that are added in by the resize observer, which triggers at
    // an awkward timing after the entirety of `renderTestEditorWithCode` executes.
    // That results in an entry for the scene being added, which then would cause the
    // test to fail.
    await renderResult.dispatch([clearSelection()], true)
    const domWalkerState = renderResult.getDomWalkerState()
    expect(Array.from(domWalkerState.invalidatedPaths)).toHaveLength(0)
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

describe('Observing runtime changes', () => {
  const changingProjectCode = `
    import * as React from 'react'
    import { Scene, Storyboard } from 'utopia-api'

    const App = (props) => {
      const [showDiv, setShowDiv] = React.useState(false)

      return (
        <div
          style={{ width: '100%', height: '100%' }}
          data-uid='app-root'
        >
          <div
            style={{
              backgroundColor: 'lightblue',
              position: 'absolute',
              left: 49,
              top: 59,
              width: 200,
              height: 44,
              textAlign: 'center',
            }}
            onMouseUp={() => {
              setTimeout(() => setShowDiv(true), 0) // setTimeout so that this happens after we handle mouse events
            }}
            data-uid='button'
            data-testid='click-me'
          >
            Click me
          </div>
          {
            // @utopia/uid=conditional
            showDiv ? (
            <div
              style={{
                backgroundColor: 'lightblue',
                position: 'absolute',
                left: 49,
                top: 150,
                width: 200,
                height: 44,
                textAlign: 'center',
              }}
              data-uid='target-div'
              data-testid='target-div'
            >
              Hello!
            </div>
          ) : null}
        </div>
      )
    }

    export var storyboard = (
      <Storyboard data-uid='sb'>
        <Scene
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 532,
            top: 66,
            width: 296,
            height: 457,
          }}
          data-uid='sc'
        >
          <App data-uid='app' />
        </Scene>
      </Storyboard>
    )
  `

  it('Updates the metadata in live mode', async () => {
    const renderResult = await renderTestEditorWithCode(
      changingProjectCode,
      'await-first-dom-report',
    )
    await renderResult.dispatch([switchEditorMode(EditorModes.liveMode())], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const targetPath = EP.fromString('sb/sc/app:app-root/conditional/target-div')

    // Check no metadata for the target at the start
    const metadataBefore = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      targetPath,
    )
    expect(metadataBefore).toBeNull()

    const recordedActionsBefore = [...renderResult.getRecordedActions()]

    const buttonToClick = renderResult.renderedDOM.getByTestId('click-me')
    const buttonToClickBounds = buttonToClick.getBoundingClientRect()
    const clickPoint = {
      x: buttonToClickBounds.width / 2,
      y: buttonToClickBounds.height / 2,
    }

    await mouseClickAtPoint(buttonToClick, clickPoint)
    await wait(20)
    await renderResult.getDispatchFollowUpActionsFinished()

    const recordedActionsAfter = [...renderResult.getRecordedActions()]
    const recordedActionsDuring = recordedActionsAfter.slice(recordedActionsBefore.length)

    const runDomWalkerActions = recordedActionsDuring.filter((a) => a.action === 'RUN_DOM_WALKER')
    expect(runDomWalkerActions.length).toEqual(2) // Both the resize observer and mutation observer will fire

    // Check there is metadata for the target at the end
    const metadataAfter = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      targetPath,
    )
    expect(metadataAfter).not.toBeNull()
  })

  xit('Does not update the metadata in select mode', async () => {
    const renderResult = await renderTestEditorWithCode(
      changingProjectCode,
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('sb/sc/app:app-root/conditional/target-div')

    // Check no metadata for the target at the start
    const metadataBefore = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      targetPath,
    )
    expect(metadataBefore).toBeNull()

    const recordedActionsBefore = [...renderResult.getRecordedActions()]

    const buttonToClick = renderResult.renderedDOM.getByTestId('click-me')
    const buttonToClickBounds = buttonToClick.getBoundingClientRect()
    const clickPoint = {
      x: buttonToClickBounds.width / 2,
      y: buttonToClickBounds.height / 2,
    }

    await mouseClickAtPoint(buttonToClick, clickPoint)
    await wait(0)
    await renderResult.getDispatchFollowUpActionsFinished()

    const recordedActionsAfter = [...renderResult.getRecordedActions()]
    const recordedActionsDuring = recordedActionsAfter.slice(recordedActionsBefore.length)

    const runDomWalkerActions = recordedActionsDuring.filter((a) => a.action === 'RUN_DOM_WALKER')
    expect(runDomWalkerActions.length).toEqual(0)

    // Check there is still no metadata for the target at the end
    const metadataAfter = MetadataUtils.findElementByElementPath(
      renderResult.getEditorState().editor.jsxMetadata,
      targetPath,
    )
    expect(metadataAfter).toBeNull()
  })
})

describe('Text content', () => {
  it('Is only captured for leaf elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import { Storyboard } from 'utopia-api'

      export var storyboard = (
        <Storyboard data-uid='sb'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 293,
              top: 176,
              width: 355,
              height: 453,
            }}
            data-uid='parent'
          >
            <div data-uid='text-only'>Text Only</div>
            <div data-uid='text-with-br'>
              Text <br data-uid='br' /> with br
            </div>
            <div data-uid='text-with-span'>
              Text with <span data-uid='span'>span</span>
            </div>
          </div>
        </Storyboard>
      )
      `,
      'await-first-dom-report',
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadata

    const pathsToCheck = [
      { path: 'sb', expectsText: false },
      { path: 'sb/parent', expectsText: false },
      { path: 'sb/parent/text-only', expectsText: true },
      { path: 'sb/parent/text-with-br', expectsText: false },
      { path: 'sb/parent/text-with-br/br', expectsText: false },
      { path: 'sb/parent/text-with-span', expectsText: false },
      { path: 'sb/parent/text-with-span/span', expectsText: true },
    ]

    pathsToCheck.forEach(({ path, expectsText }) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, EP.fromString(path))
      expect(elementMetadata).not.toBeNull()
      const textLength = elementMetadata!.textContent?.length ?? 0
      const hasText = textLength > 0
      expect(hasText).toEqual(expectsText)
    })
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
        {
          // @utopia/uid=d63
          [1, 2].map(n => {
            return <div data-uid={'bbb'} />
          })
        }
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

const TestProjectWithSVG = `
import * as React from 'react'
import {
  View,
  Scene,
  Storyboard,
} from 'utopia-api'

export var SVGComponent = () => {
  return (
    <svg
      width='92'
      height='92'
      viewBox='0 0 72 72'
      fill='none'
      xmlns='http://www.w3.org/2000/svg'
    >
      <path
        fill-rule='evenodd'
        clip-rule='evenodd'
        d='M41 5C41 2.23858 38.7614 0 36 0C33.2386 0 31 2.23858 31 5L31 31L5 31C2.23858 31 0 33.2386 0 36C0 38.7614 2.23858 41 5 41H31L31 67C31 69.7614 33.2386 72 36 72C38.7614 72 41 69.7614 41 67V41H67C69.7614 41 72 38.7614 72 36C72 33.2386 69.7614 31 67 31L41 31V5Z'
        fill='green'
      />
    </svg>
  )
}

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
        <SVGComponent />
        <svg
          width='92'
          height='92'
          viewBox='0 0 72 72'
          fill='none'
          xmlns='http://www.w3.org/2000/svg'
        >
          <path
            fill-rule='evenodd'
            clip-rule='evenodd'
            d='M41 5C41 2.23858 38.7614 0 36 0C33.2386 0 31 2.23858 31 5L31 31L5 31C2.23858 31 0 33.2386 0 36C0 38.7614 2.23858 41 5 41H31L31 67C31 69.7614 33.2386 72 36 72C38.7614 72 41 69.7614 41 67V41H67C69.7614 41 72 38.7614 72 36C72 33.2386 69.7614 31 67 31L41 31V5Z'
            fill='green'
          />
        </svg>
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
          data-uid='child-1'
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
