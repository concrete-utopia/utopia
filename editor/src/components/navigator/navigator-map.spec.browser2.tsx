/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectRegularCounterWithCount"] }] */
import {
  TestSceneUID,
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { navigatorEntryToKey } from '../editor/store/editor-state'
import { getMapCounterTestId } from './navigator-item/map-counter'
import { getNavigatorTargetsFromEditorState } from './navigator-utils'

function getProjectCode<T>(arr: Array<T>, countOverride?: number): string {
  return formatTestProjectCode(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

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
      data-testid='${TestSceneUID}'
    >
      <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='containing-div'
        data-testid='containing-div'
      >
        {
          // @utopia/uid=map
          ${countOverride != null ? `// @utopia/map-count=${countOverride}` : ''}
          ${JSON.stringify(arr)}.map(() => (
            <div
              style={{
                height: 150,
                width: 150,
                position: 'absolute',
                left: 154,
                top: 134,
                backgroundColor: 'lightblue',
              }}
              data-uid='map-child-div'
              data-testid='map-child-div'
            />)
          )}
        <div
          style={{
            height: 150,
            width: 150,
            position: 'absolute',
            left: 300,
            top: 300,
            backgroundColor: 'darkblue',
          }}
          data-uid='sibling-div'
          data-testid='sibling-div'
        />
      </div>
    </Scene>
  </Storyboard>
)
`)
}

function expectRegularCounterWithCount(domElement: HTMLElement, count: number) {
  expect(domElement.textContent).toEqual(count.toString())
}

function expectOverriddenCounterWithCount(
  domElement: HTMLElement,
  count: number,
  overrideSuccess: boolean,
) {
  expect(domElement.textContent).toEqual(count.toString())
  if (overrideSuccess) {
    // successful override background color
    expect(domElement.style.backgroundColor).toEqual('var(--utopitheme-brandNeonPink60)')
  } else {
    // failed override shows a diagonal strikethrough implemented with linear-gradient
    expect(domElement.style.background.slice(0, 15)).toEqual('linear-gradient')
  }
}

describe('maps in the navigator', () => {
  describe('with default count value', () => {
    const testArrays = [[], [0], [0, 1], [0, 1, 2], [0, 1, 2, 3]]
    testArrays.forEach((arr) => {
      it(`shows counter for map items with list length ${arr.length}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          getProjectCode(arr),
          'await-first-dom-report',
        )

        const mapPath = EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/map')
        const counterTestId = getMapCounterTestId(mapPath, 'navigator')
        const counter = await renderResult.renderedDOM.findByTestId(counterTestId)

        expectRegularCounterWithCount(counter, arr.length)
      })
    })
  })

  describe('with overridden count value', () => {
    const testData = [
      {
        arr: [0, 1, 2, 3],
        overrideCount: 0,
        overrideSuccess: true,
        expectedTargets: [
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
        ],
      },
      {
        arr: [0, 1, 2, 3],
        overrideCount: 1,
        overrideSuccess: true,
        expectedTargets: [
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~1',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
        ],
      },
      {
        arr: [0, 1, 2, 3],
        overrideCount: 2,
        overrideSuccess: true,
        expectedTargets: [
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~1',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~2',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
        ],
      },
      {
        arr: [0, 1, 2, 3],
        overrideCount: 3,
        overrideSuccess: true,
        expectedTargets: [
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~1',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~2',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~3',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
        ],
      },
      {
        arr: [0, 1, 2, 3],
        overrideCount: 4,
        overrideSuccess: true,
        expectedTargets: [
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~1',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~2',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~3',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~4',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
        ],
      },
      {
        arr: [0, 1, 2, 3],
        overrideCount: 5,
        overrideSuccess: false,
        expectedTargets: [
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~1',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~2',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~3',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~4',
          'error-utopia-storyboard-uid/scene-aaa/containing-div/map/invalid-override-5',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
        ],
      },
      {
        arr: [0, 1, 2, 3],
        overrideCount: 6,
        overrideSuccess: false,
        expectedTargets: [
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~1',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~2',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~3',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map/map-child-div~~~4',
          'error-utopia-storyboard-uid/scene-aaa/containing-div/map/invalid-override-5',
          'error-utopia-storyboard-uid/scene-aaa/containing-div/map/invalid-override-6',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/sibling-div',
        ],
      },
    ]

    testData.forEach(({ arr, overrideCount, overrideSuccess, expectedTargets }) => {
      it(`shows overridden counter ${overrideCount} for map items with list length ${arr.length}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          getProjectCode(arr, overrideCount),
          'await-first-dom-report',
        )

        const mapParent = EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/')
        const mapElement = MetadataUtils.getChildrenOrdered(
          renderResult.getEditorState().editor.jsxMetadata,
          renderResult.getEditorState().editor.elementPathTree,
          mapParent,
        )[0]

        const counterTestId = getMapCounterTestId(mapElement.elementPath, 'navigator')
        const counter = await renderResult.renderedDOM.findByTestId(counterTestId)

        expectOverriddenCounterWithCount(counter, overrideCount, overrideSuccess)
        expect(
          getNavigatorTargetsFromEditorState(
            renderResult.getEditorState().editor,
          ).navigatorTargets.map(navigatorEntryToKey),
        ).toEqual(expectedTargets)
      })
    })
  })

  describe('clicking the counter', () => {
    it('Cycles through 2, 1, 0, off if the output length is more than 2', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode([1, 2, 3]),
        'await-first-dom-report',
      )

      const mapPath = EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/map')
      const counterTestId = getMapCounterTestId(mapPath, 'navigator')
      let counter = await renderResult.renderedDOM.findByTestId(counterTestId)
      const counterBounds = counter.getBoundingClientRect()
      const counterCentre = {
        x: counterBounds.x + counterBounds.width / 2,
        y: counterBounds.y + counterBounds.height / 2,
      }

      expectRegularCounterWithCount(counter, 3)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 2, true)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 1, true)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 0, true)

      await mouseClickAtPoint(counter, counterCentre)

      // The counter will have been remounted
      counter = await renderResult.renderedDOM.findByTestId(counterTestId)
      expectRegularCounterWithCount(counter, 3)
    })

    it('Cycles through 2, 1, 0, off if the output length is 2', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode([1, 2]),
        'await-first-dom-report',
      )

      const mapPath = EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/map')
      const counterTestId = getMapCounterTestId(mapPath, 'navigator')
      let counter = await renderResult.renderedDOM.findByTestId(counterTestId)
      const counterBounds = counter.getBoundingClientRect()
      const counterCentre = {
        x: counterBounds.x + counterBounds.width / 2,
        y: counterBounds.y + counterBounds.height / 2,
      }

      expectRegularCounterWithCount(counter, 2)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 2, true)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 1, true)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 0, true)

      await mouseClickAtPoint(counter, counterCentre)

      // The counter will have been remounted
      counter = await renderResult.renderedDOM.findByTestId(counterTestId)
      expectRegularCounterWithCount(counter, 2)
    })

    it('Cycles through 1, 0, off if the output length is 1', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode([1]),
        'await-first-dom-report',
      )

      const mapPath = EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/map')
      const counterTestId = getMapCounterTestId(mapPath, 'navigator')
      let counter = await renderResult.renderedDOM.findByTestId(counterTestId)
      const counterBounds = counter.getBoundingClientRect()
      const counterCentre = {
        x: counterBounds.x + counterBounds.width / 2,
        y: counterBounds.y + counterBounds.height / 2,
      }

      expectRegularCounterWithCount(counter, 1)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 1, true)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 0, true)

      await mouseClickAtPoint(counter, counterCentre)

      // The counter will have been remounted
      counter = await renderResult.renderedDOM.findByTestId(counterTestId)
      expectRegularCounterWithCount(counter, 1)
    })
    it('Cycles through 0, off if the output length is 0', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode([]),
        'await-first-dom-report',
      )

      const mapPath = EP.fromString('utopia-storyboard-uid/scene-aaa/containing-div/map')
      const counterTestId = getMapCounterTestId(mapPath, 'navigator')
      let counter = await renderResult.renderedDOM.findByTestId(counterTestId)
      const counterBounds = counter.getBoundingClientRect()
      const counterCentre = {
        x: counterBounds.x + counterBounds.width / 2,
        y: counterBounds.y + counterBounds.height / 2,
      }

      expectRegularCounterWithCount(counter, 0)

      await mouseClickAtPoint(counter, counterCentre)
      expectOverriddenCounterWithCount(counter, 0, true)

      await mouseClickAtPoint(counter, counterCentre)

      // The counter will have been remounted
      counter = await renderResult.renderedDOM.findByTestId(counterTestId)
      expectRegularCounterWithCount(counter, 0)
    })
  })
})
