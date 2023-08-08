import {
  TestSceneUID,
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { colorTheme } from '../../uuiui'
import { navigatorEntryToKey } from '../editor/store/editor-state'
import { getMapCounterTestId } from './navigator-item/map-counter'

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
        const counterTestId = getMapCounterTestId(mapPath)
        const counter = await renderResult.renderedDOM.findByTestId(counterTestId)

        expect(counter.textContent).toEqual(arr.length.toString())
        expect(counter.style.backgroundColor).toEqual('var(--utopitheme-dynamicBlue10)')
        expect(counter.style.color).toEqual('')
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
        overrideCount: -1,
        overrideSuccess: false,
        expectedTargets: [
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div',
          'regular-utopia-storyboard-uid/scene-aaa/containing-div/map',
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

        const counterTestId = getMapCounterTestId(mapElement.elementPath)
        const counter = await renderResult.renderedDOM.findByTestId(counterTestId)

        expect(counter.textContent).toEqual(overrideCount.toString())
        expect(counter.style.color).toEqual('var(--utopitheme-brandNeonPink)')
        if (overrideSuccess) {
          // successful override background color
          expect(counter.style.backgroundColor).toEqual('var(--utopitheme-pinkSubdued)')
        } else {
          // failed override shows a diagonal strikethrough implemented with linear-gradient
          expect(counter.style.background.slice(0, 15)).toEqual('linear-gradient')
        }
        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual(expectedTargets)
      })
    })
  })
})
