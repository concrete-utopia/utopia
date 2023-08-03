import {
  TestSceneUID,
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
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
      })
    })

    describe('with overridden count value', () => {
      const testData = [
        {
          arr: [0, 1, 2, 3],
          overrideCount: 0,
        },
        {
          arr: [0, 1, 2, 3],
          overrideCount: 1,
        },
        {
          arr: [0, 1, 2, 3],
          overrideCount: 2,
        },
        {
          arr: [0, 1, 2, 3],
          overrideCount: 3,
        },
        {
          arr: [0, 1, 2, 3],
          overrideCount: 4,
        },
      ]

      testData.forEach(({ arr, overrideCount }) => {
        it(`shows counter for map items with list length ${arr.length}`, async () => {
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
        })
      })
    })
  })
})
