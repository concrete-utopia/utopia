import { renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import * as EP from '../../core/shared/element-path'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

function getProjectTestCode(arrayLength: number, countOverride: number | string | null) {
  return `import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  export var storyboard = () => {
    const arr = new Array(${arrayLength}).fill(1)
    return (
      <Storyboard data-uid='sb'>
        <div data-uid='group'>
          {
            // @utopia/uid=map
            ${countOverride != null ? `// @utopia/map-count=${countOverride}` : ''}
            arr.map((i) => (
              <div>
                <div>inner div</div>
              </div>
            ))
          }
        </div>
      </Storyboard>
    )
  }
`
}

describe('map count override', () => {
  const testCases = [
    {
      length: 4,
      countOverride: null,
      expectedCount: 4,
    },
    {
      length: 4,
      countOverride: 4,
      expectedCount: 4,
    },
    {
      length: 4,
      countOverride: 3,
      expectedCount: 3,
    },
    {
      length: 4,
      countOverride: 2,
      expectedCount: 2,
    },
    {
      length: 4,
      countOverride: 1,
      expectedCount: 1,
    },
    {
      length: 4,
      countOverride: 0,
      expectedCount: 0,
    },
    {
      length: 4,
      countOverride: 'foo',
      expectedCount: 4,
    },
    {
      length: 4,
      countOverride: 5,
      expectedCount: 4,
    },
    {
      length: 4,
      countOverride: NaN,
      expectedCount: 4,
    },
    {
      length: 4,
      countOverride: -1,
      expectedCount: 4,
    },
  ]

  testCases.forEach(({ length, countOverride, expectedCount }) =>
    it(`overriding original count ${length} with ${countOverride} results in count ${expectedCount}`, async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectTestCode(length, countOverride),
        'await-first-dom-report',
      )

      const mapPath = EP.fromString('sb/group/map')
      const editorState = renderResult.getEditorState().editor

      const mapChildren = MetadataUtils.getChildrenOrdered(
        editorState.jsxMetadata,
        editorState.elementPathTree,
        mapPath,
      )

      expect(mapChildren).toHaveLength(expectedCount)
    }),
  )
})
