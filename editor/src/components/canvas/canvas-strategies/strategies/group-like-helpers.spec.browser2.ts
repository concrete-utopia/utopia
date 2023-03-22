import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'

const project = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='group'>
      <div
        style={{
          backgroundColor: '#4ab6ff',
          position: 'absolute',
          left: 143,
          top: 146,
          width: 104,
          height: 65,
        }}
        data-uid='c86'
      />
    </div>
  </Storyboard>
)
`

describe('group like helpers', () => {
  it('sizeless div on storyboard is not laid out by flow', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

    const { allElementProps, jsxMetadata } = editor.getEditorState().editor

    const isFlow = MetadataUtils.isPositionedByFlow(
      jsxMetadata,
      allElementProps,
      EP.fromString('sb/group'),
    )

    expect(isFlow).toEqual(false)
  })
})
