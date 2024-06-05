import * as EP from '../../../../core/shared/element-path'
import { selectComponentsForTest } from '../../../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../../../canvas/event-helpers.test-utils'
import { renderTestEditorWithCode } from '../../../canvas/ui-jsx.test-utils'
import { DataSelectorInputFieldTestId } from './data-selector-modal'

describe('data selector modal', () => {
  it('can be opened', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:contents')])

    await mouseClickAtPoint(editor.renderedDOM.getByTestId('plus-button-title'), { x: 1, y: 1 })

    expect(editor.renderedDOM.getByText('Apply')).not.toBeNull()
    expect(editor.renderedDOM.getByTestId(DataSelectorInputFieldTestId).innerText).toEqual(
      ['header', '.title'].join('\n'),
    )
  })
})

const project = `import * as React from 'react'
import {
  Scene,
  Storyboard,
  FlexCol,
  FlexRow,
} from 'utopia-api'

const Contents = ({ children, style, title }) => {
  return (
    <FlexCol data-uid='c-root'>
      <h1 data-uid='c-title'>{title}</h1>
      <FlexRow {...style} data-uid='c-row'>
        {children}
      </FlexRow>
    </FlexCol>
  )
}

export var Playground = ({ style }) => {
  const testimonials = [
    { name: 'Bob', review: 'Very OK' },
    { name: 'Bob', review: 'Very OK' },
    { name: 'Bob', review: 'Very OK' },
    { name: 'Bob', review: 'Very OK' },
    { name: 'Bob', review: 'Very OK' },
  ]

  const reviews = {
    length: 12,
    nodes: [
      { user: 'bob', contents: 'super' },
      { user: 'bob', contents: 'super' },
      { user: 'bob', contents: 'super' },
      { user: 'bob', contents: 'super' },
    ],
  }

  const loaded = true

  const header = {
    title: 'Hello',
    subtitle: 'These are the reviews',
  }

  return <Contents data-uid='contents' title={header['title']} />
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      id='playground-scene'
      commentId='playground-scene'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
      data-uid='scene'
    >
      <Playground style={{}} data-uid='pg' />
    </Scene>
  </Storyboard>
)
`
