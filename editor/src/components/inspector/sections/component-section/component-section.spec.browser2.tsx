import { within } from '@testing-library/react'
import * as EP from '../../../../core/shared/element-path'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../../../canvas/event-helpers.test-utils'
import { renderTestEditorWithCode } from '../../../canvas/ui-jsx.test-utils'
import {
  DataPickerPopupButtonTestId,
  DataPickerPopupTestId,
  VariableFromScopeOptionTestId,
} from './component-section'

describe('Set element prop via the data picker', () => {
  it('can pick from the property data picker', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/title')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(DataPickerPopupButtonTestId)
    await mouseClickAtPoint(dataPickerOpenerButton, { x: 2, y: 2 })

    const dataPickerPopup = editor.renderedDOM.queryByTestId(DataPickerPopupTestId)
    expect(dataPickerPopup).not.toBeNull()

    const theScene = editor.renderedDOM.getByTestId('scene')
    const theInspector = editor.renderedDOM.getByTestId('inspector-sections-container')

    const firstOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId(0))
    await mouseClickAtPoint(firstOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Title too')).not.toBeNull()
    expect(within(theInspector).queryByText('Title too')).not.toBeNull()

    const secondOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId(1))
    await mouseClickAtPoint(secondOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Alternate title')).not.toBeNull()
    expect(within(theInspector).queryByText('Alternate title')).not.toBeNull()
  })
})

const project = `import * as React from 'react'
import { Storyboard, Scene } from 'utopia-api'

function Title({ text }) {
  const content = 'Content'

  return <h2 data-uid='0cd'>{text}</h2>
}

var Playground = ({ style }) => {
  const titleToo = 'Title too'

  const alternateTitle = 'Alternate title'

  return (
    <div style={style} data-uid='root'>
      <Title text='The Title' data-uid='title' />
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 521,
        height: 266,
        position: 'absolute',
        left: 554,
        top: 247,
        backgroundColor: 'white',
      }}
      data-uid='scene'
      data-testid='scene'
      commentId='120'
    >
      <Playground
        style={{
          width: 454,
          height: 177,
          position: 'absolute',
          left: 34,
          top: 44,
          backgroundColor: 'white',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)`
