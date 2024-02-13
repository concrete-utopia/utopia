import { within } from '@testing-library/react'
import * as EP from '../../../../core/shared/element-path'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import { mouseClickAtPoint, pressKey } from '../../../canvas/event-helpers.test-utils'
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

    const options = [
      ...editor.renderedDOM.baseElement.querySelectorAll(`[data-testid^="variable-from-scope"]`),
    ].map((node) => node.firstChild!.firstChild!.textContent)

    // the items from the data picker are expected here, so that the numbers in `VariableFromScopeOptionTestId`
    // below are put in context
    expect(options).toEqual([
      'style',
      "style['width']",
      "style['height']",
      "style['position']",
      "style['left']",
      "style['top']",
      "style['backgroundColor']",
      "style['display']",
      "style['alignItems']",
      "style['justifyContent']",
      'titleToo',
      'alternateTitle',
      'titles',
      "titles['one']",
      "titles['also JS']",
      'titleIdeas',
      'titleIdeas[0]',
    ])

    // choose a string-valued variable
    let currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId(10))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Title too')).not.toBeNull()
    expect(within(theInspector).queryByText('Title too')).not.toBeNull()

    // choose another string-valued variable
    currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId(11))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Alternate title')).not.toBeNull()
    expect(within(theInspector).queryByText('Alternate title')).not.toBeNull()

    // choose an object prop
    currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId(13))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('The First Title')).not.toBeNull()
    expect(within(theInspector).queryByText('The First Title')).not.toBeNull()

    // choose an object prop
    currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId(14))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Sweet')).not.toBeNull()
    expect(within(theInspector).queryByText('Sweet')).not.toBeNull()

    // choose an array element
    currentOption = editor.renderedDOM.getByTestId(VariableFromScopeOptionTestId(16))
    await mouseClickAtPoint(currentOption, { x: 2, y: 2 })
    expect(within(theScene).queryByText('Chapter One')).not.toBeNull()
    expect(within(theInspector).queryByText('Chapter One')).not.toBeNull()
  })
})

describe('Controls from registering components', () => {
  it('registering internal component', async () => {
    const editor = await renderTestEditorWithCode(
      registerInternalComponentProject,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/title')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(
      `text-string-input-property-control`,
    )
    dataPickerOpenerButton.focus()
    document.execCommand('insertText', false, 'New title')
    await pressKey('Enter', { targetElement: dataPickerOpenerButton })

    const theScene = editor.renderedDOM.getByTestId('scene')
    expect(within(theScene).queryByText('New title')).not.toBeNull()
  })

  it('registering external component', async () => {
    const editor = await renderTestEditorWithCode(
      registerExternalComponentProject,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/scene/pg:root/title')])

    const dataPickerOpenerButton = editor.renderedDOM.getByTestId(
      `sampleprop-string-input-property-control`,
    )
    dataPickerOpenerButton.focus()
    document.execCommand('insertText', false, 'New props value')
    await pressKey('Enter', { targetElement: dataPickerOpenerButton })

    const theView = editor.renderedDOM.getByTestId('view')
    expect(theView.outerHTML).toContain('sampleprop="New props value"')
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

  const titles = {
    one: "The First Title",
    ['also JS']: 'Sweet',
  }

  const titleIdeas = [
    'Chapter One',
  ]

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

const registerInternalComponentProject = `import * as React from 'react'
import {
  Storyboard,
  Scene,
  registerInternalComponent,
} from 'utopia-api'

function Title({ text }) {
  return <h2 data-uid='0cd'>{text}</h2>
}

var Playground = ({ style }) => {
  return (
    <div style={style} data-uid='root'>
      <Title text='Hello Utopia' data-uid='title' />
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
        title='Hello Utopia'
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)

registerInternalComponent(Title, {
  supportsChildren: false,
  properties: {
    text: {
      control: 'string-input',
    },
  },
  variants: [
    {
      code: '<Title />',
    },
  ],
})

`

const registerExternalComponentProject = `import * as React from 'react'
import {
  Storyboard,
  Scene,
  View,
  registerExternalComponent,
} from 'utopia-api'

var Playground = ({ style }) => {
  return (
    <div style={style} data-uid='root' data-testid='view'>
      <View sampleprop='Hello Utopia' data-uid='title'>
        Hello Utopia
      </View>
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
        title='Hello Utopia'
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)

registerExternalComponent(
  View,
  'utopia-api',
  {
    supportsChildren: false,
    properties: {
      sampleprop: {
        control: 'string-input',
      },
    },
    variants: [
      {
        code: '<View />',
      },
    ],
  },
)`
