import { setFeatureEnabled } from '../../../../utils/feature-switches'
import { CanvasControlsContainerID } from '../../../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from '../../../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../../../canvas/ui-jsx.test-utils'
import { TextEditMode } from '../../../editor/editor-modes'
import * as EP from '../../../../core/shared/element-path'

describe('Entering text edit mode', () => {
  before(() => {
    setFeatureEnabled('Text editing', true)
  })
  it('Entering text edit mode without selected element', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    pressKey('t')
    await editor.getDispatchFollowUpActionsFinished()

    expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
    expect((editor.getEditorState().editor.mode as TextEditMode).editedText).toBeNull()
  })
  it('Entering text edit mode with text editable selected element', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
    await selectElement(editor, 'div')
    pressKey('t')
    await editor.getDispatchFollowUpActionsFinished()

    expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
    expect(EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!)).toEqual(
      'sb/39e',
    )
  })
  it('Entering text edit mode with non-text editable selected element', async () => {
    const editor = await renderTestEditorWithCode(projectWithNestedDiv, 'await-first-dom-report')

    await selectElement(editor, 'div')
    pressKey('t')
    await editor.getDispatchFollowUpActionsFinished()

    expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
    expect((editor.getEditorState().editor.mode as TextEditMode).editedText).toBeNull()
  })
})

describe('Click to choose target text for editing', () => {
  before(() => {
    setFeatureEnabled('Text editing', true)
  })
  it('Click to select text editable target', async () => {
    const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

    pressKey('t')
    await selectElement(editor, 'div')

    expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
    expect(EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!)).toEqual(
      'sb/39e',
    )
  })
  it('Click to select on non-text editable target doesnt work', async () => {
    const editor = await renderTestEditorWithCode(projectWithNestedDiv, 'await-first-dom-report')

    pressKey('t')
    await selectElement(editor, 'div')

    expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
    expect((editor.getEditorState().editor.mode as TextEditMode).editedText).toBeNull()
  })
})

async function selectElement(editor: EditorRenderResult, testId: string) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId(testId)
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  mouseClickAtPoint(canvasControlsLayer, divCorner)
  await editor.getDispatchFollowUpActionsFinished()
}

const projectWithText = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-testid='div'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 288,
        height: 362,
      }}
      data-uid='39e'
    >
      Hello
    </div>
  </Storyboard>
)
`)

const projectWithNestedDiv = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-testid='div'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 288,
        height: 362,
      }}
      data-uid='39e'
    >
      <div/>
    </div>
  </Storyboard>
)
`)
