import * as EP from '../../../../core/shared/element-path'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { wait } from '../../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../../canvas/controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  pressKey,
} from '../../../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'
import { InsertMode, TextEditMode } from '../../../editor/editor-modes'

describe('Text edit mode', () => {
  describe('Entering text edit mode', () => {
    it('Enters insert mode without selected element', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('insert')
      expect((editor.getEditorState().editor.mode as InsertMode).subjects.length).toBeGreaterThan(0)
    })
    it('Entering insert even if editable element is selected', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      await selectElement(editor, EP.fromString('sb/39e'))
      pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('insert')
      expect((editor.getEditorState().editor.mode as InsertMode).subjects.length).toBeGreaterThan(0)
    })
    it('Entering text edit mode with double click on selected text editable element', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      await selectElement(editor, EP.fromString('sb/39e'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
    })
    it('Entering text edit mode with pressing enter on a text editable selected element', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      await selectElement(editor, EP.fromString('sb/39e'))
      pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
    })
  })

  describe('Click to choose target text for editing', () => {
    it('Click to select text editable target', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      pressKey('t')
      await clickOnElement(editor, 'div')

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
    })
  })
})

async function selectElement(editor: EditorRenderResult, path: ElementPath) {
  await editor.dispatch([selectComponents([path], false)], true)
}

async function clickOnElement(
  editor: EditorRenderResult,
  testId: string,
  singleOrDoubleClick: 'single-click' | 'double-click' = 'single-click',
) {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId(testId)
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  if (singleOrDoubleClick === 'single-click') {
    mouseClickAtPoint(canvasControlsLayer, divCorner)
  } else {
    mouseDoubleClickAtPoint(canvasControlsLayer, divCorner)
  }
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
