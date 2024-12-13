import { FOR_TESTS_setNextGeneratedUid } from '../../../../core/model/element-template-utils.test-utils'
import { wait } from '../../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../../canvas/controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDragFromPointToPoint,
  pressKey,
} from '../../../canvas/event-helpers.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../../../canvas/ui-jsx.test-utils'
import { CanvasContainerID } from '../../canvas-types'

describe('draw-to-insert text', () => {
  describe('draw', () => {
    it('allows drawing to insert some text', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId('div')
      const divBounds = div.getBoundingClientRect()

      await pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const outsideDiv = {
        x: divBounds.x + divBounds.width + 100,
        y: divBounds.y + 100,
      }

      await mouseDragFromPointToPoint(canvasControlsLayer, outsideDiv, {
        x: outsideDiv.x + 100,
        y: outsideDiv.y + 250,
      })
      await editor.getDispatchFollowUpActionsFinished()

      typeText('Utopia')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      const newElementUID = Object.keys(editor.getEditorState().editor.jsxMetadata)
        .find((k) => k.startsWith('sb/') && k !== 'sb/39e')
        ?.replace('sb/', '')

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
            import * as React from 'react'
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
                >Hello</div>
                <span
                  style={{
                    position: 'absolute',
                    wordBreak: 'break-word',
                    left: 389,
                    top: 101,
                    width: 100,
                    height: 250,
                  }}
                  data-uid='${newElementUID}'
                >Utopia</span>
              </Storyboard>
            )`),
      )
    })
  })
  describe('click', () => {
    it('allows clicking to insert some text', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId('div')
      const divBounds = div.getBoundingClientRect()

      await pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const outsideDiv = {
        x: divBounds.x + divBounds.width + 100,
        y: divBounds.y + 100,
      }

      await mouseClickAtPoint(canvasControlsLayer, outsideDiv)
      await editor.getDispatchFollowUpActionsFinished()

      typeText('Utopia')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      const newElementUID = Object.keys(editor.getEditorState().editor.jsxMetadata)
        .find((k) => k.startsWith('sb/') && k !== 'sb/39e')
        ?.replace('sb/', '')

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
            import * as React from 'react'
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
                >Hello</div>
                <span
                  style={{
                    position: 'absolute',
                    wordBreak: 'break-word',
                    left: 389,
                    top: 101,
                    width: 'max-content',
                    height: 'max-content',
                  }}
                  data-uid='${newElementUID}'
                >Utopia</span>
              </Storyboard>
            )`),
      )
    })
  })
  describe('when the target is editable', () => {
    it('inserts new text when dragging to insert', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId('div')
      const divBounds = div.getBoundingClientRect()

      await pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const insideDiv = {
        x: divBounds.x + divBounds.width / 2,
        y: divBounds.y + divBounds.height / 2,
      }

      await mouseDragFromPointToPoint(canvasControlsLayer, insideDiv, {
        x: insideDiv.x + 50,
        y: insideDiv.y + 50,
      })
      await editor.getDispatchFollowUpActionsFinished()

      typeText(' Utopia')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      const newElementUID = Object.keys(editor.getEditorState().editor.jsxMetadata)
        .find((k) => k.startsWith('sb/') && k !== 'sb/39e')
        ?.replace('sb/39e/', '')

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
            import * as React from 'react'
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
                >Hello
                  <span
                    style={{
                      position: 'absolute',
                      wordBreak: 'break-word',
                      left: 145,
                      top: 182,
                      width: 50,
                      height: 50,
                    }}
                    data-uid='${newElementUID}'
                  >
                    {' '}
                    Utopia
                  </span>
                </div>
              </Storyboard>
            )`),
      )
    })
  })
  describe('when the target is not editable', () => {
    it('inserts new text', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithNonTextEditableDiv,
        'await-first-dom-report',
      )

      const newUID = 'ddd'
      FOR_TESTS_setNextGeneratedUid(newUID)

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId('div')
      const divBounds = div.getBoundingClientRect()

      await pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const insideDiv = {
        x: divBounds.x + divBounds.width / 2,
        y: divBounds.y + divBounds.height / 2,
      }

      await mouseDragFromPointToPoint(canvasControlsLayer, insideDiv, {
        x: insideDiv.x + 50,
        y: insideDiv.y + 50,
      })
      await editor.getDispatchFollowUpActionsFinished()

      typeText('Hello Utopia')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
            import * as React from 'react'
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
                  <div data-uid='111' />
                  <span
                    style={{
                      wordBreak: 'break-word',
                      width: 50,
                      height: 50,
                      contain: 'layout',
                    }}
                    data-uid='ddd'
                  >Hello Utopia</span>
                </div>
              </Storyboard>
            )`),
      )
    })
  })
  describe('when the target is root', () => {
    it('creates a new element', async () => {
      const editor = await renderTestEditorWithCode(emptyProject, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      await pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const canvasRootElement = editor.renderedDOM
        .getByTestId(CanvasContainerID)
        .getBoundingClientRect()

      const insideDiv = {
        x: canvasRootElement.x + 120,
        y: canvasRootElement.y + 400,
      }

      await mouseDragFromPointToPoint(canvasControlsLayer, insideDiv, {
        x: insideDiv.x + 50,
        y: insideDiv.y + 50,
      })
      await editor.getDispatchFollowUpActionsFinished()

      typeText('Hey root')
      await closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      const newElementUID = Object.keys(editor.getEditorState().editor.jsxMetadata)
        .find((k) => k.startsWith('sb/'))
        ?.replace('sb/', '')

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
            import * as React from 'react'
            import { Storyboard } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='sb' >
                <span
                  style={{
                    position: 'absolute',
                    wordBreak: 'break-word',
                    left: 121,
                    top: 401,
                    width: 50,
                    height: 50,
                  }}
                  data-uid='${newElementUID}'
                >Hey root</span>
              </Storyboard>
            )`),
      )
    })
  })
})

function typeText(text: string) {
  document.execCommand('insertText', false, text)
}

async function closeTextEditor() {
  await pressKey('Escape')
  await wait(0) // this is needed so we wait until the dispatch call is launched in a settimeout when the text editor unmounts
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

const projectWithNonTextEditableDiv = formatTestProjectCode(`import * as React from 'react'
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
      <div data-uid='111'/>
    </div>
  </Storyboard>
)
`)

const emptyProject = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
  </Storyboard>
)
`)
