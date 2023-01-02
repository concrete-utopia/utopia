import { setFeatureEnabled } from '../../../../utils/feature-switches'
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

describe('draw-to-insert text', () => {
  before(() => {
    setFeatureEnabled('Text editing', true)
  })
  describe('draw', () => {
    it('allows drawing to insert some text', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId('div')
      const divBounds = div.getBoundingClientRect()

      pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const outsideDiv = {
        x: divBounds.x + divBounds.width + 100,
        y: divBounds.y + 100,
      }

      mouseDragFromPointToPoint(canvasControlsLayer, outsideDiv, {
        x: outsideDiv.x + 100,
        y: outsideDiv.y + 250,
      })
      await editor.getDispatchFollowUpActionsFinished()

      typeText('Utopia')
      closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      const newElementUID = Object.keys(editor.getEditorState().editor.domMetadata)
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
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 389,
                    top: 101,
                    width: 100,
                    height: 250,
                  }}
                  data-uid='${newElementUID}'
                >Utopia</div>
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

      pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const outsideDiv = {
        x: divBounds.x + divBounds.width + 100,
        y: divBounds.y + 100,
      }

      mouseClickAtPoint(canvasControlsLayer, outsideDiv)
      await editor.getDispatchFollowUpActionsFinished()

      typeText('Utopia')
      closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      const newElementUID = Object.keys(editor.getEditorState().editor.domMetadata)
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
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 339,
                    top: 51,
                    width: 100,
                    height: 100,
                  }}
                  data-uid='${newElementUID}'
                >Utopia</div>
              </Storyboard>
            )`),
      )
    })
  })
  describe('when the target is editable', () => {
    it('just goes into text edit mode immediately', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId('div')
      const divBounds = div.getBoundingClientRect()

      pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const insideDiv = {
        x: divBounds.x + divBounds.width / 2,
        y: divBounds.y + divBounds.height / 2,
      }

      mouseDragFromPointToPoint(canvasControlsLayer, insideDiv, {
        x: insideDiv.x + 50,
        y: insideDiv.y + 50,
      })
      await editor.getDispatchFollowUpActionsFinished()

      typeText(' Utopia')
      closeTextEditor()
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
                >Hello Utopia</div>
              </Storyboard>
            )`),
      )
    })
  })
  describe('when the target is root', () => {
    it('creates a new element', async () => {
      const editor = await renderTestEditorWithCode(emptyProject, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      const insideDiv = {
        x: 500,
        y: 500,
      }

      mouseDragFromPointToPoint(canvasControlsLayer, insideDiv, {
        x: insideDiv.x + 50,
        y: insideDiv.y + 50,
      })
      await editor.getDispatchFollowUpActionsFinished()

      typeText('Hey root')
      closeTextEditor()
      await editor.getDispatchFollowUpActionsFinished()

      const newElementUID = Object.keys(editor.getEditorState().editor.domMetadata)
        .find((k) => k.startsWith('sb/'))
        ?.replace('sb/', '')

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        formatTestProjectCode(`
            import * as React from 'react'
            import { Storyboard } from 'utopia-api'

            export var storyboard = (
              <Storyboard data-uid='sb'>
                <div
                  style={{
                    backgroundColor: '#aaaaaa33',
                    position: 'absolute',
                    left: 112,
                    top: 391,
                    width: 50,
                    height: 50,
                  }}
                  data-uid='${newElementUID}'
                >Hey root</div>
              </Storyboard>
            )`),
      )
    })
  })
})

function typeText(text: string) {
  document.execCommand('insertText', false, text)
}

function closeTextEditor() {
  pressKey('Escape')
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

const emptyProject = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
  </Storyboard>
)
`)
