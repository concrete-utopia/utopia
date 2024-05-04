import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import * as EP from '../../core/shared/element-path'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { CodeElementSectionTestId } from './sections/code-element-section'
import { InspectorSectionsContainerTestID } from './inspector'

async function clickElementWithTestId(editor: EditorRenderResult, testid: string): Promise<void> {
  const targetElement = editor.renderedDOM.getByTestId(testid)
  const targetElementBounds = targetElement.getBoundingClientRect()
  const targetElementCenter = getDomRectCenter(targetElementBounds)
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

  await mouseClickAtPoint(canvasControlsLayer, targetElementCenter)
}

describe('The Inspector code element section', () => {
  const testProjectCode = `
  import * as React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='sb'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 10,
          top: 10,
          width: 50,
          height: 50,
        }}
        data-uid='aaa'
        data-testid='aaa'
      />
      {true ? (
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 70,
            top: 10,
            width: 50,
            height: 50,
          }}
          data-uid='bbb'
          data-testid='bbb'
        />
      ) : null}
      {
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 130,
            top: 10,
            width: 50,
            height: 50,
          }}
          data-uid='ccc'
          data-testid='ccc'
        />
      }
      {[0].map(() => (
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 190,
            top: 10,
            width: 50,
            height: 50,
          }}
          data-uid='ddd'
          data-testid='ddd'
        />
      ))}
    </Storyboard>
  )
  `

  it('Is not displayed when a regular element is selected', async () => {
    const editor = await renderTestEditorWithCode(testProjectCode, 'await-first-dom-report')
    await clickElementWithTestId(editor, 'aaa')

    // Check that the code element section is not displayed
    await expect(async () =>
      editor.renderedDOM.getByTestId(CodeElementSectionTestId),
    ).rejects.toThrow()
  })

  it('Is not displayed when a conditional is selected', async () => {
    const editor = await renderTestEditorWithCode(testProjectCode, 'await-first-dom-report')
    await clickElementWithTestId(editor, 'bbb')

    // Check that the code element section is not displayed
    await expect(async () =>
      editor.renderedDOM.getByTestId(CodeElementSectionTestId),
    ).rejects.toThrow()
  })

  it('Is the only section displayed when a code element is selected', async () => {
    const editor = await renderTestEditorWithCode(testProjectCode, 'await-first-dom-report')
    await clickElementWithTestId(editor, 'ccc')

    // Check that the code element section is displayed
    const codeElementSection = editor.renderedDOM.getByTestId(CodeElementSectionTestId)
    expect(codeElementSection).toBeDefined()

    // Check that the inspector only has one section
    const inspectorSectionsContainer = editor.renderedDOM.getByTestId(
      InspectorSectionsContainerTestID,
    )
    expect(inspectorSectionsContainer.children.length).toEqual(1)
  })

  it('Is not displayed when a map is selected', async () => {
    const editor = await renderTestEditorWithCode(testProjectCode, 'await-first-dom-report')
    await clickElementWithTestId(editor, 'ddd')

    // Check that the code element section is displayed
    const codeElementSections = editor.renderedDOM.queryAllByTestId(CodeElementSectionTestId)
    expect(codeElementSections).toHaveLength(0)
  })
})
