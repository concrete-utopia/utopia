import * as EP from '../../../../core/shared/element-path'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../../canvas/controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  pressKey,
} from '../../../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../../canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'
import type { InsertMode, TextEditMode } from '../../../editor/editor-modes'

describe('Text edit mode', () => {
  describe('Entering text edit mode', () => {
    it('Enters insert mode without selected element', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      await pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('insert')
      expect((editor.getEditorState().editor.mode as InsertMode).subjects.length).toBeGreaterThan(0)
    })
    it('Entering insert even if editable element is selected', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      await selectElement(editor, EP.fromString('sb/39e'))
      await pressKey('t')
      await editor.getDispatchFollowUpActionsFinished()

      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('insert')
      expect((editor.getEditorState().editor.mode as InsertMode).subjects.length).toBeGreaterThan(0)
    })
    ;['div', 'p'].forEach((tag) => {
      it(`Entering text edit mode with double click on selected ${tag} text editable element`, async () => {
        const editor = await renderTestEditorWithCode(
          project('hello', tag),
          'await-first-dom-report',
        )
        await selectElement(editor, EP.fromString('sb/39e'))
        await clickOnElement(editor, tag, 'double-click')
        // wait for the next frame
        await wait(1)

        expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
        expect(
          EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
        ).toEqual('sb/39e')
        expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
        expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
      })
    })
    it('Entering text edit mode with double click on selected text editable element with only code inside', async () => {
      const editor = await renderTestEditorWithCode(projectWithCodeText, 'await-first-dom-report')
      await selectElement(editor, EP.fromString('sb/39e'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it('Entering text edit mode with double click on conditional with expression in active branch', async () => {
      const editor = await renderTestEditorWithCode(
        project(`{
        // @utopia/uid=cond
        true ? 'Hello' : <div />
      }`),
        'await-first-dom-report',
      )
      await selectElement(editor, EP.fromString('sb/39e/cond'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e/cond')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e/cond')
    })

    it('Entering text edit mode with double click on conditional with expression in both branches', async () => {
      const editor = await renderTestEditorWithCode(
        project(`{
        // @utopia/uid=cond
        true ? 'Hello' : Utopia
      }`),
        'await-first-dom-report',
      )
      await selectElement(editor, EP.fromString('sb/39e/cond'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e/cond')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e/cond')
    })
    it('Can not entering text edit mode with double click on conditional with expression in active branch when there is sibling', async () => {
      const editor = await renderTestEditorWithCode(
        project(`{
          // @utopia/uid=cond
          true ? 'Hello' : <div />
        }
        <div />`),
        'await-first-dom-report',
      )
      await selectElement(editor, EP.fromString('sb/39e/cond'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it('Can not enter text edit mode with double click on conditional with element in active branch', async () => {
      const editor = await renderTestEditorWithCode(
        project(`{
          // @utopia/uid=cond
          true ? <div>Hello</div> : <div />
        }`),
        'await-first-dom-report',
      )
      await selectElement(editor, EP.fromString('sb/39e/cond'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
    })
    it('Can not enter text edit mode with double click on conditional with expression in active branch which returns an element', async () => {
      const editor = await renderTestEditorWithCode(
        project(`{
          // @utopia/uid=cond
          true ? (
            // @utopia/uid=expr
            (() => <div>hello</div>)()
          ) : (
            <div />
          )
        }`),
        'await-first-dom-report',
      )
      await selectElement(editor, EP.fromString('sb/39e/cond'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual(
        'sb/39e/cond/expr',
      )
    })
    it('Entering text edit mode with double click on selected multiline text editable element', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithMultilineText,
        'await-first-dom-report',
      )
      await selectElement(editor, EP.fromString('sb/39e'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it('Clicking on selected text editable but empty element should not enter text edit mode', async () => {
      const editor = await renderTestEditorWithCode(projectWithEmptyText, 'await-first-dom-report')
      await selectElement(editor, EP.fromString('sb/39e'))
      await clickOnElement(editor, 'div', 'double-click')
      // wait for the next frame
      await wait(1)

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it('Entering text edit mode with pressing enter on a text editable selected element', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')
      await selectElement(editor, EP.fromString('sb/39e'))
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it('Does not enter text edit mode with pressing enter on a selected void html element', async () => {
      const editor = await renderTestEditorWithCode(
        project(`<img
          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'
          alt='Utopia logo'
          style={{ height: '100%' }}
          data-uid='b0e'
        />`),
        'await-first-dom-report',
      )
      await selectElement(editor, EP.fromString('sb/39e/b0e'))
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e/b0e')
    })
    it('Does not enter text edit mode with pressing enter on a selected group', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`<Group
          style={{
            backgroundColor: 'blue',
            width: 100,
            height: 100,
          }}
          data-uid='group'
        />`),
        'await-first-dom-report',
      )
      const target = EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:group')
      await selectElement(editor, target)
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(editor.getEditorState().editor.selectedViews[0]).toEqual(target)
    })
    it('Entering text edit mode with pressing enter on a multiline text editable selected element', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithMultilineText,
        'await-first-dom-report',
      )
      await selectElement(editor, EP.fromString('sb/39e'))
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it('Entering text edit mode with pressing enter on a text editable but empty selected element', async () => {
      const editor = await renderTestEditorWithCode(projectWithEmptyText, 'await-first-dom-report')
      await selectElement(editor, EP.fromString('sb/39e'))
      await pressKey('enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it("can not enter text edit mode with component that doesn't support children", async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
      import { Scene, Storyboard } from 'utopia-api'
      import { Rectangle } from 'utopia-api'
      
      var App = () => {
        return (
          <div
            style={{
              width: '100%',
              height: '100%',
              background: 'white',
              justifyContent: 'center',
              alignItems: 'center',
            }}
            data-uid='root'
          >
            <Rectangle
              style={{
                backgroundColor: '#FF69B4AB',
                position: 'absolute',
                left: 77,
                top: 111,
                width: 223,
                height: 317,
              }}
              data-uid='rect'
            />
          </div>
        )
      }
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            style={{
              width: 380,
              height: 540,
              position: 'absolute',
              left: 55,
              top: 104,
            }}
            data-label='My App'
            data-uid='scene'
          >
            <App style={{}} data-uid='app' />
          </Scene>
        </Storyboard>
      )
      `,
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/scene/app:root/rect')])

      await pressKey('Enter')
      await editor.getDispatchFollowUpActionsFinished()

      expect(editor.getEditorState().editor.mode.type).toEqual('select')
      expect(editor.getEditorState().editor.toasts.length).toEqual(1)
      expect(editor.getEditorState().editor.toasts[0].message).toEqual(
        "This element doesn't support children, so it cannot be text edited",
      )
    })
  })

  describe('Click to choose target text for editing', () => {
    it('Click to select text editable target', async () => {
      const editor = await renderTestEditorWithCode(projectWithText, 'await-first-dom-report')

      await pressKey('t')
      await clickOnElement(editor, 'div')

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it('Click to select multiline text editable target', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithMultilineText,
        'await-first-dom-report',
      )

      await pressKey('t')
      await clickOnElement(editor, 'div')

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e')
    })
    it('Click to select conditional text editable target', async () => {
      const editor = await renderTestEditorWithCode(
        project(`{
        // @utopia/uid=cond
        true ? 'Hello' : <div />
      }`),

        'await-first-dom-report',
      )

      await pressKey('t')
      await clickOnElement(editor, 'div')

      expect(editor.getEditorState().editor.mode.type).toEqual('textEdit')
      expect(
        EP.toString((editor.getEditorState().editor.mode as TextEditMode).editedText!),
      ).toEqual('sb/39e/cond')
      expect(editor.getEditorState().editor.selectedViews).toHaveLength(1)
      expect(EP.toString(editor.getEditorState().editor.selectedViews[0])).toEqual('sb/39e/cond')
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
    x: divBounds.x + 10,
    y: divBounds.y + 10,
  }

  if (singleOrDoubleClick === 'single-click') {
    await mouseClickAtPoint(canvasControlsLayer, divCorner)
  } else {
    await mouseDoubleClickAtPoint(canvasControlsLayer, divCorner)
  }
  await editor.getDispatchFollowUpActionsFinished()
}

const project = (snippet: string, tag = 'div') => {
  return formatTestProjectCode(`import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  
  const title = 'Hello'
  export var storyboard = (
    <Storyboard data-uid='sb'>
      <${tag}
        data-testid='${tag}'
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
        ${snippet}
      </${tag}>
    </Storyboard>
  )`)
}

const projectWithCodeText = project('{title}')

const projectWithText = project('hello')

const projectWithMultilineText = project(`Hello<br />
Utopia`)

const projectWithEmptyText = project('')
