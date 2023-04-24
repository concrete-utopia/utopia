import { fireEvent } from '@testing-library/react'
import { forElementOptic } from '../../core/model/common-optics'
import {
  conditionalWhenFalseOptic,
  jsxConditionalExpressionOptic,
} from '../../core/model/conditionals'
import { JSXElementChild } from '../../core/shared/element-template'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import { compose3Optics, Optic } from '../../core/shared/optics/optics'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { altCmdModifier, cmdModifier } from '../../utils/modifiers'
import { selectComponents } from '../editor/actions/meta-actions'
import { EditorState } from '../editor/store/editor-state'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from './event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  makeTestProjectCodeWithSnippetWithoutUIDs,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from './ui-jsx.test-utils'

async function openContextMenuAndClickOnItem(
  renderResult: EditorRenderResult,
  menuItemText: string,
) {
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
  fireEvent.contextMenu(canvasControlsLayer)
  await renderResult.getDispatchFollowUpActionsFinished()

  const contextMenuItem = await renderResult.renderedDOM.findByText(menuItemText)
  const contextMenuItemBounds = contextMenuItem.getBoundingClientRect()
  await mouseClickAtPoint(contextMenuItem, contextMenuItemBounds)
  await renderResult.getDispatchFollowUpActionsFinished()
}

describe('canvas context menu', () => {
  it('clicking on paste layout menu item pastes layout properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, height: 150, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.2 }}
            data-uid='ccc'
            data-testid='ccc'
          >hello</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const copyPropertiesFrom = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
    )
    await renderResult.dispatch(selectComponents([copyPropertiesFrom], false), true)

    // copy properties first
    await pressKey('c', { modifiers: altCmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)
    await renderResult.dispatch(selectComponents([target], false), true)

    // paste only layout properties
    await openContextMenuAndClickOnItem(renderResult, 'Paste Layout')

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, height: 150, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.2, width: 200, height: 150 }}
            data-uid='ccc'
            data-testid='ccc'
          >hello</div>
        </div>`,
      ),
    )
  })
  it('clicking on paste style menu item pastes style properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.2, color: 'hotpink' }}
            data-uid='ccc'
          >hello</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const copyPropertiesFrom = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
    )
    await renderResult.dispatch(selectComponents([copyPropertiesFrom], false), true)

    // copy properties first
    await pressKey('c', { modifiers: altCmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)
    await renderResult.dispatch(selectComponents([target], false), true)

    // paste only layout properties
    await openContextMenuAndClickOnItem(renderResult, 'Paste Style')

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='ccc'
          >hello</div>
        </div>`,
      ),
    )
  })
  describe('wrap in from contextmenu', () => {
    it('wrap in div works inside a conditional on an expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div
                   style={{
                    height: 150,
                     width: 150,
                     position: 'absolute',
                     left: 154,
                     top: 134,
                     backgroundColor: 'lightblue',
                   }}
                   data-uid='then-div'
                   data-testid='then-div'
                 />
               ) : 'Test' 
             }
           </div>`,
        ),
        'await-first-dom-report',
      )

      const conditionalPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional`,
      )
      const inactiveElementOptic: Optic<EditorState, JSXElementChild> = compose3Optics(
        forElementOptic(conditionalPath),
        jsxConditionalExpressionOptic,
        conditionalWhenFalseOptic,
      )
      const inactiveElement = unsafeGet(inactiveElementOptic, renderResult.getEditorState().editor)
      const testValuePath = EP.appendToPath(conditionalPath, inactiveElement.uid)

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a div.
      await openContextMenuAndClickOnItem(renderResult, 'Wrap in div')

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div
                   style={{
                    height: 150,
                     width: 150,
                     position: 'absolute',
                     left: 154,
                     top: 134,
                     backgroundColor: 'lightblue',
                   }}
                   data-testid='then-div'
                 />
               ) : (
                  <div style={{ position: 'absolute'}}>
                   {'Test'}
                 </div>
               )
             }
           </div>`,
        ),
      )
    })
    it('wrap in div works inside a conditional on an element', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div
                   style={{
                    height: 150,
                     width: 150,
                     position: 'absolute',
                     left: 154,
                     top: 134,
                     backgroundColor: 'lightblue',
                   }}
                   data-uid='then-div'
                   data-testid='then-div'
                 />
               ) : 'Test' 
             }
           </div>`,
        ),
        'await-first-dom-report',
      )

      const testValuePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional/then-div`,
      )

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a div.
      await openContextMenuAndClickOnItem(renderResult, 'Wrap in div')

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div style={{ position: 'absolute'}}>
                   <div
                     style={{
                       height: 150,
                       width: 150,
                       position: 'absolute',
                       left: 154,
                       top: 134,
                       backgroundColor: 'lightblue',
                     }}
                     data-testid='then-div'
                   />
                 </div>
               ) : (
                'Test' 
               )
             }
           </div>`,
        ),
      )
    })
    it('wrap in div works on an element', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
             <div
               style={{
                 height: 150,
                 width: 150,
                 position: 'absolute',
                 left: 154,
                 top: 134,
                 backgroundColor: 'lightblue',
               }}
               data-uid='target-div'
               data-testid='target-div'
             />
           </div>`,
        ),
        'await-first-dom-report',
      )

      const testValuePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/target-div`,
      )

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a div.
      await openContextMenuAndClickOnItem(renderResult, 'Wrap in div')

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
             <div style={{ position: 'absolute'}}>
               <div
                 style={{
                   height: 150,
                   width: 150,
                   position: 'absolute',
                   left: 154,
                   top: 134,
                   backgroundColor: 'lightblue',
                 }}
                 data-testid='target-div'
               />
             </div>
           </div>`,
        ),
      )
    })
  })
})
