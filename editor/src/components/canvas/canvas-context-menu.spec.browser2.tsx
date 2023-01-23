import { fireEvent } from '@testing-library/react'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { cmdModifier } from '../../utils/modifiers'
import { selectComponents } from '../editor/actions/meta-actions'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from './event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
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
  mouseClickAtPoint(contextMenuItem, contextMenuItemBounds)
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
    pressKey('c', { modifiers: cmdModifier })
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
    pressKey('c', { modifiers: cmdModifier })
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
})
