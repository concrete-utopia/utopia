import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { altModifier } from '../../../utils/modifiers'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../canvas//ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/meta-actions'
import { keyDown } from '../event-helpers.test-utils'

describe('Canvas Controls', () => {
  it('absolute children outline only included for non-group absolutely positioned elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          data-uid='root'
        >
          <Group
            style={{
              backgroundColor: 'white',
              height: 448,
              width: 459,
              position: 'absolute',
              left: 202,
              top: 175,
            }}
            data-uid='group'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 158,
                height: 161,
              }}
              data-uid='group-child-1'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 258,
                top: 179,
                width: 201,
                height: 269,
              }}
              data-uid='group-child-2'
            />
          </Group>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 190,
              top: 647,
              width: 491,
              height: 94,
            }}
            data-uid='div'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 33,
                top: 20,
                width: 74,
                height: 49,
              }}
              data-uid='div-child-1'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 381,
                top: 17,
                width: 101,
                height: 52,
              }}
              data-uid='div-child-2'
            />
          </div>
        </div>`),
      'await-first-dom-report',
    )

    // Check that the group does not have the absolute children outline.
    const groupPath = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group`,
    )
    await renderResult.dispatch(selectComponents([groupPath], false), true)
    await renderResult.getDispatchFollowUpActionsFinished()
    const possibleAbsoluteControlForGroup =
      renderResult.renderedDOM.queryByTestId(`absolute-children-outline`)
    expect(possibleAbsoluteControlForGroup).toBeNull()

    // Check that the div parent does have the absolute children outline.
    const divPath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/div`)
    await renderResult.dispatch(selectComponents([divPath], false), true)
    await renderResult.getDispatchFollowUpActionsFinished()
    const possibleAbsoluteControlForDiv =
      renderResult.renderedDOM.queryByTestId(`absolute-children-outline`)
    expect(possibleAbsoluteControlForDiv).not.toBeNull()
  })
})

describe('Mouse Cursor', () => {
  it('Uses the default cursor in select mode', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet('<div/>'),
      'await-first-dom-report',
    )

    const canvasControls = renderResult.renderedDOM.getByTestId('canvas-controls')

    expect(canvasControls.style.cursor).toContain('cursor-default.png')
  })
  it('Uses the duplicate cursor in select mode when ALT is pressed', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet('<div/>'),
      'await-first-dom-report',
    )

    await keyDown('Alt', { modifiers: altModifier })
    const canvasControls = renderResult.renderedDOM.getByTestId('canvas-controls')

    expect(canvasControls.style.cursor).toContain('cursor-duplicate.png')
  })
})
