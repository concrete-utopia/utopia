import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { altCmdModifier, cmdModifier, ctrlModifier } from '../../utils/modifiers'
import { wait } from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../canvas/controls/new-canvas-controls'
import { keyDown, mouseClickAtPoint, pressKey } from '../canvas/event-helpers.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../canvas/ui-jsx.test-utils'
import { selectComponents } from './actions/meta-actions'

const TestIdOne = 'one'
const TestIdTwo = 'two'
const backgroundColor = '#384C5CAB'

// for some reason, ctrl + c does not trigger the eyedropper in tests
// maybe for security since the event is programmatically triggered?
xdescribe('shortcuts', () => {
  describe('eyedropper', () => {
    it('use eyedropper to set background color', async () => {
      const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')

      const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
      const div = editor.renderedDOM.getByTestId(TestIdOne)
      const divBounds = div.getBoundingClientRect()
      const divCorner = {
        x: divBounds.x + 50,
        y: divBounds.y + 40,
      }

      mouseClickAtPoint(canvasControlsLayer, divCorner)

      expect(editor.getEditorState().editor.selectedViews.length).toEqual(1)

      await wait(5000)

      keyDown('c', { modifiers: ctrlModifier })

      await wait(5000)

      const div2 = editor.renderedDOM.getByTestId(TestIdTwo)
      const div2Bounds = div2.getBoundingClientRect()
      const div2Corner = {
        x: div2Bounds.x + 50,
        y: div2Bounds.y + 40,
      }

      mouseClickAtPoint(canvasControlsLayer, div2Corner)

      await editor.getDispatchFollowUpActionsFinished()

      await wait(10000)

      //   expect(div.style.backgroundColor).toEqual(backgroundColor)
      expect(div2.style.backgroundColor).toEqual(backgroundColor)
    })
  })
})

const project = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

    export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='${TestIdOne}'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 67,
        top: 128,
        width: 161,
        height: 171,
      }}
      data-uid='e6b'
    />
    <div
      data-testid='${TestIdTwo}'
      style={{
        backgroundColor: '${backgroundColor}',
        position: 'absolute',
        left: 183,
        top: 350,
        width: 173,
        height: 222,
      }}
      data-uid='ecf'
    />
  </Storyboard>
)
`

describe('global shortcuts to set properties', () => {
  it('cmd + b toggles text to bold', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    pressKey('b', { modifiers: cmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontWeight: 'bold' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + b toggles text to normal if it was bold', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontWeight: 'bold' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    pressKey('b', { modifiers: cmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontWeight: 'normal' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + i toggles text to italic', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    pressKey('i', { modifiers: cmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontStyle: 'italic' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + i toggles text to normal if it was italic', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontStyle: 'italic' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    pressKey('i', { modifiers: cmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ fontStyle: 'normal' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + u toggles text to underline', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    pressKey('u', { modifiers: cmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ textDecoration: 'underline' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('cmd + u toggles text to none if it was underlined', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ textDecoration: 'underline' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await renderResult.dispatch(selectComponents([target], false), true)

    pressKey('u', { modifiers: cmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ textDecoration: 'none' }}
            data-uid='bbb'
          >hello text</div>
        </div>`,
      ),
    )
  })
  it('alt + cmd + v to paste style properties', async () => {
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

    // copy style properties first
    pressKey('c', { modifiers: altCmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)
    await renderResult.dispatch(selectComponents([target], false), true)

    // paste style properties
    pressKey('v', { modifiers: altCmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{  width: 200, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
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
