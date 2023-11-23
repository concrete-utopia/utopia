import type { EditorRenderResult } from '../ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'
import { mouseDoubleClickAtPoint } from '../event-helpers.test-utils'
import * as EP from '../../../core/shared/element-path'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { selectComponents } from '../../editor/actions/meta-actions'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { ZeroSizedEventsControlTestID } from './zero-sized-element-controls'

async function selectTargetAndDoubleClickOnZeroSizeControl(
  renderResult: EditorRenderResult,
  target: ElementPath,
  testID: string,
  shiftX: number = 0,
  shiftY: number = 0,
) {
  await renderResult.dispatch(selectComponents([target], false), true)

  const zeroSizeControl = renderResult.renderedDOM.getByTestId(ZeroSizedEventsControlTestID)
  const element = renderResult.renderedDOM.getByTestId(testID)
  const elementBounds = element.getBoundingClientRect()

  const targetPoint = {
    x: elementBounds.x + shiftX,
    y: elementBounds.y + shiftY,
  }
  await mouseDoubleClickAtPoint(zeroSizeControl, targetPoint)

  await renderResult.getDispatchFollowUpActionsFinished()
}

describe('Zero sized element controls', () => {
  it('double click on a span element opens text editor', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <span style={{ position: 'absolute', top: 20, left: 20 }} data-uid='bbb' data-testid='bbb' />
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await selectTargetAndDoubleClickOnZeroSizeControl(renderResult, target, 'bbb')

    expect(renderResult.getEditorState().editor.mode.type).toEqual('textEdit')
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <span style={{ position: 'absolute', top: 20, left: 20 }} data-uid='bbb' data-testid='bbb' />
        </div>`,
      ),
    )
  })

  it('double click on a div element adds size', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div style={{ position: 'absolute', top: 20, left: 20 }} data-uid='bbb' data-testid='bbb' />
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await selectTargetAndDoubleClickOnZeroSizeControl(renderResult, target, 'bbb')

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div style={{ position: 'absolute', top: 20, left: 20, width: 100, height: 100 }} data-uid='bbb' data-testid='bbb' />
        </div>`,
      ),
    )
  })
  it('double click on a div element with a flex parent keeps position: absolute and pins, adds width and height', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div style={{ position: 'absolute', top: 20, left: 20, width: 100, height: 100, display: 'flex' }} data-uid='container'>
            <div style={{ position: 'absolute', top: 20, left: 20, flexBasis: 0 }} data-uid='bbb' data-testid='bbb' />
          </div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/container/bbb`,
    )
    await selectTargetAndDoubleClickOnZeroSizeControl(renderResult, target, 'bbb')

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div style={{ position: 'absolute', top: 20, left: 20, width: 100, height: 100, display: 'flex' }} data-uid='container'>
            <div style={{ position: 'absolute', top: 20, left: 20, width: 100, height: 100 }} data-uid='bbb' data-testid='bbb' />
          </div>
        </div>`,
      ),
    )
  })
  it('double click slightly outside a div element (where the zero sized border is visually) adds size', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div style={{ position: 'absolute', top: 20, left: 20 }} data-uid='bbb' data-testid='bbb' />
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await selectTargetAndDoubleClickOnZeroSizeControl(renderResult, target, 'bbb', -2, -2)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div style={{ position: 'absolute', top: 20, left: 20, width: 100, height: 100 }} data-uid='bbb' data-testid='bbb' />
        </div>`,
      ),
    )
  })
  it('double click on a inline element adds size and display prop', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <i style={{ position: 'absolute', top: 20, left: 20 }} data-uid='bbb' data-testid='bbb' />
        </div>`,
      ),
      'await-first-dom-report',
    )

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)
    await selectTargetAndDoubleClickOnZeroSizeControl(renderResult, target, 'bbb')

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <i style={{ position: 'absolute', top: 20, left: 20, width: 100, height: 100, display: 'inline-block' }} data-uid='bbb' data-testid='bbb' />
        </div>`,
      ),
    )
  })
})
