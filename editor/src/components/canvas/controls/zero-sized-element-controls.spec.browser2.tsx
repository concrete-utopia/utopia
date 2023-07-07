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
import { ZeroSizedControlTestID } from './zero-sized-element-controls'
import type { ElementPath } from '../../../core/shared/project-file-types'

async function selectTargetAndDoubleClickOnZeroSizeControl(
  renderResult: EditorRenderResult,
  target: ElementPath,
  testID: string,
) {
  await renderResult.dispatch(selectComponents([target], false), true)

  const zeroSizeControl = renderResult.renderedDOM.getByTestId(ZeroSizedControlTestID)
  const element = renderResult.renderedDOM.getByTestId(testID)
  const elementBounds = element.getBoundingClientRect()

  const topLeftCorner = {
    x: elementBounds.x,
    y: elementBounds.y,
  }
  await mouseDoubleClickAtPoint(zeroSizeControl, topLeftCorner)

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
})
