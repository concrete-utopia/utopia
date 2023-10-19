import * as EP from '../../../../../core/shared/element-path'
import {
  expectSingleUndo2Saves,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../../../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../../../../canvas/event-helpers.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../../../editor/actions/meta-actions'

describe('Simplified layout subsection', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Simplified Layout Section', true)

  describe('FixedFillHug label', () => {})
  it('Changing pins on an element with max-content width', async () => {
    const testCode = `
        <div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ position: 'absolute', left: 40, top: 20, width: '100', height: 50}}
            data-uid='bbb'
            data-testid='bbb'
          >hello content</div>
        </div>
`
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(testCode),
      'await-first-dom-report',
    )
    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await renderResult.dispatch(selectComponents([targetPath], false), true)

    await expectSingleUndo2Saves(renderResult, async () => {
      const pinControlRight = renderResult.renderedDOM.getByTestId(
        'positioncontrols-catcher-pin-right',
      )
      const pinControlRightRect = pinControlRight.getBoundingClientRect()
      await mouseClickAtPoint(pinControlRight, {
        x: pinControlRightRect.x,
        y: pinControlRightRect.y,
      })
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div
          style={{ position: 'absolute', top: 20, width: 'max-content', height: 50, right: 277}}
          data-uid='bbb'
          data-testid='bbb'
        >hello content</div>
      </div>
      `),
    )
  })
  it('Changing pins on an element with max-content height', async () => {
    const testCode = `
        <div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ position: 'absolute', left: 40, top: 20, width: 'max-content', height: 'max-content'}}
            data-uid='bbb'
            data-testid='bbb'
          >hello content</div>
        </div>
`
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(testCode),
      'await-first-dom-report',
    )
    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await renderResult.dispatch(selectComponents([targetPath], false), true)

    await expectSingleUndo2Saves(renderResult, async () => {
      const pinControlBottom = renderResult.renderedDOM.getByTestId(
        'positioncontrols-catcher-pin-bottom',
      )
      const pinControlBottomRect = pinControlBottom.getBoundingClientRect()
      await mouseClickAtPoint(pinControlBottom, {
        x: pinControlBottomRect.x,
        y: pinControlBottomRect.y,
      })
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div
          style={{ position: 'absolute', left: 40, width: 'max-content', height: 'max-content', bottom: 362}}
          data-uid='bbb'
          data-testid='bbb'
        >hello content</div>
      </div>
      `),
    )
  })
})
