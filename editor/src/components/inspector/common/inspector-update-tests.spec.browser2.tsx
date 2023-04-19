import * as EP from '../../../core/shared/element-path'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as PP from '../../../core/shared/property-path'
import { emptyComments, jsExpressionValue } from '../../../core/shared/element-template'

describe('updating style properties keeps the original order', () => {
  it('element with different padding props', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style, position: 'absolute' }} data-uid='aaa'>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 256,
            height: 202,
            paddingRight: 15,
            padding: '2px 4px',
            paddingLeft: 10
          }}
          data-uid='bbb'
        />
      </div>
      `),
      'await-first-dom-report',
    )

    const changePinProps = setProp_UNSAFE(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      PP.create('style', 'paddingRight'),
      jsExpressionValue(30, emptyComments),
    )

    await renderResult.dispatch([changePinProps], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style, position: 'absolute' }} data-uid='aaa'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 256,
              height: 202,
              paddingRight: 30,
              padding: '2px 4px',
              paddingLeft: 10
            }}
            data-uid='bbb'
          />
        </div>`,
      ),
    )
  })
})
