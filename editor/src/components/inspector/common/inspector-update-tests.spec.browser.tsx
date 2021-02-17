import * as React from 'react'
import * as TP from '../../../core/shared/template-path'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import { act } from 'react-test-renderer'
import { setElectronWindow } from '../../../core/shared/test-setup.test-utils'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as PP from '../../../core/shared/property-path'
import { jsxAttributeValue } from '../../../core/shared/element-template'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'

describe('updating style properties keeps the original order', () => {
  beforeAll(setElectronWindow)
  it('element with different padding props', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style, position: 'absolute' }} data-uid='aaa'>
        <div
          style={{
            backgroundColor: '#0091FFAA',
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
    )

    const changePinProps = setProp_UNSAFE(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      PP.create(['style', 'paddingRight']),
      jsxAttributeValue(30, emptyComments),
    )

    await renderResult.dispatch([changePinProps], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style, position: 'absolute' }} data-uid='aaa'>
          <div
            style={{
              backgroundColor: '#0091FFAA',
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
