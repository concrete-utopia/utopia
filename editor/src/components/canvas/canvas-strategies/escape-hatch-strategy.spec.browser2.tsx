import { act, fireEvent } from '@testing-library/react'
import * as EP from '../../../core/shared/element-path'
import { runEscapeHatch } from '../../editor/actions/action-creators'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../ui-jsx.test-utils'

describe('Convert to Absolute/runEscapeHatch action', () => {
  it('Converts 1 element to absolute', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
            data-uid='bbb'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 120 }}
            data-uid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc'])

    await renderResult.dispatch([runEscapeHatch([target])], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
            data-uid='bbb'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 120, position: 'absolute', left: 0, top: 80 }}
            data-uid='ccc'
          />
        </div>
      `),
    )
  })
  it('Converts 1 element to absolute from flex containers, keeping the visual position', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative', display: 'flex' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
            data-uid='bbb'
          />
          <div
            style={{ display: 'flex', flexDirection: 'column' }}
            data-uid='ccc'
          >
            <div style={{ width: 100, height: 100, backgroundColor: 'hotpink'}} data-uid='ddd' />
            <div style={{ width: 100, height: 100, backgroundColor: 'yellow'}} data-uid='eee' />
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc', 'eee'])

    await renderResult.dispatch([runEscapeHatch([target])], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ width: '100%', height: '100%', position: 'relative', display: 'flex' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
          data-uid='bbb'
        />
        <div
          style={{ display: 'flex', flexDirection: 'column' }}
          data-uid='ccc'
        >
          <div style={{ width: 100, height: 100, backgroundColor: 'hotpink'}} data-uid='ddd' />
          <div style={{ width: 100, height: 100, backgroundColor: 'yellow', position: 'absolute', left: 200, top: 100 }} data-uid='eee' />
        </div>
      </div>
      `),
    )
  })
  /* eslint-disable @typescript-eslint/no-empty-function */
  xit('Converts multiselect', async () => {})
  xit('Converts multiselect in hierarchy', async () => {})
  xit('Converts element with absolute descendants', async () => {})
})
