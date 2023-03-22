import { getConsoleLogsForTests } from '../../../core/shared/runtime-report-logs'
import { setFeatureForBrowserTests } from '../../../utils/utils.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../ui-jsx.test-utils'

describe('Canvas Renderer Warnings', () => {
  it('renders a Fragment without a react props error', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div data-uid='wrapper-div'>
          <React.Fragment data-uid='cica'>
            <View
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 190,
                top: 174,
                width: 132,
                height: 146,
              }}
              data-uid='99f'
            />
          </React.Fragment>
        </div>
      `),
      'await-first-dom-report',
    )

    expect(getConsoleLogsForTests()).toEqual([])
  })
})
