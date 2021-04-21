import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from './ui-jsx.test-utils'

describe('Dom-walker Caching', () => {
  it('runs the dom walker for a fresh canvas', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} layout={{ layoutSystem: 'pinSystem' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='bbb'
        />
      </View>
      `),
    )
  })
})
