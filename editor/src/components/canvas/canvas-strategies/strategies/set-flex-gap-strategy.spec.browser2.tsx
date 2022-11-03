import { renderTestEditorWithCode } from '../../ui-jsx.test-utils'

describe('Flex gap strategy', () => {
  it('gap control strategy is not applicable when element has no children', async () => {
    const editor = await renderTestEditorWithCode('', 'await-first-dom-report')
  })
  it('gap control strategy is not applicable when flex gap is not set on element', async () => {
    const editor = await renderTestEditorWithCode('', 'await-first-dom-report')
  })
  it('gap control strategy is applicable when flex gap is set on element and element has children', async () => {
    const editor = await renderTestEditorWithCode('', 'await-first-dom-report')
  })
})
