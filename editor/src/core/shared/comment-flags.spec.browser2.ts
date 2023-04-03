import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithComponentInnards,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../components/canvas/ui-jsx.test-utils'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { MetadataUtils } from '../model/element-metadata-utils'
import { wait } from '../model/performance-scripts'
import { BakedInStoryboardUID } from '../model/scene-utils'
import { fromString } from './element-path'

describe('comment flags', () => {
  it('can parse a conditional toggle', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional1
          // @utopia/conditional=false
          true ? (
              <div data-uid='bbb' data-testid='bbb'>foo</div>
            ) : (
              <div data-uid='ccc' data-testid='ccc'>bar</div>
            )
          }
        </div>
`),
      'await-first-dom-report',
    )

    const path = fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional1`,
    )

    const conditional = MetadataUtils.findElementByElementPath(
      editor.getEditorState().editor.jsxMetadata,
      path,
    )
    if (conditional == null) {
      throw new Error('should not be null')
    }
    expect(conditional.conditionValue).toEqual(false)
  })

  it('can parse an uid', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
                <div data-uid='aaa'>
                    // @utopia/uid=bbb
                    <div />
                </div>
        `),
      'await-first-dom-report',
    )

    const path = fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`)

    const conditional = MetadataUtils.findElementByElementPath(
      editor.getEditorState().editor.jsxMetadata,
      path,
    )

    if (conditional == null) {
      throw new Error('should not be null')
    }

    expect(conditional.elementPath).toEqual(path)
  })

  //   it('can parse a group flag', async () => {})
})
