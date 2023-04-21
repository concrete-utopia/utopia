import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  renderTestEditorWithCode,
  makeTestProjectCodeWithSnippet,
  TestSceneUID,
  TestAppUID,
} from '../../ui-jsx.test-utils'
import { GroupFlagKey, isElementMarkedAsGroup } from './group-like-helpers'

describe('group like helpers', () => {
  describe('group flag', () => {
    it('extracting the group flag from a conditional', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
                <div data-uid='aaa'>
                {
                  // @utopia/uid=conditional1
                  // @utopia/group=true
                  true ? (
                      <div data-uid='bbb' data-testid='bbb'>foo</div>
                    ) : (
                      <div data-uid='ccc' data-testid='ccc'>bar</div>
                    )
                }
                {
                    // @utopia/uid=conditional2
                    // @utopia/group=false
                    true ? (
                        <div data-uid='bbb' data-testid='xxx'>foo</div>
                    ) : (
                        <div data-uid='ccc' data-testid='yyy'>bar</div>
                    )
                }
                {
                    // @utopia/uid=conditional3
                    true ? (
                        <div data-uid='bbb' data-testid='ttt'>foo</div>
                    ) : (
                        <div data-uid='ccc' data-testid='uuu'>bar</div>
                    )
                }
                </div>
        `),
        'await-first-dom-report',
      )

      const conditional1 = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional1`,
      )

      expect(
        isElementMarkedAsGroup(editor.getEditorState().editor.jsxMetadata, conditional1),
      ).toEqual(true)

      const conditional2 = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional2`,
      )

      expect(
        isElementMarkedAsGroup(editor.getEditorState().editor.jsxMetadata, conditional2),
      ).toEqual(false)

      const conditional3 = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional3`,
      )

      expect(
        isElementMarkedAsGroup(editor.getEditorState().editor.jsxMetadata, conditional3),
      ).toEqual(false)
    })

    it('extracting the group flag from a jsx element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
            <div data-uid='a' ${GroupFlagKey}={true} />
            <div data-uid='b' ${GroupFlagKey} />
            <div data-uid='c' ${GroupFlagKey}={false} />
            <div data-uid='d' />
        </div>
    `),
        'await-first-dom-report',
      )

      expect(
        isElementMarkedAsGroup(
          editor.getEditorState().editor.jsxMetadata,
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/a`),
        ),
      ).toEqual(true)

      expect(
        isElementMarkedAsGroup(
          editor.getEditorState().editor.jsxMetadata,
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/b`),
        ),
      ).toEqual(true)

      expect(
        isElementMarkedAsGroup(
          editor.getEditorState().editor.jsxMetadata,
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/c`),
        ),
      ).toEqual(false)

      expect(
        isElementMarkedAsGroup(
          editor.getEditorState().editor.jsxMetadata,
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/d`),
        ),
      ).toEqual(false)
    })
  })
})
