/* eslint-disable jest/expect-expect */
import { act } from '@testing-library/react'
import { forElementOptic } from '../../core/model/common-optics'
import { conditionalWhenTrueOptic } from '../../core/model/conditionals'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isRight } from '../../core/shared/either'
import * as EP from '../../core/shared/element-path'
import {
  emptyComments,
  isJSExpressionValue,
  isJSXConditionalExpression,
  jsExpressionValue,
  jsxAttributesEntry,
  jsxElement,
  JSXElement,
  jsxTextBlock,
} from '../../core/shared/element-template'
import { filtered, fromField, fromTypeGuard } from '../../core/shared/optics/optic-creators'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import { compose6Optics, Optic } from '../../core/shared/optics/optics'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../canvas/ui-jsx.test-utils'
import {
  deleteSelected,
  pasteJSXElements,
  selectComponents,
} from '../editor/actions/action-creators'
import { ElementPaste } from './action-types'
import { EditorState } from './store/editor-state'

describe('conditionals', () => {
  describe('deletion', () => {
    it('replaces a branch with null', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          true ? (
            <div data-uid='bbb' data-testid='bbb'>foo</div>
          ) : (
            <div data-uid='ccc' data-testid='ccc'>bar</div>
          )
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional', 'bbb'])
      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      await act(async () => {
        await renderResult.dispatch([deleteSelected()], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                // @utopia/uid=conditional
                true ? null : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                )
              }
            </div>
         `),
      )
    })
    it('replaces a text string branch with null', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          true ? 'hello' : 'there'
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const helloWorldUIDOptic: Optic<EditorState, string> = compose6Optics(
        forElementOptic(EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])),
        fromTypeGuard(isJSXConditionalExpression),
        conditionalWhenTrueOptic,
        fromTypeGuard(isJSExpressionValue),
        filtered((value) => value.value === 'hello'),
        fromField('uid'),
      )
      const helloWorldUID: string = unsafeGet(
        helloWorldUIDOptic,
        renderResult.getEditorState().editor,
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, [
        'aaa',
        'conditional',
        helloWorldUID,
      ])
      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      await act(async () => {
        await renderResult.dispatch([deleteSelected()], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                // @utopia/uid=conditional
                true ? null : 'there'
              }
            </div>
         `),
      )
    })
    it('keeps the selection on the null branch (single target)', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          true ? (
            <div data-uid='bbb' data-testid='bbb'>foo</div>
          ) : (
            <div data-uid='ccc' data-testid='ccc'>bar</div>
          )
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional', 'bbb'])
      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      await act(async () => {
        await renderResult.dispatch([deleteSelected()], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                // @utopia/uid=conditional
                true ? null : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                )
              }
            </div>
         `),
      )

      const { selectedViews } = renderResult.getEditorState().editor
      expect(selectedViews).toHaveLength(1)
      const conditionalPath = EP.parentPath(targetPath)
      expect(EP.isParentOf(conditionalPath, selectedViews[0])).toBe(true)
      expect(EP.toUid(selectedViews[0])).not.toBe('ccc')
    })
    it('keeps the selection on the null branch (multiple targets)', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional1
          true ? (
            <div data-uid='bbb' data-testid='bbb'>foo</div>
          ) : (
            <div data-uid='ccc' data-testid='ccc'>bar</div>
          )
        }
        {
          // @utopia/uid=conditional2
          true ? (
            <div data-uid='ddd' data-testid='ddd'>foo</div>
          ) : (
            <div data-uid='eee' data-testid='eee'>bar</div>
          )
        }
          <div data-uid='fff' data-testid='fff'>
            <div data-uid='ggg' data-testid='ggg'>baz</div>
          </div>
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath1 = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional1', 'bbb'])
      const targetPath2 = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional2', 'eee'])
      const targetPath3 = EP.appendNewElementPath(TestScenePath, ['aaa', 'fff', 'ggg'])
      await act(async () => {
        await renderResult.dispatch(
          [selectComponents([targetPath1, targetPath2, targetPath3], false)],
          false,
        )
      })

      await act(async () => {
        await renderResult.dispatch([deleteSelected()], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
            {
              // @utopia/uid=conditional1
              true ? (
                null
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
            {
              // @utopia/uid=conditional2
              true ? (
                <div data-uid='ddd' data-testid='ddd'>foo</div>
              ) : (
                null
              )
            }
              <div data-uid='fff' data-testid='fff' />
            </div>
         `),
      )

      const { selectedViews } = renderResult.getEditorState().editor
      expect(selectedViews).toHaveLength(3)
      const conditionalPath1 = EP.parentPath(targetPath1)
      const conditionalPath2 = EP.parentPath(targetPath2)
      expect(EP.isParentOf(conditionalPath1, selectedViews[0])).toBe(true)
      expect(EP.toUid(selectedViews[0])).not.toBe('ccc')
      expect(EP.isParentOf(conditionalPath2, selectedViews[1])).toBe(true)
      expect(EP.toUid(selectedViews[0])).not.toBe('eee')
      expect(EP.isParentOf(selectedViews[2], targetPath3)).toBe(true)
    })
  })
  describe('expressions', () => {
    it('stores the string expression', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? (
            <div data-uid='bbb' data-testid='bbb'>foo</div>
          ) : (
            <div data-uid='ccc' data-testid='ccc'>bar</div>
          )
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

      const meta = MetadataUtils.findElementByElementPath(
        renderResult.getEditorState().editor.jsxMetadata,
        targetPath,
      )
      if (meta != null && isRight(meta.element) && isJSXConditionalExpression(meta.element.value)) {
        expect(meta.element.value.originalConditionString).toEqual('[].length === 0')
      } else {
        throw new Error('invalid element')
      }
    })
  })
})
