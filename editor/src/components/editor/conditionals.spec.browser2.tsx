/* eslint-disable jest/expect-expect */
import { act } from '@testing-library/react'
import { forElementOptic } from '../../core/model/common-optics'
import { conditionalWhenTrueOptic } from '../../core/model/conditionals'
import {
  filtered,
  fromField,
  fromTypeGuard,
  traverseArray,
} from '../../core/shared/optics/optic-creators'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import {
  compose3Optics,
  compose4Optics,
  compose5Optics,
  compose6Optics,
  compose8Optics,
  Optic,
} from '../../core/shared/optics/optics'
import { isParseSuccess, isTextFile, TextFile } from '../../core/shared/project-file-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isRight } from '../../core/shared/either'
import * as EP from '../../core/shared/element-path'
import {
  isJSXAttributeValue,
  isJSXConditionalExpression,
  isUtopiaJSXComponent,
  JSExpressionValue,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import { setFeatureEnabled } from '../../utils/feature-switches'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../canvas/ui-jsx.test-utils'
import { deleteSelected, selectComponents } from '../editor/actions/action-creators'
import { EditorState } from './store/editor-state'

describe('conditionals', () => {
  before(() => setFeatureEnabled('Conditional support', true))
  after(() => setFeatureEnabled('Conditional support', false))
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
        fromTypeGuard(isJSXAttributeValue),
        filtered((value) => value.value === 'hello'),
        fromField('uniqueID'),
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
