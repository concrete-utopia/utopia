/* eslint-disable jest/expect-expect */
import { act } from '@testing-library/react'
import { FOR_TESTS_setNextGeneratedUids } from '../../core/model/element-template-utils.test-utils'
import * as EP from '../../core/shared/element-path'
import { setFeatureEnabled } from '../../utils/feature-switches'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../canvas/ui-jsx.test-utils'
import { deleteSelected, selectComponents } from '../editor/actions/action-creators'

describe('conditionals', () => {
  before(() => setFeatureEnabled('Conditional support', true))
  after(() => setFeatureEnabled('Conditional support', false))
  describe('deletion', () => {
    it('replaces a branch with null', async () => {
      FOR_TESTS_setNextGeneratedUids([
        'skip1',
        'skip2',
        'skip3',
        'skip4',
        'skip5',
        'skip6',
        'skip7',
        'skip8',
        'conditional',
      ])
      const startSnippet = `
        <div data-uid='aaa'>
        {
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
                true ? null : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                )
              }
            </div>
         `),
      )
    })
    it('replaces a text string branch with null', async () => {
      FOR_TESTS_setNextGeneratedUids(['skip1', 'skip2', 'skip3', 'skip4', 'conditional'])
      const startSnippet = `
        <div data-uid='aaa'>
        {
          true ? 'hello' : 'there'
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional', 'then-case'])
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
                true ? null : 'there'
              }
            </div>
         `),
      )
    })
  })
})
