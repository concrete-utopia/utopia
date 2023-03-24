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
  emptyComments,
  isJSXConditionalExpression,
  jsxAttributesEntry,
  jsExpressionValue,
  jsxElement,
  JSXElement,
  jsxTextBlock,
  isJSExpressionValue,
} from '../../core/shared/element-template'
import { setFeatureEnabled } from '../../utils/feature-switches'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../canvas/ui-jsx.test-utils'
import { EditorState } from './store/editor-state'
import {
  deleteSelected,
  pasteJSXElements,
  selectComponents,
} from '../editor/actions/action-creators'
import { ElementPaste } from './action-types'

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
  describe('pasting into a conditional', () => {
    function makeDiv(uid: string, text: string): JSXElement {
      return jsxElement(
        'div',
        uid,
        [jsxAttributesEntry('data-uid', jsExpressionValue(uid, emptyComments), emptyComments)],
        [jsxTextBlock(text)],
      )
    }
    function makeElementPaste(element: JSXElement): ElementPaste {
      return {
        importsToAdd: {},
        originalElementPath: EP.appendNewElementPath(TestScenePath, ['000']),
        element: element,
      }
    }
    const tests: {
      name: string
      code: string
      paste: ElementPaste[]
      want: string
    }[] = [
      {
        name: 'into element (true branch)',
        code: `
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
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <>
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                  <div data-uid='ddd'>HELLO</div>
                </>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
      },
      {
        name: 'into element (false branch)',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              false ? (
                <div data-uid='bbb' data-testid='bbb'>foo</div>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              false ? (
                <div data-uid='bbb' data-testid='bbb'>foo</div>
              ) : (
                <>
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                  <div data-uid='ddd'>HELLO</div>
                </>
              )
            }
          </div>
        `,
      },
      {
        name: 'into null',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                null
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <div data-uid='ddd' style={{ display: 'block' }}>HELLO</div>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
      },
      {
        name: 'into fragment',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <>
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                  <div data-uid='eee' data-testid='eee'>baz</div>
                </>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <>
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                  <div data-uid='eee' data-testid='eee'>baz</div>
                  <div data-uid='ddd'>HELLO</div>
                </>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
      },
      {
        name: 'into empty fragment',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <React.Fragment />
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <React.Fragment>
                  <div data-uid='ddd'>HELLO</div>
                </React.Fragment>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
      },
      {
        name: 'into attribute (string)',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                "hey there"
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <>
                  hey there<div data-uid='ddd'>HELLO</div>
                </>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
      },
      {
        name: 'into attribute (number)',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                42
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <>
                  42<div data-uid='ddd'>HELLO</div>
                </>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
      },
      {
        name: 'into attribute (function)',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                (()=>"heheh")()
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <>
                  {(()=>"heheh")()}
                  <div data-uid='ddd'>HELLO</div>
                </>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
      },
      {
        name: 'into attribute (expression)',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                5 + 3
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
        paste: [makeElementPaste(makeDiv('ddd', 'HELLO'))],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <>
                  {5 + 3}
                  <div data-uid='ddd'>HELLO</div>
                </>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>bar</div>
              )
            }
          </div>
        `,
      },
      {
        name: 'multiple elements',
        code: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <div data-uid='bbb' data-testid='bbb'>bar</div>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>baz</div>
              )
            }
          </div>
        `,
        paste: [
          makeElementPaste(makeDiv('ddd', 'HELLO')),
          makeElementPaste(makeDiv('eee', 'THERE')),
        ],
        want: `
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <>
                  <div data-uid='bbb' data-testid='bbb'>bar</div>
                  <div data-uid='ddd'>HELLO</div>
                  <div data-uid='eee'>THERE</div>
                </>
              ) : (
                <div data-uid='ccc' data-testid='ccc'>baz</div>
              )
            }
          </div>
        `,
      },
    ]
    tests.forEach((t) => {
      it(`paste ${t.name}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(t.code),
          'await-first-dom-report',
        )

        const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

        await act(async () => {
          await renderResult.dispatch(
            [
              pasteJSXElements(
                targetPath,
                t.paste,
                renderResult.getEditorState().editor.jsxMetadata,
              ),
            ],
            true,
          )
        })

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(t.want),
        )
      })
    })
  })
})
