/* eslint-disable jest/expect-expect */
import { act, within } from '@testing-library/react'
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
  jsxConditionalExpression,
} from '../../core/shared/element-template'
import { filtered, fromField, fromTypeGuard } from '../../core/shared/optics/optic-creators'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import { Optic, compose6Optics } from '../../core/shared/optics/optics'
import { forceNotNull } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import {
  TestScenePath,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import {
  deleteSelected,
  pasteJSXElements,
  selectComponents,
  unwrapElement,
  wrapInElement,
} from '../editor/actions/action-creators'
import { ConditionalSectionTestId } from '../inspector/sections/layout-section/conditional-section'
import { ElementPaste } from './action-types'
import { getElementFromRenderResult } from './actions/actions.test-utils'
import { EditorState } from './store/editor-state'
import { ReparentTargetParent } from './store/insertion-path'

describe('conditionals', () => {
  describe('inspector', () => {
    it('labels for empty', async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
      import { Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          {
            // @utopia/uid=cond1
            true ? null : null
          }
          {
            // @utopia/uid=cond2
            true ? 'hello' : null
          }
          {
            // @utopia/uid=cond3
            true ? null : 'world'
          }
          {
            // @utopia/uid=cond4
            true ? 'hello' : 'world'
          }
          </Storyboard>
      )
      `,
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [EP.fromString('sb/cond1')])
      {
        const emptyLabelsInConditionalInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('Empty')

        expect(emptyLabelsInConditionalInspector.length).toEqual(2)

        const helloLabelsInInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('hello')

        expect(helloLabelsInInspector.length).toEqual(0)
      }

      await selectComponentsForTest(editor, [EP.fromString('sb/cond2')])
      {
        const emptyLabelsInConditionalInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('Empty')

        expect(emptyLabelsInConditionalInspector.length).toEqual(1)

        const helloLabelsInInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('hello')

        expect(helloLabelsInInspector.length).toEqual(1)
      }

      await selectComponentsForTest(editor, [EP.fromString('sb/cond3')])
      {
        const emptyLabelsInConditionalInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('Empty')

        expect(emptyLabelsInConditionalInspector.length).toEqual(1)

        const worldLabelsInInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('world')

        expect(worldLabelsInInspector.length).toEqual(1)
      }

      await selectComponentsForTest(editor, [EP.fromString('sb/cond4')])
      {
        const emptyLabelsInConditionalInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('Empty')

        expect(emptyLabelsInConditionalInspector.length).toEqual(0)

        const helloLabelsInInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('hello')

        expect(helloLabelsInInspector.length).toEqual(1)

        const worldLabelsInInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('world')

        expect(worldLabelsInInspector.length).toEqual(1)
      }
    })
  })
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
  describe('wrap', () => {
    it('can wrap a single element in a conditional', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
          <div data-uid='bbb'>hello there</div>
          <div data-uid='ccc'>another div</div>
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const conditional = jsxConditionalExpression(
        'cond',
        jsExpressionValue(true, emptyComments),
        'true',
        jsExpressionValue(null, emptyComments),
        jsExpressionValue(null, emptyComments),
        emptyComments,
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

      await act(async () => {
        await renderResult.dispatch(
          [wrapInElement([targetPath], { element: conditional, importsToAdd: {} })],
          true,
        )
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                true ? <div data-uid='bbb'>hello there</div> : null
              }
              <div data-uid='ccc'>another div</div>
            </div>
         `),
      )
    })
    it('can wrap a multiple elements in a conditional', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
          <div data-uid='bbb'>hello there</div>
          <div data-uid='ccc'>another div</div>
          <div data-uid='ddd'>yet another one</div>
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const conditional = jsxConditionalExpression(
        'cond',
        jsExpressionValue(true, emptyComments),
        'true',
        jsExpressionValue(null, emptyComments),
        jsExpressionValue(null, emptyComments),
        emptyComments,
      )

      const targetPaths = [
        EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
        EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
      ]

      await act(async () => {
        await renderResult.dispatch(
          [wrapInElement(targetPaths, { element: conditional, importsToAdd: {} })],
          true,
        )
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                true ? (
                  <>
                    <div data-uid='bbb'>hello there</div>
                    <div data-uid='ccc'>another div</div>
                  </>
                ) : null
              }
              <div data-uid='ddd'>yet another one</div>
            </div>
         `),
      )
    })
    it('can wrap a conditional clause element in a conditional', async () => {
      const targetUID = 'bbb'
      const startSnippet = `
        <div data-uid='aaa'>
          {
            true ? <div data-uid='${targetUID}'>hello there</div> : <div data-uid='ccc'>another div</div>
          }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const conditional = jsxConditionalExpression(
        'cond',
        jsExpressionValue(true, emptyComments),
        'true',
        jsExpressionValue(null, emptyComments),
        jsExpressionValue(null, emptyComments),
        emptyComments,
      )

      const targetPath = forceNotNull(
        `Missing ${targetUID} element`,
        Object.keys(renderResult.getEditorState().editor.jsxMetadata).find((path) =>
          path.includes(targetUID),
        ),
      )

      await act(async () => {
        await renderResult.dispatch(
          [wrapInElement([EP.fromString(targetPath)], { element: conditional, importsToAdd: {} })],
          true,
        )
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {true ? (
                true ? (
                  <div data-uid='${targetUID}'>hello there</div>
                ) : null
              ) : (
                <div data-uid='ccc'>another div</div>
              )}
            </div>
         `),
      )
    })
  })
  describe('unwrap', () => {
    it('can unwrap a conditional', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
          {
            // @utopia/uid=conditional
            true ? <div data-uid='bbb'>hello there</div> : <div data-uid='ccc'>another div</div>
          }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

      await act(async () => {
        await renderResult.dispatch([unwrapElement(targetPath)], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              <div data-uid='bbb'>hello there</div>
            </div>
         `),
      )
    })
    it('can unwrap a conditional that is pinned to false', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          // @utopia/conditional=false
          true ? (
            <div data-uid='bbb' data-testid='bbb'>hello</div>
          ) : (
            <div data-uid='ccc' data-testid='ccc'>bello</div>
          )
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

      await act(async () => {
        await renderResult.dispatch([unwrapElement(targetPath)], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
            <div data-uid='ccc' data-testid='ccc'>bello</div>
            </div>
         `),
      )
    })
    it('can unwrap a conditional with only text', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          true ? 'hello': 'bello'
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

      await act(async () => {
        await renderResult.dispatch([unwrapElement(targetPath)], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {'hello'}
            </div>
         `),
      )
    })
    it('can unwrap a conditional clause', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
          {
            // @utopia/uid=conditional
            true ? (
              <div data-uid='bbb' data-testid='bbb'>
                <span data-uid='ccc'>hello</span>
              </div>
            ) : (
              'bello'
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
        await renderResult.dispatch([unwrapElement(targetPath)], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              true ? (
                <span data-uid='ccc'>hello</span>
              ) : (
                'bello'
              )
            }
          </div>
         `),
      )
    })
  })
  describe('paste', () => {
    it('can paste a single element into a conditional', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
          {
            // @utopia/uid=cond
            true ? null : null
          }
          <div data-uid='bbb'>copy me</div>
          <div data-uid='ccc'>another div</div>
        </div>
      `

      const got = await runPaste({
        startSnippet,
        pasteInto: EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
        targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
      })

      expect(got).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa'>
            {
              // @utopia/uid=cond
              true ? (
                <div data-uid='aab' style={{ display: 'block' }}>copy me</div>
              ) : null
            }
            <div data-uid='bbb'>copy me</div>
            <div data-uid='ccc'>another div</div>
          </div>
         `),
      )
    })
    it('can paste multiple elements into a conditional', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
          {
            // @utopia/uid=cond
            true ? null : null
          }
          <div data-uid='bbb'>copy me</div>
          <div data-uid='ccc'>another div</div>
          <div data-uid='ddd'>yet another div</div>
        </div>
      `

      const got = await runPaste({
        startSnippet,
        pasteInto: EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
        targets: [
          EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
          EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
        ],
      })

      expect(got).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa'>
            {
              // @utopia/uid=cond
              true ? (
                <>
                  <div data-uid='aac'>copy me</div>
                  <div data-uid='aab' style={{ display: 'block' }}>another div</div>
                </>
              ) : null
            }
            <div data-uid='bbb'>copy me</div>
            <div data-uid='ccc'>another div</div>
            <div data-uid='ddd'>yet another div</div>
          </div>
         `),
      )
    })
    describe('branches', () => {
      describe('true branch', () => {
        it('pastes a single element', async () => {
          const startSnippet = `
            <div data-uid='aaa'>
              {
                // @utopia/uid=cond
                true ? null : null
              }
              <div data-uid='bbb'>copy me</div>
              <div data-uid='ccc'>another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: {
              clause: 'true-case',
              elementPath: EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
            },
            targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? (
                    <div data-uid='aab'>copy me</div>
                  ) : null
                }
                <div data-uid='bbb'>copy me</div>
                <div data-uid='ccc'>another div</div>
              </div>
            `),
          )
        })
        it('pastes multiple elements', async () => {
          const startSnippet = `
            <div data-uid='aaa'>
              {
                // @utopia/uid=cond
                true ? null : null
              }
              <div data-uid='bbb'>copy me</div>
              <div data-uid='ccc'>another div</div>
              <div data-uid='ddd'>yet another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: {
              clause: 'true-case',
              elementPath: EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
            },
            targets: [
              EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
              EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
            ],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? (
                    <>
                      <div data-uid='aab'>copy me</div>
                      <div data-uid='aac'>another div</div>
                    </>
                  ) : null
                }
                <div data-uid='bbb'>copy me</div>
                <div data-uid='ccc'>another div</div>
                <div data-uid='ddd'>yet another div</div>
              </div>
            `),
          )
        })
        it('cannot paste when the branch is not empty', async () => {
          const startSnippet = `
            <div data-uid='aaa'>
              {
                // @utopia/uid=cond
                true ? <div data-uid='eee'>stop right there</div> : null
              }
              <div data-uid='bbb'>copy me</div>
              <div data-uid='ccc'>another div</div>
              <div data-uid='ddd'>yet another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: {
              clause: 'true-case',
              elementPath: EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
            },
            targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? <div data-uid='eee'>stop right there</div> : null
                }
                <div data-uid='bbb'>copy me</div>
                <div data-uid='ccc'>another div</div>
                <div data-uid='ddd'>yet another div</div>
              </div>
            `),
          )
        })
      })
      describe('false branch', () => {
        it('pastes a single element', async () => {
          const startSnippet = `
            <div data-uid='aaa'>
              {
                // @utopia/uid=cond
                true ? null : null
              }
              <div data-uid='bbb'>copy me</div>
              <div data-uid='ccc'>another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: {
              clause: 'false-case',
              elementPath: EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
            },
            targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? null : (
                    <div data-uid='aab'>copy me</div>
                  )
                }
                <div data-uid='bbb'>copy me</div>
                <div data-uid='ccc'>another div</div>
              </div>
            `),
          )
        })
        it('pastes multiple elements', async () => {
          const startSnippet = `
            <div data-uid='aaa'>
              {
                // @utopia/uid=cond
                true ? null : null
              }
              <div data-uid='bbb'>copy me</div>
              <div data-uid='ccc'>another div</div>
              <div data-uid='ddd'>yet another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: {
              clause: 'false-case',
              elementPath: EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
            },
            targets: [
              EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
              EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
            ],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? null : (
                    <>
                      <div data-uid='aab'>copy me</div>
                      <div data-uid='aac'>another div</div>
                    </>
                  )
                }
                <div data-uid='bbb'>copy me</div>
                <div data-uid='ccc'>another div</div>
                <div data-uid='ddd'>yet another div</div>
              </div>
            `),
          )
        })
        it('cannot paste when the branch is not empty', async () => {
          const startSnippet = `
            <div data-uid='aaa'>
              {
                // @utopia/uid=cond
                true ? null : <div data-uid='eee'>stop right there</div>
              }
              <div data-uid='bbb'>copy me</div>
              <div data-uid='ccc'>another div</div>
              <div data-uid='ddd'>yet another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: {
              clause: 'false-case',
              elementPath: EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
            },
            targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? null : <div data-uid='eee'>stop right there</div>
                }
                <div data-uid='bbb'>copy me</div>
                <div data-uid='ccc'>another div</div>
                <div data-uid='ddd'>yet another div</div>
              </div>
            `),
          )
        })
      })
    })
  })
})

async function runPaste({
  startSnippet,
  pasteInto,
  targets,
}: {
  startSnippet: string
  pasteInto: ReparentTargetParent<ElementPath>
  targets: Array<ElementPath>
}) {
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(startSnippet),
    'await-first-dom-report',
  )

  const elements: Array<ElementPaste> = targets.map((target) => {
    return {
      element: getElementFromRenderResult(renderResult, target),
      originalElementPath: target,
      importsToAdd: {},
    }
  })

  await act(async () => {
    await renderResult.dispatch([pasteJSXElements(pasteInto, elements, {})], true)
  })

  return getPrintedUiJsCode(renderResult.getEditorState())
}
