import { act, within } from '@testing-library/react'
import { forElementChildOptic } from '../../core/model/common-optics'
import { conditionalWhenTrueOptic, maybeConditionalExpression } from '../../core/model/conditionals'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { isRight } from '../../core/shared/either'
import * as EP from '../../core/shared/element-path'
import { isJSExpressionValue, isJSXConditionalExpression } from '../../core/shared/element-template'
import { filtered, fromField, fromTypeGuard } from '../../core/shared/optics/optic-creators'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import type { Optic } from '../../core/shared/optics/optics'
import { forceNotNull } from '../../core/shared/optional-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import { searchInFloatingMenu, selectComponentsForTest } from '../../utils/utils.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  TestScenePath,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { deleteSelected, selectComponents, unwrapElements } from '../editor/actions/action-creators'
import { ConditionalSectionTestId } from '../inspector/sections/layout-section/conditional-section'
import type { EditorStorePatched } from './store/editor-state'
import type { InsertionPath } from './store/insertion-path'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  replaceWithSingleElement,
} from './store/insertion-path'
import { MockClipboardHandlers, firePasteEvent, pressKey } from '../canvas/event-helpers.test-utils'
import { cmdModifier } from '../../utils/modifiers'
import { assertNever } from '../../core/shared/utils'

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
            true ? null : <>null</>
          }
          {
            // @utopia/uid=cond2
            true ? <span>hello</span> : null
          }
          {
            // @utopia/uid=cond3
            true ? null : <span>world</span>
          }
          {
            // @utopia/uid=cond4
            true ? 'hello' : <span>world</span>
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

        expect(emptyLabelsInConditionalInspector.length).toEqual(1)

        const fragmentLabelsInInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('Fragment')

        expect(fragmentLabelsInInspector.length).toEqual(1)
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

        const spanLabelsInInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('span')

        expect(spanLabelsInInspector.length).toEqual(1)
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

        const spanLabelsInInspector = within(
          editor.renderedDOM.getByTestId(ConditionalSectionTestId),
        ).queryAllByText('span')

        expect(spanLabelsInInspector.length).toEqual(1)
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
          true ? 'hello' : <div>'there'</div>
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const helloWorldUIDOptic: Optic<EditorStorePatched, string> = forElementChildOptic(
        EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional']),
      )
        .compose(fromTypeGuard(isJSXConditionalExpression))
        .compose(conditionalWhenTrueOptic)
        .compose(fromTypeGuard(isJSExpressionValue))
        .compose(filtered((value) => value.value === 'hello'))
        .compose(fromField('uid'))
      const helloWorldUID: string = unsafeGet(helloWorldUIDOptic, renderResult.getEditorState())

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
                true ? null : <div data-uid='33d'>'there'</div>
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
    xit('keeps the selection on the null branch (multiple targets)', async () => {
      const startSnippet = `
        <div data-uid='aaa' data-testid='aaa'>
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

      const divAAA = (await renderResult.renderedDOM.findByTestId('aaa')).getBoundingClientRect()
      const divFFF = (await renderResult.renderedDOM.findByTestId('fff')).getBoundingClientRect()
      // doing this dance because of possible font render discrepancies between local and CI
      expect(divFFF.height).toBeGreaterThanOrEqual(18)
      expect(divFFF.height).toBeLessThanOrEqual(20)
      expect(divFFF.width).toBeGreaterThanOrEqual(400)
      expect(divFFF.width).toBeLessThanOrEqual(401)
      const left = divFFF.left - divAAA.left
      const top = divFFF.top - divAAA.top

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa' data-testid='aaa'>
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
              <div data-uid='fff' data-testid='fff' style={{ width: ${divFFF.width}, height: ${divFFF.height}, position: 'absolute', left: ${left}, top: ${top} }} />
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

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

      await selectComponentsForTest(renderResult, [targetPath])
      await wrapInConditional(renderResult)

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
      await selectComponentsForTest(renderResult, [
        EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
        EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
      ])

      await wrapInConditional(renderResult)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                true ? (
                  <React.Fragment>
                    <div data-uid='bbb'>hello there</div>
                    <div data-uid='ccc'>another div</div>
                  </React.Fragment>
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

      const targetPath = forceNotNull(
        `Missing ${targetUID} element`,
        Object.keys(renderResult.getEditorState().editor.jsxMetadata).find((path) =>
          path.includes(targetUID),
        ),
      )

      await selectComponentsForTest(renderResult, [EP.fromString(targetPath)])
      await wrapInConditional(renderResult)

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
        await renderResult.dispatch([unwrapElements([targetPath])], true)
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
        await renderResult.dispatch([unwrapElements([targetPath])], true)
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
          true ? 'hello': <span>'bello'</span>
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

      await act(async () => {
        await renderResult.dispatch([unwrapElements([targetPath])], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>hello</div>
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
        await renderResult.dispatch([unwrapElements([targetPath])], true)
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
    it('can unwrap a conditional with expression', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          true ? 5 + 5 + 15: <div>'bello'</div>
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

      await act(async () => {
        await renderResult.dispatch([unwrapElements([targetPath])], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>{5 + 5 + 15}</div>
         `),
      )
    })
    it('can unwrap a nested conditional clause', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
          {
            // @utopia/uid=conditional
            true ? (
              // @utopia/uid=conditional-inner
              (true ? <div data-uid='bbb'>hello</div> : null)
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

      const targetPath = EP.appendNewElementPath(TestScenePath, [
        'aaa',
        'conditional',
        'conditional-inner',
      ])

      await act(async () => {
        await renderResult.dispatch([unwrapElements([targetPath])], true)
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='aaa'>
          {
            // @utopia/uid=conditional
            true ? <div data-uid='bbb'>hello</div> : 'bello'
          }
          </div>
         `),
      )
    })
  })
  describe('paste', () => {
    const clipboardMock = new MockClipboardHandlers().mock()

    async function runPaste({
      startSnippet,
      pasteInto,
      targets,
    }: {
      startSnippet: string
      pasteInto: InsertionPath
      targets: Array<ElementPath>
    }) {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      await selectComponentsForTest(renderResult, targets)

      await pressKey('c', { modifiers: cmdModifier })

      if (pasteInto.type === 'CHILD_INSERTION') {
        await selectComponentsForTest(renderResult, [pasteInto.intendedParentPath])
      } else if (pasteInto.type === 'CONDITIONAL_CLAUSE_INSERTION') {
        const conditional = maybeConditionalExpression(
          MetadataUtils.findElementByElementPath(
            renderResult.getEditorState().editor.jsxMetadata,
            pasteInto.intendedParentPath,
          ),
        )!

        const targetUid =
          pasteInto.clause === 'true-case'
            ? conditional.whenTrue.uid
            : pasteInto.clause === 'false-case'
            ? conditional.whenFalse.uid
            : assertNever(pasteInto.clause)

        const targetPath = EP.appendToPath(pasteInto.intendedParentPath, targetUid)
        await selectComponentsForTest(renderResult, [targetPath])
      } else {
        assertNever(pasteInto)
      }

      const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

      firePasteEvent(canvasRoot)

      // Wait for the next frame
      await clipboardMock.pasteDone
      await renderResult.getDispatchFollowUpActionsFinished()

      await pressKey('Esc')
      await renderResult.getDispatchFollowUpActionsFinished()

      return getPrintedUiJsCode(renderResult.getEditorState())
    }

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
            pasteInto: conditionalClauseInsertionPath(
              EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
              'true-case',
              replaceWithSingleElement(),
            ),
            targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? (
                    <div
                      data-uid='aad'
                      style={{
                        top: 0,
                        left: 0,
                        position: 'absolute',
                      }}
                    >
                      copy me
                    </div>
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
              <div data-uid='bbb' style={{ height: 20 }}>copy me</div>
              <div data-uid='ccc' style={{ height: 20 }}>another div</div>
              <div data-uid='ddd' style={{ height: 20 }}>yet another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: conditionalClauseInsertionPath(
              EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
              'true-case',
              replaceWithSingleElement(),
            ),
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
                <React.Fragment>
                  <div
                   data-uid='aaf'
                    style={{
                      height: 20,
                      top: 0,
                      left: 0,
                      position: 'absolute',
                    }}
                  >
                    copy me
                  </div>
                  <div
               data-uid='aal'
                    style={{
                 height: 20,
                 top: 20,
                      left: 0,
                      position: 'absolute',
                    }}
                  >
                    another div
                  </div>
                </React.Fragment>
              ) : null
            }
          <div data-uid='bbb' style={{ height: 20 }}>
            copy me
          </div>
          <div data-uid='ccc' style={{ height: 20 }}>
            another div
            </div>
            <div data-uid='ddd' style={{ height: 20 }}>
              yet another div
            </div>
          </div>
            `),
          )
        })
        it('can paste to children supporting element in branch', async () => {
          const startSnippet = `
            <div data-uid='aaa' style={{ lineHeight: '20px' }}>
              {
                // @utopia/uid=cond
                true ? <div data-uid='eee'>insert into this</div> : null
              }
              <div data-uid='bbb'>copy me</div>
              <div data-uid='ccc'>another div</div>
              <div data-uid='ddd'>yet another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: childInsertionPath(
              EP.appendNewElementPath(TestScenePath, ['aaa', 'cond', 'eee']),
            ),
            targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa' style={{ lineHeight: '20px' }}>
                {
                  // @utopia/uid=cond
                  true ? (
                    <div data-uid='eee'>
                      insert into this
                      <div data-uid='aad' style={{top: 20, left: 0, position: 'absolute'}}>copy me</div>
                    </div>
                  ) : null
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
            pasteInto: conditionalClauseInsertionPath(
              EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
              'false-case',
              replaceWithSingleElement(),
            ),
            targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? null : (
                    <div
                      data-uid='aad'
                      style={{
                        top: 0,
                        left: 0,
                        position: 'absolute',
                      }}
                    >
                      copy me
                    </div>
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
              <div data-uid='ddd' style={{ height: 10 }}>yet another div</div>
              <div data-uid='bbb' style={{ height: 10 }}>copy me</div>
              <div data-uid='ccc' style={{ height: 10 }}>another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: conditionalClauseInsertionPath(
              EP.appendNewElementPath(TestScenePath, ['aaa', 'cond']),
              'false-case',
              replaceWithSingleElement(),
            ),
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
                <React.Fragment>
                  <div
                    data-uid='aaf'
                    style={{
                      height: 10,
                      top: 0,
                      left: 0,
                      position: 'absolute',
                    }}
                  >
                    copy me
                  </div>
                  <div
                    data-uid='aal'
                    style={{
                      height: 10,
                      top: 10,
                      left: 0,
                      position: 'absolute',
                    }}
                  >
                    another div
                  </div>
                </React.Fragment>
              )
            }
            <div data-uid='ddd' style={{ height: 10 }}>
              yet another div
            </div>
            <div data-uid='bbb' style={{ height: 10 }}>
              copy me
            </div>
            <div data-uid='ccc' style={{ height: 10 }}>
              another div
            </div>
          </div>
            `),
          )
        })
        it('can paste to children supporting element in branch', async () => {
          const startSnippet = `
            <div data-uid='aaa'>
              {
                // @utopia/uid=cond
                true ? null : <div data-uid='eee'>insert into this</div>
              }
              <div data-uid='bbb'>copy me</div>
              <div data-uid='ccc'>another div</div>
              <div data-uid='ddd'>yet another div</div>
            </div>
          `

          const got = await runPaste({
            startSnippet,
            pasteInto: childInsertionPath(
              EP.appendNewElementPath(TestScenePath, ['aaa', 'cond', 'eee']),
            ),
            targets: [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])],
          })

          expect(got).toEqual(
            makeTestProjectCodeWithSnippet(`
              <div data-uid='aaa'>
                {
                  // @utopia/uid=cond
                  true ? null : (
                    <div data-uid='eee'>
                      insert into this
                      <div data-uid='aad' style={{top: 0, left: 0, position: 'absolute'}} >copy me</div>
                    </div>
                  )
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

  describe('canvas', () => {
    it('renders fine with nested arbitrary blocks and variables coming from different scopes', async () => {
      const editor = await renderTestEditorWithCode(
        `
        import * as React from 'react'
        import { Scene, Storyboard } from 'utopia-api'

        const a = 1

        export var storyboard = (
          <Storyboard data-uid='sb'>
            {[1].map((i) => (
              <div data-uid='cond'>
                {i > 0
                  ? [2].map((j) => (
                      <div>
                        {j > i ? (
                          <div>
                            {(() => {
                              const b = 5
                              return (
                                <div>
                                  {b + a > j + i ? (
                                    <div data-testid='find-me'>
                                      It works
                                    </div>
                                  ) : null}
                                </div>
                              )
                            })()}
                          </div>
                        ) : null}
                      </div>
                    ))
                  : null}
              </div>
            ))}
          </Storyboard>
        )
      `,
        'await-first-dom-report',
      )

      const innerMostDiv = editor.renderedDOM.getByTestId('find-me')
      expect(innerMostDiv.textContent?.trim()).toEqual('It works')
    })
  })
})

async function wrapInConditional(renderResult: EditorRenderResult) {
  await pressKey('w') // open the wrap menu
  await searchInFloatingMenu(renderResult, 'Condition')
}
