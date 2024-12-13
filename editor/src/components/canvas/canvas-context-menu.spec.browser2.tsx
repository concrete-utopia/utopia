import { forElementChildOptic } from '../../core/model/common-optics'
import {
  conditionalWhenFalseOptic,
  jsxConditionalExpressionOptic,
} from '../../core/model/conditionals'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { altCmdModifier, cmdModifier } from '../../utils/modifiers'
import { selectComponents } from '../editor/actions/meta-actions'
import { navigatorEntryToKey } from '../editor/store/editor-state'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  openContextMenuAndClickOnItem,
  pressKey,
} from './event-helpers.test-utils'
import type { EditorRenderResult } from './ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  makeTestProjectCodeWithSnippetWithoutUIDs,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from './ui-jsx.test-utils'
import {
  expectNoAction,
  searchInComponentPicker,
  selectComponentsForTest,
} from '../../utils/utils.test-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import { getNavigatorTargetsFromEditorState } from '../navigator/navigator-utils'

function expectAllSelectedViewsToHaveMetadata(editor: EditorRenderResult) {
  const selectedViews = editor.getEditorState().editor.selectedViews

  expect(selectedViews.length > 0).toEqual(true)

  expect(
    editor
      .getEditorState()
      .editor.selectedViews.every(
        (path) =>
          MetadataUtils.findElementByElementPath(
            editor.getEditorState().editor.jsxMetadata,
            path,
          ) != null,
      ),
  ).toEqual(true)
}

function expectElementSelected(editor: EditorRenderResult, path: ElementPath) {
  expect(editor.getEditorState().editor.selectedViews.find((p) => EP.pathsEqual(p, path))).toEqual(
    path,
  )
}

type Trigger = (editor: EditorRenderResult, testid: string) => Promise<void>
type TemplatedTestWithTrigger = (trigger: Trigger) => Promise<void>

const expectTemplatedTestWithTrigger = async (
  testWithTrigger: TemplatedTestWithTrigger,
  trigger: Trigger,
) => testWithTrigger(trigger)

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectNoAction", "expectTemplatedTestWithTrigger"] }] */

describe('canvas context menu', () => {
  it('clicking on paste layout menu item pastes layout properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, height: 150, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.2 }}
            data-uid='ccc'
            data-testid='ccc'
          >hello</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const copyPropertiesFrom = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
    )
    await renderResult.dispatch(selectComponents([copyPropertiesFrom], false), true)

    // copy properties first
    await pressKey('c', { modifiers: altCmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const element = renderResult.renderedDOM.getByTestId('ccc')
    const elementCenter = getDomRectCenter(element.getBoundingClientRect())
    await mouseClickAtPoint(canvasControlsLayer, elementCenter)
    await renderResult.getDispatchFollowUpActionsFinished()

    // paste only layout properties
    await openContextMenuAndClickOnItem(
      renderResult,
      canvasControlsLayer,
      elementCenter,
      'Paste Layout',
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, height: 150, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ opacity: 0.2, width: 200, height: 150 }}
            data-uid='ccc'
            data-testid='ccc'
          >hello</div>
        </div>`,
      ),
    )
  })
  it('clicking on paste style menu item pastes style properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.2, color: 'hotpink' }}
            data-uid='ccc'
            data-testid='ccc'
          >hello</div>
        </div>`,
      ),
      'await-first-dom-report',
    )

    const copyPropertiesFrom = EP.fromString(
      `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/bbb`,
    )
    await renderResult.dispatch(selectComponents([copyPropertiesFrom], false), true)

    // copy properties first
    await pressKey('c', { modifiers: altCmdModifier })
    await renderResult.getDispatchFollowUpActionsFinished()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const element = renderResult.renderedDOM.getByTestId('ccc')
    const elementCenter = getDomRectCenter(element.getBoundingClientRect())
    await mouseClickAtPoint(canvasControlsLayer, elementCenter)
    await renderResult.getDispatchFollowUpActionsFinished()

    // paste only style properties
    await openContextMenuAndClickOnItem(
      renderResult,
      canvasControlsLayer,
      elementCenter,
      'Paste Style',
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.5, fontSize: 20, borderRadius: 5 }}
            data-uid='ccc'
            data-testid='ccc'
          >hello</div>
        </div>`,
      ),
    )
  })

  describe('Bring to Front / Send to Back', () => {
    const testCaseElementInConditional: TemplatedTestWithTrigger = async (
      trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
    ) => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
      <div data-uid='container'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? (
            <div
              style={{
              height: 150,
                width: 150,
                position: 'absolute',
                left: 154,
                top: 134,
                backgroundColor: 'lightblue',
              }}
              data-uid='then-div'
              data-testid='then-div'
            />
          ) : 'Test' 
        }
        </div>
      `),
        'await-first-dom-report',
      )

      const initialEditor = getPrintedUiJsCode(editor.getEditorState())

      const targetPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/conditional/then-div`,
      )

      await selectComponentsForTest(editor, [targetPath])

      // to ensure that the selected element is actually an element in the project
      expectAllSelectedViewsToHaveMetadata(editor)
      await expectNoAction(editor, async () => {
        await trigger(editor, 'then-div')
      })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)

      expectElementSelected(editor, targetPath)
    }

    describe('Bring Forward', () => {
      const BringForwardLabel = 'Bring Forward'

      const testCaseElementInBack: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='first' data-testid='first'>First</span>
          <span data-uid='second'>Second</span>
          <span data-uid='third'>Third</span>
          </div>
        `),
          'await-first-dom-report',
        )

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/first`,
        )

        await selectComponentsForTest(editor, [targetPath])
        await trigger(editor, 'first')

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='container'>
              <span data-uid='second'>Second</span>
              <span data-uid='first' data-testid='first'>First</span>
              <span data-uid='third'>Third</span>
            </div>
        `),
        )

        expectElementSelected(editor, targetPath)
      }

      const testCaseElementOnTop: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='container'>
            <span data-uid='third'>Third</span>
            <span data-uid='second'>Second</span>
            <span data-uid='first' data-testid='first'>First</span>
          </div>
          `),
          'await-first-dom-report',
        )

        const initialEditor = getPrintedUiJsCode(editor.getEditorState())

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/first`,
        )

        await selectComponentsForTest(editor, [targetPath])

        // to ensure that the selected element is actually an element in the project
        expectAllSelectedViewsToHaveMetadata(editor)
        await expectNoAction(editor, async () => {
          await trigger(editor, 'first')
        })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)

        expectElementSelected(editor, targetPath)
      }

      describe('context menu', () => {
        const contextMenuTrigger = async (e: EditorRenderResult, testid: string) => {
          const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
          const element = e.renderedDOM.getByTestId(testid)
          const elementCenter = getDomRectCenter(element.getBoundingClientRect())

          await openContextMenuAndClickOnItem(
            e,
            canvasControlsLayer,
            elementCenter,
            BringForwardLabel,
          )
          await e.getDispatchFollowUpActionsFinished()
        }

        it('clicking bring forward on element that is in the back', () =>
          expectTemplatedTestWithTrigger(testCaseElementInBack, contextMenuTrigger))
        it('clicking bring forward on element that is already on top', () =>
          expectTemplatedTestWithTrigger(testCaseElementOnTop, contextMenuTrigger))

        it('clicking bring forward on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(testCaseElementInConditional, contextMenuTrigger))
      })

      describe('shortcut', () => {
        const shortcutTrigger = () => pressKey(']', { modifiers: cmdModifier })
        it('clicking bring forward on element that is in the back', () =>
          expectTemplatedTestWithTrigger(testCaseElementInBack, shortcutTrigger))

        it('clicking bring forward on element that is already on top', () =>
          expectTemplatedTestWithTrigger(testCaseElementOnTop, shortcutTrigger))

        it('clicking bring forward on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(testCaseElementInConditional, shortcutTrigger))
      })
    })

    describe('Send Backward', () => {
      const SendBackwardLabel = 'Send Backward'

      const testCaseElementInBack: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='first' data-testid='first'>First</span>
          <span data-uid='second'>Second</span>
          <span data-uid='third'>Third</span>
        </div>
        `),
          'await-first-dom-report',
        )

        const initialEditor = getPrintedUiJsCode(editor.getEditorState())

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/first`,
        )

        await selectComponentsForTest(editor, [targetPath])

        // to ensure that the selected element is actually an element in the project
        expectAllSelectedViewsToHaveMetadata(editor)
        await expectNoAction(editor, async () => {
          await trigger(editor, 'first')
        })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)

        expectElementSelected(editor, targetPath)
      }

      const testCaseElementOnTop: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='zero'>Zero</span>
          <span data-uid='first'>First</span>
          <span data-uid='second' data-testid='second'>Second</span>
        </div>
        `),
          'await-first-dom-report',
        )

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/second`,
        )

        await selectComponentsForTest(editor, [targetPath])
        await trigger(editor, 'second')

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='zero'>Zero</span>
          <span data-uid='second' data-testid='second'>Second</span>
          <span data-uid='first'>First</span>
        </div>
        `),
        )

        expectElementSelected(editor, targetPath)
      }

      describe('context menu', () => {
        it('clicking send backward on element that is already in the back', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementInBack,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                SendBackwardLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))

        it('clicking send backward on element that is on top', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementOnTop,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                SendBackwardLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))

        it('clicking send backward on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementInConditional,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                SendBackwardLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))
      })

      describe('shortcut', () => {
        it('clicking send backward on element that is already in the back', () =>
          expectTemplatedTestWithTrigger(testCaseElementInBack, () =>
            pressKey('[', { modifiers: cmdModifier }),
          ))

        it('clicking send backward on element that on top', () =>
          expectTemplatedTestWithTrigger(testCaseElementOnTop, () =>
            pressKey('[', { modifiers: cmdModifier }),
          ))

        it('clicking send backward on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(testCaseElementInConditional, () =>
            pressKey('[', { modifiers: cmdModifier }),
          ))
      })
    })

    describe('Bring To Front', () => {
      const BringToFrontLabel = 'Bring To Front'

      const testCaseElementInBack: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='zero' data-testid='zero'>Zero</span>
          <span data-uid='first'>First</span>
          <span data-uid='second'>Second</span>
        </div>
        `),
          'await-first-dom-report',
        )

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/zero`,
        )

        await selectComponentsForTest(editor, [targetPath])
        await trigger(editor, 'zero')

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='container'>
              <span data-uid='first'>First</span>
              <span data-uid='second'>Second</span>
              <span data-uid='zero' data-testid='zero'>Zero</span>
            </div>
        `),
        )

        expectElementSelected(editor, targetPath)
      }

      const testCaseElementInFront: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='first'>First</span>
          <span data-uid='second'>Second</span>
          <span data-uid='third' data-testid='third'>Third</span>
        </div>
        `),
          'await-first-dom-report',
        )

        const initialEditor = getPrintedUiJsCode(editor.getEditorState())

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/third`,
        )

        await selectComponentsForTest(editor, [targetPath])

        // to ensure that the selected element is actually an element in the project
        expectAllSelectedViewsToHaveMetadata(editor)
        await expectNoAction(editor, async () => {
          await trigger(editor, 'third')
        })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)

        expectElementSelected(editor, targetPath)
      }

      describe('context menu', () => {
        it('clicking bring to front on element that is already in the front', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementInFront,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                BringToFrontLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))

        it('clicking bring to front on element in the back', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementInBack,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                BringToFrontLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))

        it('clicking bring to front on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementInConditional,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                BringToFrontLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))
      })

      describe('shortcut', () => {
        it('clicking bring to front on element that is already in the front', () =>
          expectTemplatedTestWithTrigger(testCaseElementInFront, () =>
            pressKey(']', { modifiers: altCmdModifier }),
          ))

        it('clicking bring to front on element in the back', () =>
          expectTemplatedTestWithTrigger(testCaseElementInBack, () =>
            pressKey(']', { modifiers: altCmdModifier }),
          ))

        it('clicking bring to front on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(testCaseElementInConditional, () =>
            pressKey(']', { modifiers: altCmdModifier }),
          ))
      })
    })

    describe('Send To Back', () => {
      const SendToBackLabel = 'Send To Back'

      const testCaseElementInBack: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='first' data-testid='first'>First</span>
          <span data-uid='second'>Second</span>
          <span data-uid='third'>Third</span>
        </div>
        `),
          'await-first-dom-report',
        )

        const initialEditor = getPrintedUiJsCode(editor.getEditorState())

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/first`,
        )

        await selectComponentsForTest(editor, [targetPath])

        // to ensure that the selected element is actually an element in the project
        expectAllSelectedViewsToHaveMetadata(editor)
        await expectNoAction(editor, async () => {
          await trigger(editor, 'first')
        })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)
        expectElementSelected(editor, targetPath)
      }

      const testCaseElementInFront: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult, testid: string) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='zero'>Zero</span>
          <span data-uid='first'>First</span>
          <span data-uid='second' data-testid='second'>Second</span>
        </div>
        `),
          'await-first-dom-report',
        )

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/second`,
        )

        await selectComponentsForTest(editor, [targetPath])
        // await expectNoAction(editor, async () => {
        await trigger(editor, 'second')
        // })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='second' data-testid='second'>Second</span>
          <span data-uid='zero'>Zero</span>
          <span data-uid='first'>First</span>
        </div>
        `),
        )
        expectElementSelected(editor, targetPath)
      }

      describe('context menu', () => {
        it('clicking send to back on element that is already in the back', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementInBack,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                SendToBackLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))

        it('clicking send to back on element that is in the front', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementInFront,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                SendToBackLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))

        it('clicking send to back on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(
            testCaseElementInConditional,
            async (e: EditorRenderResult, testid: string) => {
              const canvasControlsLayer = e.renderedDOM.getByTestId(CanvasControlsContainerID)
              const element = e.renderedDOM.getByTestId(testid)
              const elementBounds = element.getBoundingClientRect()
              await openContextMenuAndClickOnItem(
                e,
                canvasControlsLayer,
                elementBounds,
                SendToBackLabel,
              )
              await e.getDispatchFollowUpActionsFinished()
            },
          ))
      })

      describe('shortcut', () => {
        it('clicking send to back on element that is already in the back', () =>
          expectTemplatedTestWithTrigger(testCaseElementInBack, () =>
            pressKey('[', { modifiers: altCmdModifier }),
          ))

        it('clicking send to back on element that is in the front', () =>
          expectTemplatedTestWithTrigger(testCaseElementInFront, () =>
            pressKey('[', { modifiers: altCmdModifier }),
          ))

        it('clicking send to back on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(testCaseElementInConditional, () =>
            pressKey('[', { modifiers: altCmdModifier }),
          ))
      })
    })

    it('Bring forward / send to back in a flex container', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 427,
          top: 128,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'column',
          gap: 58.5,
          padding: '36px 57px',
        }}
        data-uid='container'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 78,
            height: 101,
            contain: 'layout',
          }}
          data-uid='element'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 66,
            height: 81,
            contain: 'layout',
          }}
          data-uid='duck'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 81,
            height: 35,
            contain: 'layout',
          }}
          data-uid='mallard'
        />
      </div>
      `),
        'await-first-dom-report',
      )

      expect(
        getNavigatorTargetsFromEditorState(editor.getEditorState().editor).navigatorTargets.map(
          navigatorEntryToKey,
        ),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/element',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/duck',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/mallard',
      ])

      const targetPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/element`,
      )

      await selectComponentsForTest(editor, [targetPath])

      await pressKey(']', { modifiers: cmdModifier }) // Bring Forward

      expect(
        getNavigatorTargetsFromEditorState(editor.getEditorState().editor).navigatorTargets.map(
          navigatorEntryToKey,
        ),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/duck',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/element', // moved above duck
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/mallard',
      ])
      expectElementSelected(editor, targetPath)

      await pressKey('[', { modifiers: cmdModifier }) // Send Backward

      expect(
        getNavigatorTargetsFromEditorState(editor.getEditorState().editor).navigatorTargets.map(
          navigatorEntryToKey,
        ),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/element', // moved below duck, in its original place
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/duck',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/mallard',
      ])
      expectElementSelected(editor, targetPath)

      await pressKey(']', { modifiers: altCmdModifier }) // Bring To Front

      expect(
        getNavigatorTargetsFromEditorState(editor.getEditorState().editor).navigatorTargets.map(
          navigatorEntryToKey,
        ),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/duck',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/mallard',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/element', // moved above mallard and duck, to the front
      ])
      expectElementSelected(editor, targetPath)

      await pressKey('[', { modifiers: altCmdModifier }) // Send To Back

      expect(
        getNavigatorTargetsFromEditorState(editor.getEditorState().editor).navigatorTargets.map(
          navigatorEntryToKey,
        ),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/element', // moved below mallard and duck, to the back
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/duck',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/mallard',
      ])
      expectElementSelected(editor, targetPath)
    })
  })

  describe('wrap in from contextmenu', () => {
    xit('wrap in div works inside a conditional on an expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div
                   style={{
                    height: 150,
                     width: 150,
                     position: 'absolute',
                     left: 154,
                     top: 134,
                     backgroundColor: 'lightblue',
                   }}
                   data-uid='then-div'
                   data-testid='then-div'
                 />
               ) : 'Test' 
             }
           </div>`,
        ),
        'await-first-dom-report',
      )

      const conditionalPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional`,
      )
      const inactiveElementOptic = forElementChildOptic(conditionalPath)
        .compose(jsxConditionalExpressionOptic)
        .compose(conditionalWhenFalseOptic)
      const inactiveElement = unsafeGet(inactiveElementOptic, renderResult.getEditorState())
      const testValuePath = EP.appendToPath(conditionalPath, inactiveElement.uid)

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      await wrapInElement(renderResult, 'div')
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div
                   style={{
                    height: 150,
                     width: 150,
                     position: 'absolute',
                     left: 154,
                     top: 134,
                     backgroundColor: 'lightblue',
                   }}
                   data-testid='then-div'
                 />
               ) : (
                  <div style={{ position: 'absolute'}}>
                   {'Test'}
                 </div>
               )
             }
           </div>`,
        ),
      )
    })
  })
})

async function wrapInElement(renderResult: EditorRenderResult, query: string) {
  await pressKey('w') // open the wrap menu
  await searchInComponentPicker(renderResult, query)
}
