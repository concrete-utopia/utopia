import { fireEvent } from '@testing-library/react'
import { forElementOptic } from '../../core/model/common-optics'
import {
  conditionalWhenFalseOptic,
  jsxConditionalExpressionOptic,
} from '../../core/model/conditionals'
import { JSXElementChild } from '../../core/shared/element-template'
import { unsafeGet } from '../../core/shared/optics/optic-utilities'
import { Optic } from '../../core/shared/optics/optics'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import * as EP from '../../core/shared/element-path'
import { altCmdModifier, cmdModifier } from '../../utils/modifiers'
import { selectComponents } from '../editor/actions/meta-actions'
import { EditorState, navigatorEntryToKey } from '../editor/store/editor-state'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { mouseClickAtPoint, pressKey } from './event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  makeTestProjectCodeWithSnippetWithoutUIDs,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from './ui-jsx.test-utils'
import { expectNoAction, selectComponentsForTest, wait } from '../../utils/utils.test-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { ElementPath } from '../../core/shared/project-file-types'

async function openContextMenuAndClickOnItem(
  renderResult: EditorRenderResult,
  menuItemText: string,
) {
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
  fireEvent.contextMenu(canvasControlsLayer)
  await renderResult.getDispatchFollowUpActionsFinished()

  const contextMenuItem = await renderResult.renderedDOM.findByText(menuItemText)
  const contextMenuItemBounds = contextMenuItem.getBoundingClientRect()
  await mouseClickAtPoint(contextMenuItem, contextMenuItemBounds)
  await renderResult.getDispatchFollowUpActionsFinished()
}

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

type Trigger = (editor: EditorRenderResult) => Promise<void>
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

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)
    await renderResult.dispatch(selectComponents([target], false), true)

    // paste only layout properties
    await openContextMenuAndClickOnItem(renderResult, 'Paste Layout')

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ width: 200, height: 150, borderRadius: 5 }}
            data-uid='bbb'
          >paste</div>
          <div
            style={{ position: 'absolute', top: 20, opacity: 0.2, width: 200, height: 150 }}
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

    const target = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/ccc`)
    await renderResult.dispatch(selectComponents([target], false), true)

    // paste only layout properties
    await openContextMenuAndClickOnItem(renderResult, 'Paste Style')

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
          >hello</div>
        </div>`,
      ),
    )
  })

  describe('Bring to Front / Send to Back', () => {
    const testCaseElementInConditional: TemplatedTestWithTrigger = async (
      trigger: (editor: EditorRenderResult) => Promise<void>,
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
        await trigger(editor)
      })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)

      expectElementSelected(editor, targetPath)
    }

    describe('Bring Forward', () => {
      const BringForwardLabel = 'Bring Forward'

      const testCaseElementInBack: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='first'>First</span>
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
        await trigger(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='container'>
              <span data-uid='second'>Second</span>
              <span data-uid='first'>First</span>
              <span data-uid='third'>Third</span>
            </div>
        `),
        )

        expectElementSelected(editor, targetPath)
      }

      const testCaseElementOnTop: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
          <div data-uid='container'>
            <span data-uid='third'>Third</span>
            <span data-uid='second'>Second</span>
            <span data-uid='first'>First</span>
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
          await trigger(editor)
        })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)

        expectElementSelected(editor, targetPath)
      }

      describe('context menu', () => {
        const contextMenuTrigger = (e: EditorRenderResult) =>
          openContextMenuAndClickOnItem(e, BringForwardLabel)

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
        trigger: (editor: EditorRenderResult) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='first'>First</span>
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
          await trigger(editor)
        })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)

        expectElementSelected(editor, targetPath)
      }

      const testCaseElementOnTop: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='zero'>Zero</span>
          <span data-uid='first'>First</span>
          <span data-uid='second'>Second</span>
        </div>
        `),
          'await-first-dom-report',
        )

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/second`,
        )

        await selectComponentsForTest(editor, [targetPath])
        await trigger(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='zero'>Zero</span>
          <span data-uid='second'>Second</span>
          <span data-uid='first'>First</span>
        </div>
        `),
        )

        expectElementSelected(editor, targetPath)
      }

      describe('context menu', () => {
        it('clicking send backward on element that is already in the back', () =>
          expectTemplatedTestWithTrigger(testCaseElementInBack, (e) =>
            openContextMenuAndClickOnItem(e, SendBackwardLabel),
          ))

        it('clicking send backward on element that is on top', () =>
          expectTemplatedTestWithTrigger(testCaseElementOnTop, (e) =>
            openContextMenuAndClickOnItem(e, SendBackwardLabel),
          ))

        it('clicking send backward on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(testCaseElementInConditional, (e) =>
            openContextMenuAndClickOnItem(e, SendBackwardLabel),
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
        trigger: (editor: EditorRenderResult) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='zero'>Zero</span>
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
        await trigger(editor)

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='container'>
              <span data-uid='first'>First</span>
              <span data-uid='second'>Second</span>
              <span data-uid='zero'>Zero</span>
            </div>
        `),
        )

        expectElementSelected(editor, targetPath)
      }

      const testCaseElementInFront: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='first'>First</span>
          <span data-uid='second'>Second</span>
          <span data-uid='third'>Third</span>
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
          await trigger(editor)
        })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)

        expectElementSelected(editor, targetPath)
      }

      describe('context menu', () => {
        it('clicking bring to front on element that is already in the front', () =>
          expectTemplatedTestWithTrigger(testCaseElementInFront, (e) =>
            openContextMenuAndClickOnItem(e, BringToFrontLabel),
          ))

        it('clicking bring to front on element in the back', () =>
          expectTemplatedTestWithTrigger(testCaseElementInBack, (e) =>
            openContextMenuAndClickOnItem(e, BringToFrontLabel),
          ))

        it('clicking bring to front on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(testCaseElementInConditional, (e) =>
            openContextMenuAndClickOnItem(e, BringToFrontLabel),
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
        trigger: (editor: EditorRenderResult) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='first'>First</span>
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
          await trigger(editor)
        })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)
        expectElementSelected(editor, targetPath)
      }

      const testCaseElementInFront: TemplatedTestWithTrigger = async (
        trigger: (editor: EditorRenderResult) => Promise<void>,
      ) => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='zero'>Zero</span>
          <span data-uid='first'>First</span>
          <span data-uid='second'>Second</span>
        </div>
        `),
          'await-first-dom-report',
        )

        const targetPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/second`,
        )

        await selectComponentsForTest(editor, [targetPath])
        // await expectNoAction(editor, async () => {
        await trigger(editor)
        // })

        expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='container'>
          <span data-uid='second'>Second</span>
          <span data-uid='zero'>Zero</span>
          <span data-uid='first'>First</span>
        </div>
        `),
        )
        expectElementSelected(editor, targetPath)
      }

      describe('context menu', () => {
        it('clicking send to back on element that is already in the back', () =>
          expectTemplatedTestWithTrigger(testCaseElementInBack, (e) =>
            openContextMenuAndClickOnItem(e, SendToBackLabel),
          ))

        it('clicking send to back on element that is in the front', () =>
          expectTemplatedTestWithTrigger(testCaseElementInFront, (e) =>
            openContextMenuAndClickOnItem(e, SendToBackLabel),
          ))

        it('clicking send to back on element in a conditional branch', () =>
          expectTemplatedTestWithTrigger(testCaseElementInConditional, (e) =>
            openContextMenuAndClickOnItem(e, SendToBackLabel),
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

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
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

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/duck',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/element', // moved above duck
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/mallard',
      ])
      expectElementSelected(editor, targetPath)

      await pressKey('[', { modifiers: cmdModifier }) // Send Backward

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/element', // moved below duck, in its original place
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/duck',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/mallard',
      ])
      expectElementSelected(editor, targetPath)

      await pressKey(']', { modifiers: altCmdModifier }) // Bring To Front

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/duck',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/mallard',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/element', // moved above mallard and duck, to the front
      ])
      expectElementSelected(editor, targetPath)

      await pressKey('[', { modifiers: altCmdModifier }) // Send To Back

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
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
    it('wrap in div works inside a conditional on an expression', async () => {
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
      const inactiveElementOptic = forElementOptic(conditionalPath)
        .compose(jsxConditionalExpressionOptic)
        .compose(conditionalWhenFalseOptic)
      const inactiveElement = unsafeGet(inactiveElementOptic, renderResult.getEditorState().editor)
      const testValuePath = EP.appendToPath(conditionalPath, inactiveElement.uid)

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a div.
      await openContextMenuAndClickOnItem(renderResult, 'Wrap in div')

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
    it('wrap in div works inside a conditional on an element', async () => {
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

      const testValuePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/conditional/then-div`,
      )

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a div.
      await openContextMenuAndClickOnItem(renderResult, 'Wrap in div')

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
             {
               // @utopia/uid=conditional
               [].length === 0 ? (
                 <div style={{ position: 'absolute'}}>
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
                 </div>
               ) : (
                'Test' 
               )
             }
           </div>`,
        ),
      )
    })
    it('wrap in div works on an element', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...props.style }} data-uid='aaa'>
             <div
               style={{
                 height: 150,
                 width: 150,
                 position: 'absolute',
                 left: 154,
                 top: 134,
                 backgroundColor: 'lightblue',
               }}
               data-uid='target-div'
               data-testid='target-div'
             />
           </div>`,
        ),
        'await-first-dom-report',
      )

      const testValuePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:aaa/target-div`,
      )

      await renderResult.dispatch(selectComponents([testValuePath], false), true)

      // Wrap it in a div.
      await openContextMenuAndClickOnItem(renderResult, 'Wrap in div')

      expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippetWithoutUIDs(
          `<div style={{ ...props.style }}>
             <div style={{ position: 'absolute'}}>
               <div
                 style={{
                   height: 150,
                   width: 150,
                   position: 'absolute',
                   left: 154,
                   top: 134,
                   backgroundColor: 'lightblue',
                 }}
                 data-testid='target-div'
               />
             </div>
           </div>`,
        ),
      )
    })
  })
})
