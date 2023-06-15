import { act, fireEvent } from '@testing-library/react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { isInfinityRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import {
  expectSingleUndo2Saves,
  selectComponentsForTest,
  setFeatureForBrowserTests,
} from '../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, mouseDoubleClickAtPoint } from '../../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  formatTestProjectCode,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/meta-actions'
import { FlexDirection } from '../common/css-utils'
import {
  FillContainerLabel,
  FillFixedHugControlId,
  FixedLabel,
  HugContentsLabel,
  selectOptionLabel,
} from '../fill-hug-fixed-control'
import { Axis, FixedHugFillMode, MaxContent } from '../inspector-common'
import { TextAutoSizingTestId } from '../sections/style-section/text-subsection/text-auto-sizing-control'

describe('Fixed / Fill / Hug control', () => {
  describe('fill container', () => {
    it('set width to fill container in flex row', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithWidth('row'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(div.style.width).toEqual('')
      expect(div.style.minWidth).toEqual('')
      expect(div.style.maxWidth).toEqual('')
      expect(div.style.flexGrow).toEqual('1')
    })

    it('detects fill in flex row with shorthand style', async () => {
      const editor = await renderTestEditorWithCode(
        flexProjectWithInjectedStyle(`flex: '2 1 10px'`),
        'await-first-dom-report',
      )

      await select(editor, 'child')

      const fillControls = await editor.renderedDOM.findAllByText(FillContainerLabel)
      expect(fillControls.length).toEqual(1)

      const control = editor.renderedDOM.getByTestId(FillFixedHugControlId('width'))
      expect((control as HTMLInputElement).value).toEqual('2')
    })

    it('detects fill in flex row with longhand style', async () => {
      const editor = await renderTestEditorWithCode(
        flexProjectWithInjectedStyle(`flexGrow: 1`),
        'await-first-dom-report',
      )

      await select(editor, 'child')

      const fillControls = await editor.renderedDOM.findAllByText(FillContainerLabel)
      expect(fillControls.length).toEqual(1)

      const control = editor.renderedDOM.getByTestId(FillFixedHugControlId('width'))
      expect((control as HTMLInputElement).value).toEqual('1')
    })

    it('set width to fill container in flex column', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithWidth('column'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(div.style.minWidth).toEqual('')
      expect(div.style.maxWidth).toEqual('')
      expect(div.style.width).toEqual('100%')
      expect(div.style.flexGrow).toEqual('')
    })

    it('set height to fill container in flex row', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithHeight('row'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(div.style.minHeight).toEqual('')
      expect(div.style.maxHeight).toEqual('')
      expect(div.style.height).toEqual('100%')
      expect(div.style.flexGrow).toEqual('')
    })

    it('set height to fill container in flex column', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithHeight('column'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(div.style.minHeight).toEqual('')
      expect(div.style.maxHeight).toEqual('')
      expect(div.style.height).toEqual('')
      expect(div.style.flexGrow).toEqual('1')
    })

    it('edit fill container value in flex', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFlexChildInFill,
        'await-first-dom-report',
      )
      const child = await select(editor, 'child')
      await editor.getDispatchFollowUpActionsFinished()

      expect(child.style.flexGrow).toEqual('1')
      const control = editor.renderedDOM.getByTestId(FillFixedHugControlId('width'))
      await mouseClickAtPoint(control, { x: 5, y: 5 })
      await expectSingleUndo2Saves(editor, async () => {
        act(() => {
          fireEvent.change(control, { target: { value: '3' } })
          fireEvent.blur(control)
        })
      })

      expect(child.style.flexGrow).toEqual('3')
    })

    it('edit fill container value in flow', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithChildInFlowLayout,
        'await-first-dom-report',
      )
      const child = await select(editor, 'child')

      expect(child.style.width).toEqual('100%')
      const control = editor.renderedDOM.getByTestId(FillFixedHugControlId('width'))
      await mouseClickAtPoint(control, { x: 5, y: 5 })

      await expectSingleUndo2Saves(editor, async () => {
        act(() => {
          fireEvent.change(control, { target: { value: '50%' } })
          fireEvent.blur(control)
        })
      })

      expect(child.style.width).toEqual('50%')
    })

    it('set width to fill container on absolute positioned element', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          left: 10,
          top: 10,
          width: 100,
          height: 100,
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          top: 10,
          width: '100%',
          height: 100,
        `),
      )
    })

    it('set width to fill container on absolute positioned element with height already set to fill', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          left: 10,
          width: 100,
          height: '100%',
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`
        width: '100%',
        height: '100%',
        contain: 'layout',
        `),
      )
    })

    it('set height to fill container on absolute positioned element', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          left: 10,
          top: 10,
          width: 100,
          height: 100,
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          left: 10,
          width: 100,
          height: '100%',
        `),
      )
    })

    it('set height to fill container on absolute positioned element with width already set to fill', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          position: 'absolute',
          top: 10,
          width: '100%',
          height: 100,
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[1]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`
          width: '100%',
          height: '100%',
          contain: 'layout',
        `),
      )
    })

    it('set height to fill container on static positioned element with width already set to fill', async () => {
      const editor = await renderTestEditorWithCode(
        absoluteProjectWithInjectedStyle(`
          top: 10,
          width: '100%',
          height: 100,
        `),
        'await-first-dom-report',
      )
      await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[1]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      // Should not add contain: layout
      expect(getPrintedUiJsCodeWithoutUIDs(editor.getEditorState())).toEqual(
        absoluteProjectWithInjectedStyle(`width: '100%', height: '100%'`),
      )
    })
  })

  describe('hug contents', () => {
    it('hug contents along the horizontal axis', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithWidth('row'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'parent')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(div.style.width).toEqual(MaxContent)
      expect(div.style.minWidth).toEqual('')
      expect(div.style.maxWidth).toEqual('')
    })

    it('hug contents along the vertical axis', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithHeight('row'),
        'await-first-dom-report',
      )
      const div = await select(editor, 'parent')

      const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(div.style.height).toEqual(MaxContent)
      expect(div.style.minHeight).toEqual('')
      expect(div.style.maxHeight).toEqual('')
    })

    describe('Convert children to fixed size when setting to hug contents to avoid parent container collapsing', () => {
      it('child is set to fill container on the horizontal axis', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToHorizontalFill,
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.height).toEqual('149px')
        expect(child.style.width).toEqual('')
        expect(child.style.flexGrow).toEqual('1')

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

        await mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        await expectSingleUndo2Saves(editor, async () => {
          await mouseClickAtPoint(button, { x: 5, y: 5 })
        })

        expect(parent.style.height).toEqual('759px')
        expect(child.style.height).toEqual('149px')
        expect(parent.style.width).toEqual(MaxContent)
        expect(child.style.width).toEqual('700px')
      })
      it('child is set to fill container on the vertical axis', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToVerticalFill,
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual('149px')
        expect(child.style.height).toEqual('')
        expect(child.style.flexGrow).toEqual('1')

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

        await mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        await expectSingleUndo2Saves(editor, async () => {
          await mouseClickAtPoint(button, { x: 5, y: 5 })
        })

        expect(parent.style.width).toEqual('700px')
        expect(child.style.width).toEqual('149px')
        expect(parent.style.height).toEqual(MaxContent)
        expect(child.style.height).toEqual('759px')
      })

      it('child is set to fixed size on the horizontal axis', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToFixed('row'),
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual('302px')
        expect(child.style.height).toEqual('141px')

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

        await mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        await expectSingleUndo2Saves(editor, async () => {
          await mouseClickAtPoint(button, { x: 5, y: 5 })
        })

        expect(parent.style.width).toEqual(MaxContent)
        expect(child.style.width).toEqual('302px')
        expect(parent.style.height).toEqual('759px')
        expect(child.style.height).toEqual('141px')
      })

      it('child is set to fixed size on the vertical axis, no conversion happens', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToFixed('column'),
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual('302px')
        expect(child.style.height).toEqual('141px')

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

        await mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        await expectSingleUndo2Saves(editor, async () => {
          await mouseClickAtPoint(button, { x: 5, y: 5 })
        })

        expect(parent.style.width).toEqual('700px')
        expect(child.style.width).toEqual('302px')
        expect(parent.style.height).toEqual(MaxContent)
        expect(child.style.height).toEqual('141px')
      })

      it('child is set to hug contents on the horizontal axis, no conversion happens', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToHugContents('row'),
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual(MaxContent)
        expect(child.style.height).toEqual(MaxContent)

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[0]

        await mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        await expectSingleUndo2Saves(editor, async () => {
          await mouseClickAtPoint(button, { x: 5, y: 5 })
        })

        expect(parent.style.width).toEqual(MaxContent)
        expect(child.style.width).toEqual(MaxContent)
        expect(parent.style.height).toEqual('751px')
        expect(child.style.height).toEqual(MaxContent)
      })

      it('child is set to hug contents on the vertical axis, no conversion happens', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithChildSetToHugContents('column'),
          'await-first-dom-report',
        )
        const parent = await select(editor, 'parent')
        const child = editor.renderedDOM.getByTestId('child')

        expect(child.style.width).toEqual(MaxContent)
        expect(child.style.height).toEqual(MaxContent)

        const control = (await editor.renderedDOM.findAllByText(FixedLabel))[1]

        await mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
        await expectSingleUndo2Saves(editor, async () => {
          await mouseClickAtPoint(button, { x: 5, y: 5 })
        })

        expect(parent.style.width).toEqual('508px')
        expect(child.style.width).toEqual(MaxContent)
        expect(parent.style.height).toEqual(MaxContent)
        expect(child.style.height).toEqual(MaxContent)
      })
    })
    it('setting hug contents on an absolute element removes extra pins', async () => {
      const testCode = `
      <div style={{ ...props.style }} data-uid='aaa'>
        <div
          style={{ position: 'absolute', left: 40, top: 20, bottom: 50, right: 60}}
          data-uid='bbb'
          data-testid='bbb'
        >hello content</div>
      </div>
`
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(testCode),
        'await-first-dom-report',
      )
      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
      await editor.dispatch(selectComponents([targetPath], false), true)

      const fixedControls = await editor.renderedDOM.findAllByText(FixedLabel)
      const horizontalControl = fixedControls[0]
      await mouseClickAtPoint(horizontalControl, { x: 5, y: 5 })

      const horizontalLabel = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(horizontalLabel, { x: 5, y: 5 })
      })

      const verticalControl = fixedControls[1]
      await mouseClickAtPoint(verticalControl, { x: 5, y: 5 })

      const verticalLabel = (await editor.renderedDOM.findAllByText(HugContentsLabel))[1]

      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(verticalLabel, { x: 5, y: 5 })
      })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div style={{ ...props.style }} data-uid='aaa'>
          <div
            style={{ position: 'absolute', left: 40, top: 20, width: 'max-content', height: 'max-content'}}
            data-uid='bbb'
            data-testid='bbb'
          >hello content</div>
        </div>
        `),
      )
    })
  })

  /* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectOptionsToBePresent"] }] */
  it('when toggling between element, options in the dropdown are updated', async () => {
    const editor = await renderTestEditorWithCode(
      projectWithElementsToToggleBetween,
      'await-first-dom-report',
    )
    await selectComponentsForTest(editor, [EP.fromString('sb/upper')])
    await openDropdownViaLabel(editor, 'fixed')
    await expectOptionsToBePresent(editor, ['fixed'])

    await selectComponentsForTest(editor, [EP.fromString('sb/lower')])
    await openDropdownViaLabel(editor, 'fixed')
    await expectOptionsToBePresent(editor, ['fixed', 'hug'])
  })

  describe('converts parent element to fixed size when child is set to fill container', () => {
    describe('in a flex container', () => {
      describe('with flex-direction: row set', () => {
        it('along the horizontal axis', async () => {
          const editor = await renderTestEditorWithCode(
            projectWithFlexParentSetToFitContents('row'),
            'await-first-dom-report',
          )

          const parent = editor.renderedDOM.getByTestId('parent')
          const child = editor.renderedDOM.getByTestId('child')

          await selectComponentsForTest(editor, [EP.fromString('sb/parent/child')])

          await setSelectedElementsToFill(editor, 'horizontal')

          expect(child.style.width).toEqual('')
          expect(child.style.flexGrow).toEqual('1')
          expect(parent.style.width).toEqual('417.5px')
        })

        it('along the vertical axis', async () => {
          const editor = await renderTestEditorWithCode(
            projectWithFlexParentSetToFitContents('row'),
            'await-first-dom-report',
          )

          const parent = editor.renderedDOM.getByTestId('parent')
          const child = editor.renderedDOM.getByTestId('child')

          await selectComponentsForTest(editor, [EP.fromString('sb/parent/child')])

          await setSelectedElementsToFill(editor, 'vertical')

          expect(child.style.height).toEqual('100%')
          expect(parent.style.height).toEqual('386px')
        })
      })

      describe('with flex-direction: column set', () => {
        it('along the horizontal axis', async () => {
          const editor = await renderTestEditorWithCode(
            projectWithFlexParentSetToFitContents('column'),
            'await-first-dom-report',
          )

          const parent = editor.renderedDOM.getByTestId('parent')
          const child = editor.renderedDOM.getByTestId('child')

          await selectComponentsForTest(editor, [EP.fromString('sb/parent/child')])

          await setSelectedElementsToFill(editor, 'horizontal')

          expect(child.style.width).toEqual('100%')
          expect(parent.style.width).toEqual('417.5px')
        })

        it('along the vertical axis', async () => {
          const editor = await renderTestEditorWithCode(
            projectWithFlexParentSetToFitContents('column'),
            'await-first-dom-report',
          )

          const parent = editor.renderedDOM.getByTestId('parent')
          const child = editor.renderedDOM.getByTestId('child')

          await selectComponentsForTest(editor, [EP.fromString('sb/parent/child')])

          await setSelectedElementsToFill(editor, 'vertical')

          expect(child.style.height).toEqual('')
          expect(child.style.flexGrow).toEqual('1')
          expect(parent.style.height).toEqual('386px')
        })
      })
    })

    describe('in a flow container', () => {
      it('along the horizontal axis', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithParentSetToFitContents,
          'await-first-dom-report',
        )

        const parent = editor.renderedDOM.getByTestId('parent')
        const child = editor.renderedDOM.getByTestId('child')

        await selectComponentsForTest(editor, [EP.fromString('sb/parent/child')])

        await setSelectedElementsToFill(editor, 'horizontal')

        expect(child.style.width).toEqual('100%')
        expect(parent.style.width).toEqual('417.5px')
      })

      it('along the vertical axis', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithParentSetToFitContents,
          'await-first-dom-report',
        )

        const parent = editor.renderedDOM.getByTestId('parent')
        const child = editor.renderedDOM.getByTestId('child')

        await selectComponentsForTest(editor, [EP.fromString('sb/parent/child')])

        await setSelectedElementsToFill(editor, 'vertical')

        expect(child.style.height).toEqual('100%')
        expect(parent.style.height).toEqual('386px')
      })
    })
  })

  describe('fixed size', () => {
    it('global frames are correct for frames wrapping frames', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithNestedFrames,
        'await-first-dom-report',
      )

      const superGroupGlobalFrame = await getGlobalFrame(editor, EP.fromString('sb/grandparent'))

      expect(superGroupGlobalFrame.width).toBe(326)
      expect(superGroupGlobalFrame.height).toBe(407)

      {
        const widthControl = editor.renderedDOM.getByTestId(FillFixedHugControlId('width'))
        expect((widthControl as HTMLInputElement).value).toEqual('326')
        const heightControl = editor.renderedDOM.getByTestId(FillFixedHugControlId('height'))
        expect((heightControl as HTMLInputElement).value).toEqual('407')
      }

      const groupGlobalFrame = await getGlobalFrame(editor, EP.fromString('sb/grandparent/parent'))
      expect(groupGlobalFrame.width).toBe(326)
      expect(groupGlobalFrame.height).toBe(407)

      {
        const widthControl = editor.renderedDOM.getByTestId(FillFixedHugControlId('width'))
        expect((widthControl as HTMLInputElement).value).toEqual('326')
        const heightControl = editor.renderedDOM.getByTestId(FillFixedHugControlId('height'))
        expect((heightControl as HTMLInputElement).value).toEqual('407')
      }
    })
  })
})

describe('Fixed/hug on text elements', () => {
  it('Sets text element fixed to hug inside the font section', async () => {
    const testCode = `
    <div style={{ ...props.style }} data-uid='aaa'>
      <div
        style={{ position: 'absolute', left: 40, top: 20, width: 100, height: 25}}
        data-uid='bbb'
        data-testid='bbb'
      >hello text element!</div>
    </div>
`
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(testCode),
      'await-first-dom-report',
    )
    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await editor.dispatch(selectComponents([targetPath], false), true)

    const textAutoSizeIcon = editor.renderedDOM.getByTestId(`${TextAutoSizingTestId}-0`)
    await expectSingleUndo2Saves(editor, async () => {
      await mouseClickAtPoint(textAutoSizeIcon, { x: 2, y: 2 })
    })

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div
          style={{ position: 'absolute', left: 40, top: 20, width: 'max-content', height: 'max-content'}}
          data-uid='bbb'
          data-testid='bbb'
        >hello text element!</div>
      </div>
      `),
    )
  })
  it('Sets text element hug to fixed inside the font section', async () => {
    const testCode = `
    <div style={{ ...props.style }} data-uid='aaa'>
      <div
        style={{ position: 'absolute', left: 40, top: 20, width: 'max-content', height: 'max-content', lineHeight: '18px' }}
        data-uid='bbb'
        data-testid='bbb'
      >hello text element!</div>
    </div>
`
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(testCode),
      'await-first-dom-report',
    )
    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    await editor.dispatch(selectComponents([targetPath], false), true)

    const textFixedSizeIcon = editor.renderedDOM.getByTestId(`${TextAutoSizingTestId}-1`)
    await expectSingleUndo2Saves(editor, async () => {
      await mouseClickAtPoint(textFixedSizeIcon, { x: 2, y: 2 })
    })

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div
          style={{ position: 'absolute', left: 40, top: 20, width: 120, height: 18, lineHeight: '18px'}}
          data-uid='bbb'
          data-testid='bbb'
        >hello text element!</div>
      </div>
      `),
    )
  })
})

async function expectOptionsToBePresent(
  editor: EditorRenderResult,
  modes: Array<FixedHugFillMode>,
) {
  expect(
    modes.map((mode) => editor.renderedDOM.queryAllByText(selectOptionLabel(mode)[0])).length,
  ).toEqual(modes.length)
}

async function openDropdownViaLabel(editor: EditorRenderResult, mode: FixedHugFillMode) {
  const control = (await editor.renderedDOM.findAllByText(selectOptionLabel(mode)))[1]
  await mouseClickAtPoint(control, { x: 5, y: 5 })
}

async function select(
  editor: EditorRenderResult,
  testId: 'child' | 'parent',
): Promise<HTMLElement> {
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId(testId)
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 5,
    y: divBounds.y + 4,
  }

  if (testId === 'child') {
    await mouseDoubleClickAtPoint(canvasControlsLayer, divCorner)
  } else if (testId === 'parent') {
    await mouseClickAtPoint(canvasControlsLayer, divCorner)
  } else {
    assertNever(testId)
  }

  return div
}

async function setSelectedElementsToFill(editor: EditorRenderResult, axis: Axis) {
  const idx = axis === 'horizontal' ? 0 : axis === 'vertical' ? 1 : assertNever(axis)
  const control = (await editor.renderedDOM.findAllByText(FixedLabel))[idx]

  await mouseClickAtPoint(control, { x: 5, y: 5 })

  const button = (await editor.renderedDOM.findAllByText(FillContainerLabel))[0]
  await expectSingleUndo2Saves(editor, async () => {
    await mouseClickAtPoint(button, { x: 5, y: 5 })
  })
}

const projectWithWidth = (flexDirection: FlexDirection) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard>
    <div
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          minWidth: 100,
          maxWidth: 1000,
          width: 229,
          height: 149,
          contain: 'layout',
        }}
      />
    </div>
  </Storyboard>
)

`

const projectWithHeight = (flexDirection: FlexDirection) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          minHeight: 100,
          maxHeight: 1000,
          width: 229,
          height: 149,
          contain: 'layout',
        }}
      />
    </div>
  </Storyboard>
)
`

const projectWithChildSetToHorizontalFill = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          flexGrow: 1,
          height: 149,
          contain: 'layout',
        }}
      />
    </div>
  </Storyboard>
)
`

const projectWithChildSetToVerticalFill = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: 'column'
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          flexGrow: 1,
          width: 149,
          contain: 'layout',
        }}
      />
    </div>
  </Storyboard>
)
`

const projectWithChildSetToFixed = (flexDirection: FlexDirection) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='33d'>
    <div
      data-testid='parent'
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}'
      }}
      data-label='Playground'
      data-uid='26c'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          height: 141,
          contain: 'layout',
          width: 302,
        }}
        data-uid='744'
      />
    </div>
  </Storyboard>
)
`

const projectWithChildSetToHugContents = (
  flexDirection: FlexDirection,
) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'
import { App } from '/src/app.js'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      data-testid='parent'
      style={{
        height: 751,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: '${flexDirection}',
        width: 508,
      }}
      data-label='Playground'
      data-uid='26c'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          contain: 'layout',
          display: 'flex',
          width: 'max-content',
          height: 'max-content',
        }}
        data-uid='744'
      >
        <div
          style={{
            backgroundColor: '#ffa19c',
            contain: 'layout',
            height: 208,
            width: 165.5,
          }}
          data-uid='0b9'
        />
        <div
          style={{
            backgroundColor: '#c4ded1',
            contain: 'layout',
            height: 208,
            width: 165.5,
          }}
          data-uid='741'
        />
      </div>
    </div>
  </Storyboard>
)
`

const projectWithFlexChildInFill = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 200,
        top: 38,
        width: 533,
        height: 354,
        display: 'flex',
        padding: '30px 50px 30px 50px',
        gap: 55,
        flexDirection: 'row',
      }}
      data-uid='6b7'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          flexGrow: 1,
        }}
        data-uid='a9d'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          flexGrow: 1,
        }}
        data-uid='aaa'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          flexGrow: 1,
        }}
        data-uid='aab'
      />
    </div>
  </Storyboard>
)
`

const projectWithChildInFlowLayout = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 300,
        top: 38,
        width: 533,
        height: 354,
        padding: '30px 50px 30px 50px',
      }}
      data-uid='6b7'
    >
      <div
        data-testid='child'
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          width: '100%',
        }}
        data-uid='a9d'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          width: 65,
        }}
        data-uid='aaa'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 61,
          contain: 'layout',
          width: 65,
        }}
        data-uid='aab'
      />
    </div>
  </Storyboard>
)
`
const projectWithNestedFrames = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='grandparent' style={{ position: 'absolute', width: 326, height: 407, left: 0, top: 0 }}>
      <div data-uid='parent' style={{ position: 'absolute', width: 326, height: 407, left: 0, top: 0 }}>
        <div
          style={{
            backgroundColor: '#00acff',
            position: 'absolute',
            left: 0,
            top: 0,
            width: 100,
            height: 407,
          }}
          data-uid='aab'
          data-label='eee'
        />
        <div
          style={{
            backgroundColor: '#ff0001',
            position: 'absolute',
            left: 226,
            top: 0,
            width: 100,
            height: 407,
          }}
          data-uid='aaa'
          data-label='eee'
        />
      </div>
      <div
        style={{
          backgroundColor: '#ffffff',
          position: 'absolute',
          left: 113,
          top: 0,
          width: 100,
          height: 407,
        }}
        data-uid='aac'
        data-label='eee'
      />
    </div>
  </Storyboard>
)
`

const absoluteProjectWithInjectedStyle = (stylePropsAsString: string) =>
  formatTestProjectCode(`
import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard>
    <div
      data-testid='parent'
      style={{
        position: 'absolute',
        left: 0,
        top: 0,
        width: 500,
        height: 500,
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{${stylePropsAsString}}}
      />
    </div>
  </Storyboard>
)`)

const flexProjectWithInjectedStyle = (stylePropsAsString: string) =>
  formatTestProjectCode(`
import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard>
    <div
      data-testid='parent'
      style={{
        position: 'absolute',
        left: 0,
        top: 0,
        width: 500,
        height: 500,
        display: 'flex',
      }}
      data-label='Playground'
    >
      <div
        data-testid='child'
        style={{${stylePropsAsString}}}
      />
    </div>
  </Storyboard>
)`)

const projectWithElementsToToggleBetween = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 16,
        top: 10,
        width: 200,
        height: 200,
      }}
      data-uid='upper'
    >
      <div
        style={{
          backgroundColor: '#F6141433',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 100,
          height: 100,
        }}
      />
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 10,
        top: 220,
        width: 200,
        height: 200,
      }}
      data-uid='lower'
    >
      <div
        style={{
          backgroundColor: '#F6141433',
          width: 100,
          height: 100,
        }}
      />
    </div>
  </Storyboard>
)
`

const projectWithFlexParentSetToFitContents = (
  flexDirection: FlexDirection,
) => `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 219,
        top: 101,
        padding: '80px 80px 80px 80px',
        display: 'flex',
        flexDirection: '${flexDirection}',
        alignItems: 'center',
        justifyContent: 'center',
        width: 'max-content',
        height: 'max-content',
      }}
      data-testid='parent'
      data-uid='parent'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 226,
          contain: 'layout',
          width: 257.5,
        }}
        data-testid='child'
        data-uid='child'
      />
    </div>
  </Storyboard>
)
`

const projectWithParentSetToFitContents = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 219,
        top: 101,
        padding: '80px 80px 80px 80px',
        width: 'max-content',
        height: 'max-content',
      }}
      data-testid='parent'
      data-uid='parent'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          height: 226,
          contain: 'layout',
          width: 257.5,
        }}
        data-testid='child'
        data-uid='child'
      />
    </div>
  </Storyboard>
)
`

async function getGlobalFrame(editor: EditorRenderResult, path: ElementPath) {
  await selectComponentsForTest(editor, [path])
  const instance = MetadataUtils.findElementByElementPath(
    editor.getEditorState().editor.jsxMetadata,
    path,
  )
  if (instance?.globalFrame == null) {
    throw new Error('`instance?.globalFrame` is null')
  }
  if (isInfinityRectangle(instance.globalFrame)) {
    throw new Error('`instance?.globalFrame` is infinity rect')
  }
  return instance.globalFrame
}
