import { act, fireEvent } from '@testing-library/react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { isInfinityRectangle } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import {
  expectNoAction,
  expectSingleUndo2Saves,
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../../utils/utils.test-utils'
import { CanvasControlsContainerID } from '../../canvas/controls/new-canvas-controls'
import { mouseClickAtPoint, mouseDoubleClickAtPoint } from '../../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/meta-actions'
import type { FlexDirection } from '../common/css-utils'
import {
  DetectedLabel,
  ComputedLabel,
  FillContainerLabel,
  FillFixedHugControlId,
  FixedLabel,
  HugContentsLabel,
  selectOptionLabel,
  ScaledLabel,
  SqueezeContentsLabel,
  CollapsedLabel,
} from '../fill-hug-fixed-control'
import type { Axis, FixedHugFillMode } from '../inspector-common'
import { MaxContent } from '../inspector-common'
import {
  TextAutoSizingTestId,
  detectTextSizingStateMultiSelect,
} from '../sections/style-section/text-subsection/text-auto-sizing-control'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'

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

      // TODO we currently don't have any control that could show the flex grow value

      // const control = editor.renderedDOM.getByTestId('frame-width-number-input')
      // expect((control as HTMLInputElement).value).toEqual('2')
    })

    it('detects fill in flex row with longhand style', async () => {
      const editor = await renderTestEditorWithCode(
        flexProjectWithInjectedStyle(`flexGrow: 1`),
        'await-first-dom-report',
      )

      await select(editor, 'child')

      const fillControls = await editor.renderedDOM.findAllByText(FillContainerLabel)
      expect(fillControls.length).toEqual(1)

      // TODO we currently don't have any control that could show the flex grow value

      // const control = editor.renderedDOM.getByTestId('frame-width-number-input')
      // expect((control as HTMLInputElement).value).toEqual('1')
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

    xit('edit fill container value in flex', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithFlexChildInFill,
        'await-first-dom-report',
      )
      const child = await select(editor, 'child')
      await editor.getDispatchFollowUpActionsFinished()

      expect(child.style.flexGrow).toEqual('1')
      const control = editor.renderedDOM.getByTestId('frame-width-number-input')
      await mouseClickAtPoint(control, { x: 5, y: 5 })
      await expectSingleUndo2Saves(editor, async () => {
        act(() => {
          fireEvent.change(control, { target: { value: '3' } })
          fireEvent.blur(control)
        })
      })

      expect(child.style.flexGrow).toEqual('3')
    })

    xit('edit fill container value in flow', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithChildInFlowLayout,
        'await-first-dom-report',
      )
      const child = await select(editor, 'child')

      expect(child.style.width).toEqual('100%')
      const control = editor.renderedDOM.getByTestId('frame-width-number-input')
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

    it('detects fill container on a static element', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: '100%',
            height: '100%',
            contain: 'layout',
          }}
          data-uid='thediv'
        />
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:thediv`),
      ])

      expect(editor.renderedDOM.getAllByText(FillContainerLabel).length).toEqual(2)
    })

    it('percent values that are not 100% are classified as scaled container', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: '90%',
            height: '90%',
            contain: 'layout',
          }}
          data-uid='thediv'
        />
        `),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:thediv`),
      ])

      expect(editor.renderedDOM.queryAllByText(FillContainerLabel).length).toEqual(0)
      expect(editor.renderedDOM.getAllByText(ScaledLabel).length).toEqual(2)
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

    it('Group with explicit width, height show up as Hug x Hug label, but the number inputs for width and height act like for Fixed divs', async () => {
      const editor = await renderTestEditorWithCode(
        `
        import * as React from 'react'
        import { Storyboard, Group } from 'utopia-api'
        import { App } from '/src/app.js'

        export var storyboard = (
          <Storyboard>
            <Group
              data-uid='parent'
              data-testid='parent'
              style={{
                position: 'absolute',
                left: 50,
                top: 150,
                width: 200,
                height: 300,
              }}
            >
              <div
                data-testid='child'
                data-uid='child'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 200,
                  height: 300,
                }}
              />
            </Group>
          </Storyboard>
        )
`,
        'await-first-dom-report',
      )

      const groupDiv = await select(editor, 'parent')

      const hugContentsDropdown = editor.renderedDOM.getAllByText(HugContentsLabel)

      const widthNumberControl = editor.renderedDOM.getByTestId(
        'frame-width-number-input',
      ) as HTMLInputElement
      const heightNumberControl = editor.renderedDOM.getByTestId(
        'frame-height-number-input',
      ) as HTMLInputElement

      expect(hugContentsDropdown.length).toEqual(2)
      expect(widthNumberControl.value).toEqual('200')
      expect(heightNumberControl.value).toEqual('300')

      expect(widthNumberControl.getAttribute('data-controlstatus')).toEqual('simple')
      expect(heightNumberControl.getAttribute('data-controlstatus')).toEqual('simple')
    })

    it('Group without width, height show up as Hug x Hug label, and the number inputs for width and height show up as regular', async () => {
      const editor = await renderTestEditorWithCode(
        `
        import * as React from 'react'
        import { Storyboard, Group } from 'utopia-api'
        import { App } from '/src/app.js'

        export var storyboard = (
          <Storyboard>
            <Group
              data-uid='parent'
              data-testid='parent'
              style={{
                position: 'absolute',
                left: 50,
                top: 150,
              }}
            >
              <div
                data-testid='child'
                data-uid='child'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 200,
                  height: 300,
                }}
              />
            </Group>
          </Storyboard>
        )
`,
        'await-first-dom-report',
      )

      const groupDiv = await select(editor, 'parent')

      const hugContentsDropdown = editor.renderedDOM.getAllByText(HugContentsLabel)

      const widthNumberControl = editor.renderedDOM.getByTestId(
        'frame-width-number-input',
      ) as HTMLInputElement
      const heightNumberControl = editor.renderedDOM.getByTestId(
        'frame-height-number-input',
      ) as HTMLInputElement

      expect(hugContentsDropdown.length).toEqual(2)
      expect(widthNumberControl.value).toEqual('200')
      expect(heightNumberControl.value).toEqual('300')

      expect(widthNumberControl.getAttribute('data-controlstatus')).toEqual('simple')
      expect(heightNumberControl.getAttribute('data-controlstatus')).toEqual('simple')
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

      const fixedControls = await editor.renderedDOM.findAllByText(DetectedLabel)
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
    it('rounds up on text elements', async () => {
      const editor = await renderTestEditorWithCode(
        `import * as React from 'react'
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
                flexDirection: 'row'
              }}
              data-label='Playground'
            >
              <span
                data-testid='child'
                style={{
                  backgroundColor: '#aaaaaa33',
                  wordBreak: 'break-word',
                  width: 'max-content',
                  height: 'max-content',
                  contain: 'layout',
                }}
              >span</span>
            </div>
          </Storyboard>
        )`,
        'await-first-dom-report',
      )
      const span = await select(editor, 'child')

      const control = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FixedLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(span.style.width).toEqual('30px')
      expect(span.style.height).toEqual(MaxContent)
    })

    describe('detecting types of hugging', () => {
      it('in case of width max-content it is hug', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
        import { Storyboard } from 'utopia-api'
        
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
                flexDirection: 'row'
              }}
              data-label='Playground'
            >
              <span
                data-testid='child'
                style={{
                  backgroundColor: '#aaaaaa33',
                  wordBreak: 'break-word',
                  width: 'max-content',
                  height: 'max-content',
                  contain: 'layout',
                }}
              >span</span>
            </div>
          </Storyboard>
        )`,
          'await-first-dom-report',
        )
        await select(editor, 'child')

        const hugControls = await editor.renderedDOM.findAllByText(HugContentsLabel)

        expect(hugControls).toHaveLength(2)
      })

      it('in case of width/height max-content from css it is hug', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
      import { Storyboard } from 'utopia-api'

      const css = \`.minContent {
        width: min-content;
        height: min-content;
      }

      .maxContent {
        width: max-content;
        height: max-content;
      }
      
      .auto {
        width: auto;
        height: auto;
      }\`
      
      export var storyboard = (
        <Storyboard>
          <style>{css}</style>
          <div
            data-testid='parent'
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
              display: 'flex',
              flexDirection: 'row'
            }}
            data-label='Playground'
          >
            <span
              data-testid='child'
              className='maxContent'
              style={{
                backgroundColor: '#aaaaaa33',
                wordBreak: 'break-word',
                contain: 'layout',
              }}
            >span</span>
          </div>
        </Storyboard>
      )`,
          'await-first-dom-report',
        )
        await select(editor, 'child')

        const hugControls = await editor.renderedDOM.findAllByText(HugContentsLabel)

        expect(hugControls).toHaveLength(2)
        // TODO check the control status is detected from css
      })

      it('in case of width min-content it is squeeze', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
        import { Storyboard } from 'utopia-api'
        
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
                flexDirection: 'row'
              }}
              data-label='Playground'
            >
              <span
                data-testid='child'
                style={{
                  backgroundColor: '#aaaaaa33',
                  wordBreak: 'break-word',
                  width: 'min-content',
                  height: 'min-content',
                  contain: 'layout',
                }}
              >span</span>
            </div>
          </Storyboard>
        )`,
          'await-first-dom-report',
        )
        await select(editor, 'child')

        const hugControls = await editor.renderedDOM.findAllByText(SqueezeContentsLabel)

        expect(hugControls).toHaveLength(2)
      })

      it('in case of width/height min-content from css it is squeeze', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
      import { Storyboard } from 'utopia-api'

      const css = \`.minContent {
        width: min-content;
        height: min-content;
      }

      .maxContent {
        width: max-content;
        height: max-content;
      }
      
      .auto {
        width: auto;
        height: auto;
      }\`
      
      export var storyboard = (
        <Storyboard>
          <style>{css}</style>
          <div
            data-testid='parent'
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
              display: 'flex',
              flexDirection: 'row'
            }}
            data-label='Playground'
          >
            <span
              data-testid='child'
              className='minContent'
              style={{
                backgroundColor: '#aaaaaa33',
                wordBreak: 'break-word',
                contain: 'layout',
              }}
            >span</span>
          </div>
        </Storyboard>
      )`,
          'await-first-dom-report',
        )
        await select(editor, 'child')

        const hugControls = await editor.renderedDOM.findAllByText(SqueezeContentsLabel)

        expect(hugControls).toHaveLength(2)
        // TODO check the control status is detected from css
      })

      it('in case of hugging zero size is collapsed', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
        import { Storyboard } from 'utopia-api'
        
        export var storyboard = (
          <Storyboard data-uid='storyboard'>
            <div
              data-testid='parent'
              style={{
                width: 700,
                height: 759,
                position: 'absolute',
                left: 212,
                top: 128,
                display: 'flex',
                flexDirection: 'row'
              }}
              data-uid='parent'
              data-label='Playground'
            >
              <span
                data-testid='child'
                data-uid='child'
                style={{
                  backgroundColor: '#aaaaaa33',
                  wordBreak: 'break-word',
                  width: 'max-content',
                  height: 'max-content',
                  contain: 'layout',
                }}
              />
            </div>
          </Storyboard>
        )`,
          'await-first-dom-report',
        )
        await editor.dispatch(
          selectComponents([EP.fromString('storyboard/parent/child')], false),
          true,
        )
        const hugControls = await editor.renderedDOM.findAllByText(CollapsedLabel)

        expect(hugControls).toHaveLength(2)
      })
    })
  })

  describe('groups', () => {
    it('setting fixed on a group element converts it to a frame', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
			import { Scene, Storyboard, Group } from 'utopia-api'

			export var storyboard = (
			  <Storyboard data-uid='sb'>
				<Group data-uid='group' style={{ position: 'absolute', left: 100, top: 100, width: 187, height: 116, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 68, height: 46 }} />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 119, top: 70, width: 68, height: 46 }} />
				</Group>
			  </Storyboard>
			)
		`),
        'await-first-dom-report',
      )

      const targetPath = EP.fromString(`sb/group`)
      await renderResult.dispatch(selectComponents([targetPath], false), true)

      const fixedControls = await renderResult.renderedDOM.findAllByText(HugContentsLabel)
      const horizontalControl = fixedControls[0]
      await mouseClickAtPoint(horizontalControl, { x: 5, y: 5 })

      const horizontalLabel = (await renderResult.renderedDOM.findAllByText(FixedLabel))[0]
      await expectSingleUndo2Saves(renderResult, async () => {
        await mouseClickAtPoint(horizontalLabel, { x: 5, y: 5 })
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
			import { Scene, Storyboard, Group } from 'utopia-api'

			export var storyboard = (
			  <Storyboard data-uid='sb'>
				<div data-uid='group' style={{ position: 'absolute', left: 100, top: 100, width: 187, height: 116, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 68, height: 46 }} />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 119, top: 70, width: 68, height: 46 }} />
				</div>
			  </Storyboard>
			)
		`),
      )

      expect(renderResult.getEditorState().editor.toasts).toHaveLength(1)
      expect(renderResult.getEditorState().editor.toasts[0].message).toEqual(`Converted to frame`)
    })
    it('setting fixed on a group element that cannot be converted shows a toast and does nothing', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
			import { Scene, Storyboard, Group } from 'utopia-api'

			export var storyboard = (
			  <Storyboard data-uid='sb'>
				<Group data-uid='group' style={{ position: 'absolute', left: 100, top: 100, width: 187, height: 116, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'relative', left: 0, top: 0, width: 68, height: 46 }} />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 119, top: 70, width: 68, height: 46 }} />
				</Group>
			  </Storyboard>
			)
		`),
        'await-first-dom-report',
      )

      const targetPath = EP.fromString(`sb/group`)
      await renderResult.dispatch(selectComponents([targetPath], false), true)

      const fixedControls = await renderResult.renderedDOM.findAllByText(HugContentsLabel)
      const horizontalControl = fixedControls[0]
      await mouseClickAtPoint(horizontalControl, { x: 5, y: 5 })

      const horizontalLabel = (await renderResult.renderedDOM.findAllByText(FixedLabel))[0]
      await expectNoAction(renderResult, async () => {
        await mouseClickAtPoint(horizontalLabel, { x: 5, y: 5 })
      })

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        formatTestProjectCode(`
			import { Scene, Storyboard, Group } from 'utopia-api'

			export var storyboard = (
			  <Storyboard data-uid='sb'>
				<Group data-uid='group' style={{ position: 'absolute', left: 100, top: 100, width: 187, height: 116, backgroundColor: 'white' }}>
					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'relative', left: 0, top: 0, width: 68, height: 46 }} />
					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 119, top: 70, width: 68, height: 46 }} />
				</Group>
			  </Storyboard>
			)
		`),
      )

      expect(renderResult.getEditorState().editor.toasts).toHaveLength(1)
      expect(renderResult.getEditorState().editor.toasts[0].message).toEqual(
        `Group children must be positioned absolutely`,
      )
      expect(renderResult.getEditorState().editor.toasts[0].level).toEqual('ERROR')
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
        const widthControl = editor.renderedDOM.getByTestId('frame-width-number-input')
        expect((widthControl as HTMLInputElement).value).toEqual('326')
        const heightControl = editor.renderedDOM.getByTestId('frame-height-number-input')
        expect((heightControl as HTMLInputElement).value).toEqual('407')
      }

      const groupGlobalFrame = await getGlobalFrame(editor, EP.fromString('sb/grandparent/parent'))
      expect(groupGlobalFrame.width).toBe(326)
      expect(groupGlobalFrame.height).toBe(407)

      {
        const widthControl = editor.renderedDOM.getByTestId('frame-width-number-input')
        expect((widthControl as HTMLInputElement).value).toEqual('326')
        const heightControl = editor.renderedDOM.getByTestId('frame-height-number-input')
        expect((heightControl as HTMLInputElement).value).toEqual('407')
      }
    })

    it('changing the fixed size of a group resize its children', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                bottom: 100,
                right: 100,
                width: 100,
                height: 100,
              }}
            />
            <Group data-uid='inner-group' style={{position: 'absolute', bottom: 0, right: 0}}>
              <div 
                data-uid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        </div>
        `),
        'await-first-dom-report',
      )

      const GroupPath = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/group`

      await selectComponentsForTest(editor, [EP.fromString(GroupPath)])

      const groupDiv = editor.renderedDOM.getByTestId('group')
      expect(groupDiv.style.width).toEqual('200px')

      const control = editor.renderedDOM.getByTestId('frame-width-number-input')
      await mouseClickAtPoint(control, { x: 5, y: 5 })
      await expectSingleUndo2Saves(editor, async () => {
        act(() => {
          fireEvent.change(control, { target: { value: '300' } })
          fireEvent.blur(control)
        })
      })

      await editor.getDispatchFollowUpActionsFinished()

      expect(groupDiv.style.width).toEqual('300px')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
        <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50, width: 300}}>
          <div 
            data-uid='child-1'
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              bottom: 100,
              right: 150,
              width: 150,
              height: 100,
            }}
          />
          <Group data-uid='inner-group' style={{position: 'absolute', bottom: 0, right: 0}}>
            <div 
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 150,
                height: 100,
              }}
            />
          </Group>
        </Group>
      </div>
      `),
      )
    })

    it('changing a right-pinned child correctly updates the group ancestors', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                bottom: 100,
                right: 100,
                width: 100,
                height: 100,
              }}
            />
            <Group data-uid='inner-group' style={{position: 'absolute', bottom: 0, right: 0}}>
              <div 
                data-uid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        </div>
        `),
        'await-first-dom-report',
      )

      const TargetPath = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/group/child-1`

      await selectComponentsForTest(editor, [EP.fromString(TargetPath)])

      const groupDiv = editor.renderedDOM.getByTestId('group')
      expect(groupDiv.style.width).toEqual('200px')

      const control = editor.renderedDOM.getByTestId('frame-left-number-input')
      await mouseClickAtPoint(control, { x: 5, y: 5 })
      await expectSingleUndo2Saves(editor, async () => {
        act(() => {
          fireEvent.change(control, { target: { value: '-50' } })
          fireEvent.blur(control)
        })
      })

      await editor.getDispatchFollowUpActionsFinished()

      expect(groupDiv.style.width).toEqual('250px')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
      <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
        <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 0, top: 50}}>
          <div 
            data-uid='child-1'
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              bottom: 100,
              right: 150,
              width: 100,
              height: 100,
            }}
          />
          <Group data-uid='inner-group' style={{position: 'absolute', bottom: 0, right: 0}}>
            <div 
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        </Group>
      </div>
      `),
      )
    })

    it('changing the fixed size of a group inside a group resize its children and the group parent', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                bottom: 100,
                right: 100,
                width: 100,
                height: 100,
              }}
            />
            <Group data-uid='inner-group' style={{position: 'absolute', top: 100, left: 100}}>
              <div 
                data-uid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        </div>
        `),
        'await-first-dom-report',
      )

      const InnerGroupPath = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/group/inner-group`

      await selectComponentsForTest(editor, [EP.fromString(InnerGroupPath)])

      const groupDiv = editor.renderedDOM.getByTestId('group')
      expect(groupDiv.style.width).toEqual('200px')

      const control = editor.renderedDOM.getByTestId('frame-height-number-input')
      await mouseClickAtPoint(control, { x: 5, y: 5 })
      await expectSingleUndo2Saves(editor, async () => {
        act(() => {
          fireEvent.change(control, { target: { value: '150' } })
          fireEvent.blur(control)
        })
      })

      await editor.getDispatchFollowUpActionsFinished()

      expect(groupDiv.style.height).toEqual('250px')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                bottom: 150,
                right: 100,
                width: 100,
                height: 100,
              }}
            />
            <Group data-uid='inner-group' style={{position: 'absolute', top: 100, left: 100, height: 150}}>
              <div 
                data-uid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 150,
                }}
              />
            </Group>
          </Group>
        </div>
      `),
      )
    })

    it('changing the fixed size of a child inside a nested group resizes both group parents', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                bottom: 100,
                right: 100,
                width: 100,
                height: 100,
              }}
            />
            <Group data-uid='inner-group' style={{position: 'absolute', top: 100, left: 100}}>
              <div 
                data-uid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        </div>
        `),
        'await-first-dom-report',
      )

      const InnerChildPath = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/group/inner-group/child-2`

      await selectComponentsForTest(editor, [EP.fromString(InnerChildPath)])

      const groupDiv = editor.renderedDOM.getByTestId('group')
      expect(groupDiv.style.width).toEqual('200px')

      const control = editor.renderedDOM.getByTestId('frame-width-number-input')
      await mouseClickAtPoint(control, { x: 5, y: 5 })
      await expectSingleUndo2Saves(editor, async () => {
        act(() => {
          fireEvent.change(control, { target: { value: '200' } })
          fireEvent.blur(control)
        })
      })

      await editor.getDispatchFollowUpActionsFinished()

      expect(groupDiv.style.width).toEqual('300px')

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                bottom: 100,
                right: 200,
                width: 100,
                height: 100,
              }}
            />
            <Group data-uid='inner-group' style={{position: 'absolute', top: 100, left: 100}}>
              <div 
                data-uid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 200,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        </div>
      `),
      )
    })

    it('changing the fixed size of a group child to % refuses to work', async () => {
      const startingProject = `
      <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
        <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
          <div 
            data-uid='child-1'
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              bottom: 100,
              right: 100,
              width: 100,
              height: 100,
            }}
          />
          <Group data-uid='inner-group' style={{position: 'absolute', top: 100, left: 100}}>
            <div 
              data-uid='child-2'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        </Group>
      </div>
      `

      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startingProject),
        'await-first-dom-report',
      )

      const ChildPath = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/group/child-1`

      await selectComponentsForTest(editor, [EP.fromString(ChildPath)])

      const groupDiv = editor.renderedDOM.getByTestId('group')
      expect(groupDiv.style.width).toEqual('200px')

      const widthControl = editor.renderedDOM.getByTestId('frame-width-number-input')
      await mouseClickAtPoint(widthControl, { x: 5, y: 5 })
      await expectNoAction(editor, async () => {
        act(() => {
          fireEvent.change(widthControl, { target: { value: '75%' } })
          fireEvent.blur(widthControl)
        })
      })

      await editor.getDispatchFollowUpActionsFinished()

      expect(groupDiv.style.width).toEqual('200px')

      // expect that nothing changed
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(startingProject),
      )

      const heightControl = editor.renderedDOM.getByTestId('frame-height-number-input')
      await mouseClickAtPoint(widthControl, { x: 5, y: 5 })
      await expectNoAction(editor, async () => {
        act(() => {
          fireEvent.change(heightControl, { target: { value: '75%' } })
          fireEvent.blur(heightControl)
        })
      })

      await editor.getDispatchFollowUpActionsFinished()

      expect(groupDiv.style.height).toEqual('200px')

      // expect that nothing changed
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(startingProject),
      )
    })

    it('Changing the crazy % pin of a group childis allowed', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                bottom: 100,
                right: 100,
                width: '50%',
                height: 100,
              }}
            />
            <Group data-uid='inner-group' style={{position: 'absolute', top: 100, left: 100}}>
              <div 
                data-uid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        </div>
        `),
        'await-first-dom-report',
      )

      const ChildPath = `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/group/child-1`

      await selectComponentsForTest(editor, [EP.fromString(ChildPath)])

      const groupDiv = editor.renderedDOM.getByTestId('group')
      expect(groupDiv.style.width).toEqual('200px')

      const control = editor.renderedDOM.getByTestId('frame-width-number-input')
      await mouseClickAtPoint(control, { x: 5, y: 5 })

      await expectSingleUndo2Saves(editor, async () => {
        act(() => {
          fireEvent.change(control, { target: { value: '110' } })
          fireEvent.blur(control)
        })
      })

      await editor.getDispatchFollowUpActionsFinished()

      expect(groupDiv.style.width).toEqual('200px')

      // expect that nothing changed
      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
          <Group data-uid='group' data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              data-uid='child-1'
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                bottom: 100,
                right: 90,
                width: '55%',
                height: 100,
              }}
            />
            <Group data-uid='inner-group' style={{position: 'absolute', top: 100, left: 100}}>
              <div 
                data-uid='child-2'
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
            </Group>
          </Group>
        </div>
        `),
      )
    })
  })

  describe('detected size', () => {
    const project = `
    <div
    style={{
      backgroundColor: '#a6c0dc',
      position: 'absolute',
      left: 279,
      top: 609,
      width: 415,
      height: 195,
    }}
    data-uid='container'
  >
    <span
      style={{
        position: 'absolute',
        wordBreak: 'break-word',
        left: 51,
        top: 60,
      }}
      data-uid='text'
      data-testid='text'
    >
      whaddup
    </span>
    <div
      style={{
        backgroundColor: '#4326c5',
        position: 'absolute',
        left: 166,
        top: 49,
        display: 'flex',
        flexDirection: 'row',
        padding: '20px 20.5px',
      }}
      data-uid='flex-parent'
      data-testid='flex-parent'
    >
      <div
        style={{
          backgroundColor: '#fc6314',
          width: 117,
          height: 82,
          contain: 'layout',
        }}
        data-uid='flex-child'
      />
    </div>
  </div>
    `
    it('element with no explicit size is classified as `Detected` height and `Hug contents` width', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(project),
        'await-first-dom-report',
      )

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/text`),
      ])

      expect(editor.renderedDOM.getAllByText(DetectedLabel).length).toEqual(1)
      expect(editor.renderedDOM.getAllByText(HugContentsLabel).length).toEqual(1)

      await selectComponentsForTest(editor, [
        EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/flex-parent`,
        ),
      ])

      expect(editor.renderedDOM.getAllByText(HugContentsLabel).length).toEqual(2)

      await selectComponentsForTest(editor, [
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}`),
      ])

      await mouseClickAtPoint(editor.renderedDOM.getByTestId('section-header-Styles'), {
        x: 3,
        y: 3,
      })

      expect(editor.renderedDOM.getAllByText(DetectedLabel).length).toEqual(2)
    })

    it('can set from detected and hug to fixed size', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(project),
        'await-first-dom-report',
      )

      {
        await selectComponentsForTest(editor, [
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/text`),
        ])

        const control = (await editor.renderedDOM.findAllByText(DetectedLabel))[0]

        await mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(FixedLabel))[0]
        await expectSingleUndo2Saves(editor, async () => {
          await mouseClickAtPoint(button, { x: 5, y: 5 })
        })

        const text = editor.renderedDOM.getByTestId('text')

        expect(text.style.width).toEqual('59px')
      }

      {
        await selectComponentsForTest(editor, [
          EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container/flex-parent`,
          ),
        ])

        const control = (await editor.renderedDOM.findAllByText(HugContentsLabel))[0]

        await mouseClickAtPoint(control, { x: 5, y: 5 })

        const button = (await editor.renderedDOM.findAllByText(FixedLabel))[0]
        await expectSingleUndo2Saves(editor, async () => {
          await mouseClickAtPoint(button, { x: 5, y: 5 })
        })

        const text = editor.renderedDOM.getByTestId('flex-parent')

        expect(text.style.width).toEqual('158px')
      }
    })
  })

  describe('computed', () => {
    const project = (widthValue: string) => `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const App = () => {
  const width = 44
  const height = 33

  return (
    <div
      data-uid='root'
      style={{
        position: 'absolute',
        width: ${widthValue},
        height: height,
        left: 100,
        top: 100,
        backgroundColor: '#cfe5ff',
      }}
    />
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 200,
        height: 300,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-uid='scene'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)
`

    it('width/height for element with code coming from props is classified as computed', async () => {
      const editor = await renderTestEditorWithCode(project('width'), 'await-first-dom-report')

      await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root`)])

      const computedControls = await editor.renderedDOM.findAllByText(ComputedLabel)
      expect(computedControls.length).toEqual(2) // both width and height set to `Computed`
    })

    it('can toggle from computed to fixed', async () => {
      const editor = await renderTestEditorWithCode(project('width'), 'await-first-dom-report')

      await selectComponentsForTest(editor, [EP.fromString(`sb/scene/app:root`)])

      const control = (await editor.renderedDOM.findAllByText(ComputedLabel))[0]

      await mouseClickAtPoint(control, { x: 5, y: 5 })

      const button = (await editor.renderedDOM.findAllByText(FixedLabel))[0]
      await expectSingleUndo2Saves(editor, async () => {
        await mouseClickAtPoint(button, { x: 5, y: 5 })
      })

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(project('44'))
    })
  })
})

describe('Fixed/hug on text elements', () => {
  it('detects auto sizing settings', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      height: '100%',
      width: '100%',
      contain: 'layout',
    }}
    data-uid='root'
  >
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 75,
        top: 122,
        width: 255,
        height: 72,
      }}
      data-uid='hug-x-hug'
    >
      <span
        style={{
          position: 'absolute',
          wordBreak: 'break-word',
          left: 53,
          top: 26,
          width: 'max-content',
          height: 'max-content',
        }}
        data-uid='hug-x-hug-text'
      >
        Hug x Hug
      </span>
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 75,
        top: 214,
        width: 255,
        height: 72,
      }}
      data-uid='auto-width'
    >
      <span
        style={{
          position: 'absolute',
          wordBreak: 'break-word',
          left: 53,
          top: 27,
          width: 'max-content',
          height: 'max-content',
        }}
        data-uid='auto-width-text'
      >
        Auto Width
      </span>
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 75,
        top: 316,
        width: 255,
        height: 72,
      }}
      data-uid='auto-height'
    >
      <span
        style={{
          position: 'absolute',
          wordBreak: 'break-word',
          left: 53,
          top: 27,
          width: 85.8828125,
          height: 'max-content',
        }}
        data-uid='auto-height-text'
      >
        Auto Height
      </span>
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 75,
        top: 411,
        width: 255,
        height: 72,
      }}
      data-uid='fixed-size'
    >
      <span
        style={{
          position: 'absolute',
          wordBreak: 'break-word',
          left: 53,
          top: 27,
          height: 19,
          width: 86,
        }}
        data-uid='fixed-size-text'
      >
        Fixed Width
      </span>
    </div>
  </div>`),
      'await-first-dom-report',
    )

    const settings = [
      ['hug-x-hug', 'auto-width'],
      ['auto-width', 'auto-width'],
      ['auto-height', 'auto-height'],
      ['fixed-size', 'fixed-size'],
    ] as const

    for (const [uid, expectedSetting] of settings) {
      await selectComponentsForTest(renderResult, [
        EP.appendNewElementPath(TestScenePath, ['root', uid, `${uid}-text`]),
      ])
      const { jsxMetadata, elementPathTree, selectedViews } = renderResult.getEditorState().editor
      const setting = detectTextSizingStateMultiSelect(jsxMetadata, elementPathTree, selectedViews)
      expect(setting).toEqual(expectedSetting)
    }
    {
      await selectComponentsForTest(renderResult, [])
      const { jsxMetadata, elementPathTree, selectedViews } = renderResult.getEditorState().editor
      const disabledSetting = detectTextSizingStateMultiSelect(
        jsxMetadata,
        elementPathTree,
        selectedViews,
      )
      expect(disabledSetting).toEqual('disabled')
    }

    {
      await selectComponentsForTest(
        renderResult,
        settings.map(([uid]) =>
          EP.appendNewElementPath(TestScenePath, ['root', uid, `${uid}-text`]),
        ),
      )

      const { jsxMetadata, elementPathTree, selectedViews } = renderResult.getEditorState().editor
      const mixedSetting = detectTextSizingStateMultiSelect(
        jsxMetadata,
        elementPathTree,
        selectedViews,
      )
      expect(mixedSetting).toEqual('mixed')
    }
  })
  it('Sets text element from fixed to auto-width inside the font section', async () => {
    const testCode = `
    <div style={{ ...props.style }} data-uid='aaa'>
      <div
        style={{ position: 'absolute', left: 40, top: 20, width: 100, height: 25, fontSize: 15 }}
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
          style={{ position: 'absolute', left: 40, top: 20, width: 'max-content', height: 25, fontSize: 15 }}
          data-uid='bbb'
          data-testid='bbb'
        >hello text element!</div>
      </div>
      `),
    )
  })
  it('Sets text element from fixed to auto-height inside the font section', async () => {
    const testCode = `
    <div style={{ ...props.style }} data-uid='aaa'>
      <div
        style={{ position: 'absolute', left: 40, top: 20, width: 100, height: 25, fontSize: 15 }}
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
          style={{ position: 'absolute', left: 40, top: 20, width: 100, height: 'max-content', fontSize: 15 }}
          data-uid='bbb'
          data-testid='bbb'
        >hello text element!</div>
      </div>
      `),
    )
  })
  it('Sets text element from auto-height to fixed inside the font section', async () => {
    const testCode = `
    <div style={{ ...props.style }} data-uid='aaa'>
        <div
          style={{ position: 'absolute', left: 40, top: 20, width: 100, height: 'max-content', fontSize: 15 }}
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

    const textFixedSizeIcon = editor.renderedDOM.getByTestId(`${TextAutoSizingTestId}-2`)
    await expectSingleUndo2Saves(editor, async () => {
      await mouseClickAtPoint(textFixedSizeIcon, { x: 2, y: 2 })
    })

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div
          style={{ position: 'absolute', left: 40, top: 20, width: 100, height: 34, fontSize: 15 }}
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
