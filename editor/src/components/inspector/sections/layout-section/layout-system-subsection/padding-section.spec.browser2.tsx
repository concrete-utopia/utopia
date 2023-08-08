import type { RenderResult } from '@testing-library/react'
import { act, fireEvent, screen } from '@testing-library/react'
import * as EP from '../../../../../core/shared/element-path'
import {
  expectSingleUndo2Saves,
  selectComponentsForTest,
} from '../../../../../utils/utils.test-utils'
import { getSubduedPaddingControlTestID } from '../../../../canvas/controls/select-mode/subdued-padding-control'
import type { EditorRenderResult } from '../../../../canvas/ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../../../editor/actions/action-creators'

async function getControl(
  controlTestId: string,
  renderedDOM: RenderResult,
): Promise<HTMLInputElement> {
  return (await renderedDOM.findByTestId(controlTestId)) as HTMLInputElement
}

async function setControlValue(
  controlTestId: string,
  newValue: string,
  renderedDOM: RenderResult,
): Promise<void> {
  const control = await getControl(controlTestId, renderedDOM)

  await act(() => {
    fireEvent.focus(control)
    fireEvent.change(control, { target: { value: newValue } })
    fireEvent.blur(control)
  })
}

describe('padding controls shorthand', () => {
  function makeCodeSnippetWithKeyValue(props: { [key: string]: any }): string {
    const propsStr = Object.keys(props)
      .map((k) => `${k}: ${JSON.stringify(props[k])},`)
      .join('\n')
    return `
      <div
        data-uid='aaa'
      >
        <div
          style={{ boxSizing: 'border-box', ${propsStr} }}
          data-uid='bbb'
        >
          <div
            style={{ width: 100, height: 100 }}
            data-uid='ccc'
          />
        </div>
      </div>
  `
  }

  const tests = [
    {
      name: 'without props',
      startSnippet: makeCodeSnippetWithKeyValue({ width: 100, height: 100 }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-V', '20', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({ width: 100, height: 140, padding: '20px 0px' }),
    },
    {
      name: 'with shorthand',
      startSnippet: makeCodeSnippetWithKeyValue({ width: 120, height: 120, padding: 10 }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-one', '20', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({ width: 140, height: 140, padding: 20 }),
    },
    {
      name: 'with single value (2-values)',
      startSnippet: makeCodeSnippetWithKeyValue({ width: 110, height: 100, paddingLeft: 10 }),
      control: async (renderResult: EditorRenderResult) => {
        await setControlValue('padding-V', '20', renderResult.renderedDOM)
      },
      endSnippet: makeCodeSnippetWithKeyValue({
        width: 110,
        height: 140,
        paddingLeft: 10,
        paddingTop: 20,
        paddingBottom: 20,
      }),
    },
    {
      name: 'with single value (1-value)',
      startSnippet: makeCodeSnippetWithKeyValue({ width: 110, height: 100, paddingLeft: 10 }),
      before: async (renderResult: EditorRenderResult) => {
        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })

        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })
      },
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-one', '20', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({
        width: 140,
        height: 140,
        paddingLeft: 20,
        paddingTop: 20,
        paddingRight: 20,
        paddingBottom: 20,
      }),
    },
    {
      name: 'with multiple values (1-value)',
      startSnippet: makeCodeSnippetWithKeyValue({
        width: 130,
        height: 100,
        paddingLeft: 10,
        paddingRight: 20,
      }),
      before: async (renderResult: EditorRenderResult) => {
        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })

        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })
      },
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-one', '20', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({
        width: 140,
        height: 140,
        paddingLeft: 20,
        paddingRight: 20,
        paddingTop: 20,
        paddingBottom: 20,
      }),
    },
    {
      name: 'with shorthand (2-value)',
      startSnippet: makeCodeSnippetWithKeyValue({ width: 120, height: 120, padding: 10 }),
      before: async (renderResult: EditorRenderResult) => {
        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })
      },
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-H', '20', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({ width: 140, height: 120, padding: '10px 20px' }),
    },
    {
      name: 'single value with no other values set',
      startSnippet: makeCodeSnippetWithKeyValue({ width: 100, height: 100 }),
      before: async (renderResult: EditorRenderResult) => {
        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })
      },
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-R', '20', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({ width: 120, height: 100, paddingRight: 20 }),
    },
    {
      name: 'single value with shorthand set',
      startSnippet: makeCodeSnippetWithKeyValue({ width: 120, height: 120, padding: 10 }),
      before: async (renderResult: EditorRenderResult) => {
        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })

        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })
      },
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-R', '20', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({
        width: 130,
        height: 120,
        padding: '10px 20px 10px 10px',
      }),
    },
    {
      name: 'delete value (shorthand, one value)',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: 10 }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-one', '', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({}),
    },
    {
      name: 'delete value (longhand, one value)',
      startSnippet: makeCodeSnippetWithKeyValue({
        paddingLeft: 10,
        paddingRight: 10,
        paddingTop: 10,
        paddingBottom: 10,
      }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-one', '', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({}),
    },
    {
      name: 'delete value (shorthand, two value)',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '10px 20px' }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-H', '', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({ padding: '10px 0px' }),
    },
    {
      name: 'delete value (longhand, two value)',
      startSnippet: makeCodeSnippetWithKeyValue({
        paddingLeft: 10,
        paddingRight: 10,
        paddingTop: 20,
        paddingBottom: 20,
      }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-H', '', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({
        paddingTop: 20,
        paddingBottom: 20,
      }),
    },
    {
      name: 'delete value (shorthand, two value, all empty)',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '0px 10px' }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-H', '', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({}),
    },
    {
      name: 'delete value (shorthand, four value)',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '10px 20px 30px 40px' }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-R', '', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({ padding: '10px 0px 30px 40px' }),
    },
    {
      name: 'delete value (shorthand, four value, all empty)',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '0px 20px 0px 0px' }),
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-R', '', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({ padding: '0px 0px 0px 0px' }),
    },
    {
      name: 'delete value (longhand, four value)',
      startSnippet: makeCodeSnippetWithKeyValue({
        paddingLeft: 10,
        paddingTop: 20,
      }),
      before: async (renderResult: EditorRenderResult) => {
        await act(async () => {
          fireEvent.click(screen.getByTestId('padding-cycle-mode'))
          await renderResult.getDispatchFollowUpActionsFinished()
        })
      },
      control: async (renderResult: EditorRenderResult) => {
        await expectSingleUndo2Saves(renderResult, async () => {
          await setControlValue('padding-T', '', renderResult.renderedDOM)
        })
      },
      endSnippet: makeCodeSnippetWithKeyValue({ paddingLeft: 10 }),
    },
  ]

  tests.forEach((tt, idx) => {
    it(`(${idx + 1}) padding controls shorthand: ${tt.name}`, async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(tt.startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

      await selectComponentsForTest(renderResult, [targetPath])

      if (tt.before != null) {
        await tt.before(renderResult)
        await renderResult.getDispatchFollowUpActionsFinished()
      }
      await tt.control(renderResult)

      await selectComponentsForTest(renderResult, [targetPath])

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(tt.endSnippet),
      )
    })
  })
})

describe('canvas padding controls from the inspector', () => {
  function makeCodeSnippetWithKeyValue(props: { [key: string]: any }): string {
    const propsStr = Object.keys(props)
      .map((k) => `${k}: ${JSON.stringify(props[k])},`)
      .join('\n')
    return `
      <div
        data-uid='aaa'
      >
        <div
          style={{ ${propsStr} }}
          data-uid='bbb'
        >test</div>
      </div>
  `
  }

  const tests = [
    {
      name: 'single value shows controls on all sides',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '50px' }),
      controlTestID: 'padding-one',
      hoveredCanvasControls: [
        getSubduedPaddingControlTestID('top', 'hovered'),
        getSubduedPaddingControlTestID('right', 'hovered'),
        getSubduedPaddingControlTestID('bottom', 'hovered'),
        getSubduedPaddingControlTestID('left', 'hovered'),
      ],
      focusedCanvasControls: [
        getSubduedPaddingControlTestID('top', 'focused'),
        getSubduedPaddingControlTestID('right', 'focused'),
        getSubduedPaddingControlTestID('bottom', 'focused'),
        getSubduedPaddingControlTestID('left', 'focused'),
      ],
    },
    {
      name: 'per-direction H value shows controls on horizontal sides',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '50px 60px' }),
      controlTestID: 'padding-H',
      hoveredCanvasControls: [
        getSubduedPaddingControlTestID('right', 'hovered'),
        getSubduedPaddingControlTestID('left', 'hovered'),
      ],
      focusedCanvasControls: [
        getSubduedPaddingControlTestID('right', 'focused'),
        getSubduedPaddingControlTestID('left', 'focused'),
      ],
    },
    {
      name: 'per-direction V value shows controls on vertical sides',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '50px 60px' }),
      controlTestID: 'padding-V',
      hoveredCanvasControls: [
        getSubduedPaddingControlTestID('top', 'hovered'),
        getSubduedPaddingControlTestID('bottom', 'hovered'),
      ],
      focusedCanvasControls: [
        getSubduedPaddingControlTestID('top', 'focused'),
        getSubduedPaddingControlTestID('bottom', 'focused'),
      ],
    },
    {
      name: 'per-side T value shows controls on top side',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '50px 60px 70px 80px' }),
      controlTestID: 'padding-T',
      hoveredCanvasControls: [getSubduedPaddingControlTestID('top', 'hovered')],
      focusedCanvasControls: [getSubduedPaddingControlTestID('top', 'focused')],
    },
    {
      name: 'per-side R value shows controls on right side',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '50px 60px 70px 80px' }),
      controlTestID: 'padding-R',
      hoveredCanvasControls: [getSubduedPaddingControlTestID('right', 'hovered')],
      focusedCanvasControls: [getSubduedPaddingControlTestID('right', 'focused')],
    },
    {
      name: 'per-side B value shows controls on bottom side',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '50px 60px 70px 80px' }),
      controlTestID: 'padding-B',
      hoveredCanvasControls: [getSubduedPaddingControlTestID('bottom', 'hovered')],
      focusedCanvasControls: [getSubduedPaddingControlTestID('bottom', 'focused')],
    },
    {
      name: 'per-side L value shows controls on left side',
      startSnippet: makeCodeSnippetWithKeyValue({ padding: '50px 60px 70px 80px' }),
      controlTestID: 'padding-L',
      hoveredCanvasControls: [getSubduedPaddingControlTestID('left', 'hovered')],
      focusedCanvasControls: [getSubduedPaddingControlTestID('left', 'focused')],
    },
  ]

  tests.forEach((t) => {
    it(`${t.name} when hovering and focusing`, async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(t.startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      const control = await getControl(t.controlTestID, renderResult.renderedDOM)

      // Check the controls show when hovering
      fireEvent.mouseEnter(control)
      await renderResult.getDispatchFollowUpActionsFinished()

      const hoveredControls = t.hoveredCanvasControls.flatMap((expectedControl) =>
        renderResult.renderedDOM.queryAllByTestId(expectedControl),
      )
      expect(hoveredControls.length).toEqual(t.hoveredCanvasControls.length)

      // Check the controls show when focusing
      fireEvent.focus(control)
      await renderResult.getDispatchFollowUpActionsFinished()

      const focusedControls = t.focusedCanvasControls.flatMap((expectedControl) =>
        renderResult.renderedDOM.queryAllByTestId(expectedControl),
      )
      expect(focusedControls.length).toEqual(t.focusedCanvasControls.length)
    })
  })
})
