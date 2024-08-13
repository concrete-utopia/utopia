/* eslint-disable jest/expect-expect */
import type { RenderResult } from '@testing-library/react'
import { act, fireEvent, screen } from '@testing-library/react'
import * as Prettier from 'prettier/standalone'
import { PrettierConfig } from 'utopia-vscode-common'
import { matchInlineSnapshotBrowser } from '../../../../test/karma-snapshots'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath, ProjectContents } from '../../../core/shared/project-file-types'
import { directory } from '../../../core/shared/project-file-types'
import {
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../../../core/shared/project-file-types'
import {
  expectSingleUndo2Saves,
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
  wait,
} from '../../../utils/utils.test-utils'
import { contentsToTree } from '../../assets'
import { SubduedBorderRadiusControlTestId } from '../../canvas/controls/select-mode/subdued-border-radius-control'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  makeTestProjectCodeWithSnippetStyledComponents,
  renderTestEditorWithCode,
  renderTestEditorWithProjectContent,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../canvas/ui-jsx.test-utils'
import { createCodeFile } from '../../custom-code/code-file.test-utils'
import type { EditorAction } from '../../editor/action-types'
import { selectComponents } from '../../editor/actions/action-creators'
import { DefaultPackageJson, StoryboardFilePath } from '../../editor/store/editor-state'
import {
  ConditionalOverrideControlToggleTestId,
  ConditionalOverrideControlTestIdPrefix,
} from '../controls/conditional-override-control'
import { getOptionControlTestId } from '../controls/option-chain-control'
import {
  ConditionalsControlBranchFalseTestId,
  ConditionalsControlBranchTrueTestId,
  ConditionalsControlSectionExpressionTestId,
  ConditionalsControlSectionOpenTestId,
  ConditionalsControlSwitchBranchesTestId,
} from '../sections/layout-section/conditional-section'
import {
  expectSelectControlValue,
  mouseClickAtPoint,
  mouseDownAtPoint,
  pickFromReactSelectPopupList,
  pressKey,
} from '../../canvas/event-helpers.test-utils'
import {
  sendLinterRequestMessage,
  updateFromCodeEditor,
} from '../../editor/actions/actions-from-vscode'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { InvalidGroupState } from '../../canvas/canvas-strategies/strategies/group-helpers'
import { invalidGroupStateToString } from '../../canvas/canvas-strategies/strategies/group-helpers'
import selectEvent from 'react-select-event'
import { MixedPlaceholder } from '../../../uuiui/inputs/base-input'
import { cmdModifier } from '../../../utils/modifiers'

async function getControl(
  controlTestId: string,
  renderedDOM: RenderResult,
): Promise<HTMLInputElement> {
  return (await renderedDOM.findByTestId(controlTestId)) as HTMLInputElement
}

const ConditionalOverrideControlTrueTestId = getOptionControlTestId(
  ConditionalOverrideControlTestIdPrefix,
  'true',
)
const ConditionalOverrideControlFalseTestId = getOptionControlTestId(
  ConditionalOverrideControlTestIdPrefix,
  'false',
)

async function getControlValue(controlTestId: string, renderedDOM: RenderResult): Promise<string> {
  const control = await getControl(controlTestId, renderedDOM)

  return control.value
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

async function dispatchActionsAndWaitUntilComplete(
  actionsToDispatch: readonly EditorAction[],
  renderResult: EditorRenderResult,
): Promise<void> {
  await act(() => renderResult.dispatch(actionsToDispatch, false))
  await renderResult.getDispatchFollowUpActionsFinished()
}

async function selectElement(
  targetPath: ElementPath,
  renderResult: EditorRenderResult,
): Promise<void> {
  return dispatchActionsAndWaitUntilComplete([selectComponents([targetPath], false)], renderResult)
}

function actionsForUpdatedCode(updatedCodeSnippet: string) {
  const fullCode = makeTestProjectCodeWithSnippet(updatedCodeSnippet)
  const updateAction = updateFromCodeEditor(StoryboardFilePath, fullCode, null)
  const requestLintAction = sendLinterRequestMessage(StoryboardFilePath, fullCode)
  return [updateAction, requestLintAction]
}

async function clickButtonAndSelectTarget(
  renderResult: EditorRenderResult,
  buttonTestId: string,
  targetPath: ElementPath[],
): Promise<void> {
  await expectSingleUndo2Saves(renderResult, async () => {
    await act(async () => {
      fireEvent.click(screen.getByTestId(buttonTestId))
      await renderResult.getDispatchFollowUpActionsFinished()
    })
  })

  await act(async () => {
    const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
    await renderResult.dispatch([selectComponents(targetPath, false)], true)
    await dispatchDone
  })

  await renderResult.getDispatchFollowUpActionsFinished()
}

async function pressKeyTimes(
  target: HTMLElement,
  renderResult: EditorRenderResult,
  keys: string[],
) {
  return act(async () => {
    target.focus()
    await Promise.all(keys.map((key) => pressKey(key, { targetElement: target })))
    target.blur()
    await renderResult.getDispatchFollowUpActionsFinished()
  })
}

describe('inspector tests with real metadata', () => {
  it('padding controls', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              position: 'fixed',
              padding: 20,
              paddingLeft: 15,
              left: 55,
              top: 98,
              width: 266,
              height: 124,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const paddingTopControl = (await renderResult.renderedDOM.findByTestId(
      'padding-T',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement

    // Padding top is coming from the shorthand `padding` value.
    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingTop'], `"20px"`)
    matchInlineSnapshotBrowser(paddingTopControl.value, `"20"`)
    matchInlineSnapshotBrowser(
      paddingTopControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    // Padding left is coming from the `paddingLeft` value.
    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingLeft'], `"15px"`)
    matchInlineSnapshotBrowser(paddingLeftControl.value, `"15"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('TLWH layout controls', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              left: 55,
              top: 98,
              width: 266,
              height: 124,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['width'], `"266px"`)
    matchInlineSnapshotBrowser(widthControl.value, `"266"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['height'], `"124px"`)
    matchInlineSnapshotBrowser(heightControl.value, `"124"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Left')
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Top')

    matchInlineSnapshotBrowser(metadata.computedStyle?.['top'], `"98px"`)
    matchInlineSnapshotBrowser(topControl.value, `"98"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['left'], `"55px"`)
    matchInlineSnapshotBrowser(leftControl.value, `"55"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('TLWH layout controls in multiselect', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              left: 55,
              top: 98,
              width: 266,
              height: 124,
            }}
            data-uid={'bbb'}
          ></div>
          <div
          style={{
            position: 'absolute',
            backgroundColor: '#DDDDDD',
            left: 100,
            top: 100,
            width: 150,
            height: 160,
          }}
          data-uid={'ccc'}
        ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath1 = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])
    const targetPath2 = EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath1, targetPath2], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath1)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['width'], `"266px"`)
    matchInlineSnapshotBrowser(widthControl.value, '""')
    matchInlineSnapshotBrowser(widthControl.placeholder, `"${MixedPlaceholder}"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"multiselect-mixed-simple-or-unset"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['height'], `"124px"`)
    matchInlineSnapshotBrowser(heightControl.value, `""`)
    matchInlineSnapshotBrowser(heightControl.placeholder, `"${MixedPlaceholder}"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"multiselect-mixed-simple-or-unset"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Left')
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Top')

    matchInlineSnapshotBrowser(metadata.computedStyle?.['top'], `"98px"`)
    matchInlineSnapshotBrowser(topControl.value, `""`)
    matchInlineSnapshotBrowser(topControl.placeholder, `"${MixedPlaceholder}"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"multiselect-mixed-simple-or-unset"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['left'], `"55px"`)
    matchInlineSnapshotBrowser(leftControl.value, `""`)
    matchInlineSnapshotBrowser(leftControl.placeholder, `"${MixedPlaceholder}"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"multiselect-mixed-simple-or-unset"`,
    )

    // Ensure that changing a value during multiselect doesn't cause duplicate UIDs
    await setControlValue('frame-top-number-input', '10', renderResult.renderedDOM)
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(renderResult.getActionsCausingDuplicateUIDs().length).toEqual(0)
  })
  it('TLBR layout controls', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              left: 55,
              top: 98,
              bottom: 200,
              right: 10,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"335"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"102"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(
      renderResult,
      'frame-child-constraint-width-popuplist',
      'Left and Right',
    )
    expectSelectControlValue(
      renderResult,
      'frame-child-constraint-height-popuplist',
      'Top and Bottom',
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['top'], `"98px"`)
    matchInlineSnapshotBrowser(topControl.value, `"98"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['left'], `"55px"`)
    matchInlineSnapshotBrowser(leftControl.value, `"55"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('WHBR layout controls', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              width: 203,
              height: 102,
              bottom: 200,
              right: 10,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['width'], `"203px"`)
    matchInlineSnapshotBrowser(widthControl.value, `"203"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['height'], `"102px"`)
    matchInlineSnapshotBrowser(heightControl.value, `"102"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Right')
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Bottom')

    matchInlineSnapshotBrowser(topControl.value, `"98"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"187"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('TLWH layout controls non-px values', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              left: '2em',
              top: '1.4cm',
              width: '10vw',
              height: '124pt',
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"220"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"165.5"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed')
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Top')

    matchInlineSnapshotBrowser(topControl.value, `"53"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"32"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('Style props using numbers', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              width: 203,
              height: 102,
              padding: 16,
              paddingRight: 12,
              opacity: 0.5,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['width'], `"203px"`)
    matchInlineSnapshotBrowser(widthControl.value, `"203"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['height'], `"102px"`)
    matchInlineSnapshotBrowser(heightControl.value, `"102"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Left')
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Top')

    matchInlineSnapshotBrowser(paddingLeftControl.value, `"16"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingRightControl.value, `"12"`)
    matchInlineSnapshotBrowser(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(opacityControl.value, `"50%"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('Style props default value set inline', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: 'auto',
              left: 'auto',
              width: 'auto',
              height: 'auto',
              padding: 0,
              paddingRight: 0,
              borderRadius: 0,
              opacity: 1,
              minWidth: 0,
              maxWidth: 'none',
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })
    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement
    const paddingControl = (await renderResult.renderedDOM.findByTestId(
      'padding-one',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed') // this could be improved to be Left
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Mixed') // this could be improved to be Top

    matchInlineSnapshotBrowser(topControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      paddingControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(radiusControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(opacityControl.value, `"100%"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('Style props strings using px', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: '25px',
              left: '14px',
              width: '203px',
              height: '102px',
              padding: '4px',
              paddingRight: '8px',
              borderRadius: '2px',
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"203"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"102"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Left')
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Top')

    matchInlineSnapshotBrowser(topControl.value, `"25"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"14"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingLeftControl.value, `"4"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingRightControl.value, `"8"`)
    matchInlineSnapshotBrowser(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(radiusControl.value, `"2"`)

    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('Style props in %', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: '25%',
              left: '10%',
              width: '80%',
              height: '65%',
              padding: '4%',
              paddingRight: '8%',
              borderRadius: '50%',
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"320"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"260"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Scale')
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Scale')

    matchInlineSnapshotBrowser(topControl.value, `"100"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"40"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingLeftControl.value, `"4%"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingRightControl.value, `"8%"`)
    matchInlineSnapshotBrowser(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(radiusControl.value, `"50%"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('Style props using css calc()', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'xxx'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: 'calc(50% + 20px)',
              left: 'calc(50px + 50px)',
              width: 'calc(150px)',
              height: 'calc(10% + 30px)',
              padding: 'calc(10% + 4px)',
              paddingRight: 'calc(10% + 2px)',
              borderRadius: 'calc(10% + 5%)',
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['xxx', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"150"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"88"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed') // this could be improved to be Scale once we understand calc()
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Mixed') // this could be improved to be Top

    matchInlineSnapshotBrowser(topControl.value, `"220"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"100"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingLeftControl.value, `"44"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingRightControl.value, `"42"`)
    matchInlineSnapshotBrowser(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(radiusControl.value, `"15%"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )
  })
  it('Style props using a simple expression', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: 10+23,
              left: 50+24,
              width: 100+50,
              height: 30+100,
              padding: 2+2,
              paddingRight: 1+4,
              borderRadius: 5+2,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"150"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"130"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed') // this should be Left (in follow-up PR)
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Mixed') // this should be Top (in follow-up PR)

    matchInlineSnapshotBrowser(topControl.value, `"33"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"74"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingLeftControl.value, `"4"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )

    matchInlineSnapshotBrowser(paddingRightControl.value, `"5"`)
    matchInlineSnapshotBrowser(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )

    matchInlineSnapshotBrowser(radiusControl.value, `"7"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )
  })
  it('Style using react props', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View, Group } from 'utopia-api'

      export var App = (props) => {
        return (
          <div
            style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid={'div'}
          >
            <div
              style={{
                position: 'absolute',
                backgroundColor: '#DDDDDD',
                top: 100,
                left: props.left,
                width: 100,
                height: 50,
                padding: props.padding,
                paddingRight: props.paddingRight,
                borderRadius: props.border,
                opacity: props.opacity,
              }}
              data-uid={'bbb'}
            ></div>
          </div>
        )
      }

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              data-uid='${TestSceneUID}'
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
                padding={5}
                paddingRight={10}
                opacity={0.5}
                left={30}
                border={'50%'}
              />
            </Scene>
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['div', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'frame-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'frame-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"100"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"50"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Left')
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Top')

    matchInlineSnapshotBrowser(topControl.value, `"100"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"30"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingLeftControl.value, `"5"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )

    matchInlineSnapshotBrowser(paddingRightControl.value, `"10"`)
    matchInlineSnapshotBrowser(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['borderRadius'], `"50%"`)
    matchInlineSnapshotBrowser(radiusControl.value, `"50%"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['opacity'], `"0.5"`)
    matchInlineSnapshotBrowser(opacityControl.value, `"50%"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )
  })
  it('CSS props using numbers', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippetStyledComponents(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            css={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              width: 203,
              height: 102,
              padding: 16,
              paddingRight: 12,
              opacity: 0.5,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const earlyMetadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const earlyWidthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const earlyHeightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const earlyPaddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const earlyPaddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const earlyOpacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(earlyMetadata.computedStyle?.['width'], `"203px"`)
    matchInlineSnapshotBrowser(earlyWidthControl.value, `"203"`)
    matchInlineSnapshotBrowser(
      earlyWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(earlyMetadata.computedStyle?.['height'], `"102px"`)
    matchInlineSnapshotBrowser(earlyHeightControl.value, `"102"`)
    matchInlineSnapshotBrowser(
      earlyHeightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed') // TODO this is wrong, should show Left from css
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Mixed') // TODO this is wrong, should show Top from css

    matchInlineSnapshotBrowser(earlyPaddingLeftControl.value, `"16"`)
    matchInlineSnapshotBrowser(
      earlyPaddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(earlyPaddingRightControl.value, `"12"`)
    matchInlineSnapshotBrowser(
      earlyPaddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(earlyOpacityControl.value, `"50%"`)
    matchInlineSnapshotBrowser(
      earlyOpacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    await act(async () => {
      await screen.findByTestId('section-header-Advanced')
      fireEvent.click(screen.getByTestId('section-header-Advanced'))
    })

    await act(async () => {
      await screen.findByTestId('target-selector-style')
      fireEvent.click(screen.getByTestId('target-selector'))
    })
    await act(async () => {
      await screen.findByTestId('target-list-item-css')
      fireEvent.mouseDown(screen.getByTestId('target-list-item-css'))
    })

    await act(async () => {
      await screen.findByTestId('target-selector-css')
    })

    const laterMetadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const laterWidthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const laterHeightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const laterPaddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const laterPaddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const laterOpacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(laterMetadata.computedStyle?.['width'], `"203px"`)
    matchInlineSnapshotBrowser(laterWidthControl.value, `"203"`)
    matchInlineSnapshotBrowser(
      laterWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(laterMetadata.computedStyle?.['height'], `"102px"`)
    matchInlineSnapshotBrowser(laterHeightControl.value, `"102"`)
    matchInlineSnapshotBrowser(
      laterHeightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed') // TODO this is wrong, should show Left from css
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Mixed') // TODO this is wrong, should show Top from css

    matchInlineSnapshotBrowser(laterPaddingLeftControl.value, `"16"`)
    matchInlineSnapshotBrowser(
      laterPaddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(laterPaddingRightControl.value, `"12"`)
    matchInlineSnapshotBrowser(
      laterPaddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(laterOpacityControl.value, `"50%"`)
    matchInlineSnapshotBrowser(
      laterOpacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('CSS using default values set inline', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippetStyledComponents(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            css={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: 'auto',
              left: 'auto',
              width: 'auto',
              height: 'auto',
              padding: 0,
              paddingRight: 0,
              borderRadius: 0,
              opacity: 1,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    await act(async () => {
      await screen.findByTestId('section-header-Advanced')
      fireEvent.click(screen.getByTestId('section-header-Advanced'))
    })

    await act(async () => {
      await screen.findByTestId('target-selector-style')
      fireEvent.click(screen.getByTestId('target-selector'))
    })
    await act(async () => {
      await screen.findByTestId('target-list-item-css')
      fireEvent.mouseDown(screen.getByTestId('target-list-item-css'))
    })
    await act(async () => {
      await screen.findByTestId('target-selector-css')
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const paddingControl = (await renderResult.renderedDOM.findByTestId(
      'padding-one',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed') // TODO should show up as Left!
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Mixed') // TODO should show up as Top

    matchInlineSnapshotBrowser(paddingControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      paddingControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(radiusControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(opacityControl.value, `"100%"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('CSS number input arrow increment', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippetStyledComponents(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            css={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: 'auto',
              left: 'auto',
              width: 'auto',
              height: 'auto',
              padding: 0,
              paddingRight: 0,
              borderRadius: 0,
              opacity: 1,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toBe('0')

    await pressKeyTimes(widthControl, renderResult, ['ArrowUp'])
    expect(widthControl.value).toBe('1')

    await pressKeyTimes(widthControl, renderResult, ['ArrowUp', 'ArrowUp', 'ArrowUp'])
    expect(widthControl.value).toBe('4')

    await pressKeyTimes(widthControl, renderResult, [
      'ArrowUp',
      'ArrowDown',
      'ArrowDown',
      'ArrowUp',
      'ArrowDown',
      'ArrowDown',
    ])
    expect(widthControl.value).toBe('2')
  })
  it('CSS number enter and arrow increment', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippetStyledComponents(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            css={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: 'auto',
              left: 'auto',
              width: 'auto',
              height: 'auto',
              padding: 0,
              paddingRight: 0,
              borderRadius: 0,
              opacity: 1,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toBe('0')

    await pressKeyTimes(widthControl, renderResult, ['ArrowUp'])
    expect(widthControl.value).toBe('1')

    await setControlValue('frame-width-number-input', '100', renderResult.renderedDOM)
    await renderResult.getDispatchFollowUpActionsFinished()

    expect(widthControl.value).toBe('100')

    await pressKeyTimes(widthControl, renderResult, ['ArrowUp', 'ArrowUp', 'ArrowUp'])
    expect(widthControl.value).toBe('103')
  })
  it('CSS number with incorrect data', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippetStyledComponents(`
        <div
          style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            css={{
              position: 'absolute',
              backgroundColor: '#DDDDDD',
              top: 'auto',
              left: 'auto',
              width: 'auto',
              height: 'auto',
              padding: 0,
              paddingRight: 0,
              borderRadius: 0,
              opacity: 1,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toBe('0')

    await pressKeyTimes(widthControl, renderResult, ['ArrowUp', 'ArrowUp'])
    expect(widthControl.value).toBe('2')
    await renderResult.getDispatchFollowUpActionsFinished()

    await act(async () => {
      widthControl.focus()
      document.execCommand('insertText', false, 'wrong')

      fireEvent.blur(widthControl)
      await renderResult.getDispatchFollowUpActionsFinished()
    })

    expect(widthControl.value).toBe('2')
  })

  it('Style is using css className', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View, Group } from 'utopia-api'

      export var App = (props) => {
        return (
          <div
            style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <StyleDiv />
            <div
              className='customClassName'
              data-uid={'bbb'}
			  style={{ position: 'absolute' }}
            ></div>
          </div>
        )
      }

      export var StyleDiv = (props) => {
        const styleContent = ".customClassName {width: 250px; height: 250px; padding: 14px; border-radius: 10px; opacity: 0.3;}"
        return (
          <div>
            <style>{styleContent}</style>
          </div>
        )
      }

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              data-uid='${TestSceneUID}'
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement
    const paddingControl = (await renderResult.renderedDOM.findByTestId(
      'padding-one',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['width'], `"250px"`)
    matchInlineSnapshotBrowser(widthControl.value, `"250"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['height'], `"250px"`)
    matchInlineSnapshotBrowser(heightControl.value, `"250"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed') // TODO this should be Left
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Mixed') // TODO this should be Top

    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingLeft'], `"14px"`)
    matchInlineSnapshotBrowser(paddingControl.value, `"14"`)
    matchInlineSnapshotBrowser(
      paddingControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['borderRadius'], `"10px"`)
    matchInlineSnapshotBrowser(radiusControl.value, `"10"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['opacity'], `"0.3"`)
    matchInlineSnapshotBrowser(opacityControl.value, `"30%"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )
  })
  it('Style is using css className, with default values', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View, Group } from 'utopia-api'

      export var App = (props) => {
        return (
          <div
            style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
          >
            <StyleDiv />
            <div
              style={{ position: 'absolute' }}
              className='customClassName'
              data-uid={'bbb'}
            ></div>
          </div>
        )
      }

      export var StyleDiv = (props) => {
        const styleContent = ".customClassName {min-width: 0, max-width: 'none', padding: 0px; border-radius: 0; opacity: 1;}"
        return (
          <div>
            <style>{styleContent}</style>
          </div>
        )
      }

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              data-uid='${TestSceneUID}'
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    // Min-max control is missing

    // await act(async () => {
    //   await screen.findByTestId('toggle-min-max-button')
    //   fireEvent.click(screen.getByTestId('toggle-min-max-button'))
    //   await screen.findByTestId('position-maxWidth-number-input')
    //   await screen.findByTestId('padding-H')
    // })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    // const minWidthControl = (await renderResult.renderedDOM.findByTestId(
    //   'position-minWidth-number-input',
    // )) as HTMLInputElement
    // const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
    //   'position-maxWidth-number-input',
    // )) as HTMLInputElement
    const paddingHorizontalControl = (await renderResult.renderedDOM.findByTestId(
      'padding-H',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    // matchInlineSnapshotBrowser(metadata.computedStyle?.['minWidth'], `"0px"`)
    // matchInlineSnapshotBrowser(minWidthControl.value, `""`)
    // matchInlineSnapshotBrowser(
    //   minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    //   `"trivial-default"`,
    // )

    // matchInlineSnapshotBrowser(metadata.computedStyle?.['maxWidth'], `"none"`)
    // matchInlineSnapshotBrowser(maxWidthControl.value, `""`)
    // matchInlineSnapshotBrowser(
    //   maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    //   `"trivial-default"`,
    // )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingLeft'], `"0px"`)
    matchInlineSnapshotBrowser(paddingHorizontalControl.value, `"–"`)
    matchInlineSnapshotBrowser(
      paddingHorizontalControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    ) // this will be `detected-fromcss` once we use the padding shorthand

    matchInlineSnapshotBrowser(metadata.computedStyle?.['borderRadius'], `"0px"`)
    matchInlineSnapshotBrowser(radiusControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['opacity'], `"1"`)
    matchInlineSnapshotBrowser(opacityControl.value, `"100%"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )
  })
  it('Empty style with lots of trivial defaults', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View, Group } from 'utopia-api'

      export var App = (props) => {
        return (
          <div
            data-uid={'aaa'}
          >
            <div
              style={{ }}
              data-uid={'bbb'}
            ></div>
          </div>
        )
      }

      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              data-uid='${TestSceneUID}'
            >
              <App
                data-uid='${TestAppUID}'
                style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              />
            </Scene>
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    // Min-max control is missing

    // await act(async () => {
    //   await screen.findByTestId('toggle-min-max-button')
    //   fireEvent.click(screen.getByTestId('toggle-min-max-button'))
    //   await screen.findByTestId('position-maxWidth-number-input')
    //   await screen.findByTestId('padding-H')
    // })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    // const minWidthControl = (await renderResult.renderedDOM.findByTestId(
    //   'position-minWidth-number-input',
    // )) as HTMLInputElement
    // const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
    //   'position-maxHeight-number-input',
    // )) as HTMLInputElement
    const paddingHorizontalControl = (await renderResult.renderedDOM.findByTestId(
      'padding-H',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-one',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement

    // matchInlineSnapshotBrowser(metadata.computedStyle?.['minWidth'], `"0px"`)
    // matchInlineSnapshotBrowser(minWidthControl.value, `""`)
    // matchInlineSnapshotBrowser(
    //   minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    //   `"trivial-default"`,
    // )

    // matchInlineSnapshotBrowser(metadata.computedStyle?.['maxWidth'], `"none"`)
    // matchInlineSnapshotBrowser(maxWidthControl.value, `""`)
    // matchInlineSnapshotBrowser(
    //   maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    //   `"trivial-default"`,
    // )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingLeft'], `"0px"`)
    matchInlineSnapshotBrowser(paddingHorizontalControl.value, `"–"`)
    matchInlineSnapshotBrowser(
      paddingHorizontalControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    ) // this will be `detected-fromcss` once we use the padding shorthand

    matchInlineSnapshotBrowser(metadata.computedStyle?.['borderRadius'], `"0px"`)
    matchInlineSnapshotBrowser(radiusControl.value, `"–"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['opacity'], `"1"`)
    matchInlineSnapshotBrowser(opacityControl.value, `"100%"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected"`,
    )
  })
  it('Style properties inherited from parent', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            ...props.style,
            position: 'absolute',
            backgroundColor: '#FFFFFF',
            color: '#ff00ff',
            fontSize: '24px',
          }}
          data-uid={'aaa'}
        >
          <div data-uid={'bbb'} style={{ position: 'absolute' }}>hello</div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const fontSizeControl = (await renderResult.renderedDOM.findByTestId(
      'fontSize',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['fontSize'], `"24px"`)
    matchInlineSnapshotBrowser(fontSizeControl.value, `"24"`)
    matchInlineSnapshotBrowser(
      fontSizeControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected"`,
    )

    expectSelectControlValue(renderResult, 'frame-child-constraint-width-popuplist', 'Mixed') // TODO this should be Left
    expectSelectControlValue(renderResult, 'frame-child-constraint-height-popuplist', 'Mixed') // TODO this should be Top
  })

  // TODO reinstate once flex shorthand is supported again
  it('Flex shorthand properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            display: 'flex',
          }}
          data-uid={'aaa'}
        >
          <div data-uid={'bbb'} style={{ flex: '1 0 15px' }}>hello</div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    // const flexBasis = (await renderResult.renderedDOM.findByTestId(
    //   'position-flexBasis-number-input',
    // )) as HTMLInputElement
    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    // const flexShrink = (await renderResult.renderedDOM.findByTestId(
    //   'position-flexShrink-number-input',
    // )) as HTMLInputElement

    // matchInlineSnapshotBrowser(metadata.computedStyle?.['flexBasis'], `"15px"`)
    // matchInlineSnapshotBrowser(flexBasis.value, `"15"`)
    // matchInlineSnapshotBrowser(
    //   flexBasis.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    //   `"simple"`,
    // )
    matchInlineSnapshotBrowser(metadata.computedStyle?.['flexGrow'], `"1"`)
    matchInlineSnapshotBrowser(widthControl.value, `"400"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
    // matchInlineSnapshotBrowser(metadata.computedStyle?.['flexShrink'], `"0"`)
    // matchInlineSnapshotBrowser(flexShrink.value, `"0"`)
    // matchInlineSnapshotBrowser(
    //   flexShrink.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    //   `"simple"`,
    // )
  })

  it('Flex longhand properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            display: 'flex',
          }}
          data-uid={'aaa'}
        >
          <div
            data-uid={'bbb'}
            style={{
              flexGrow: 1,
              flexShrink: 0,
              flexBasis: '15px',
            }}
          >hello</div>
        </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    // const flexBasis = (await renderResult.renderedDOM.findByTestId(
    //   'position-flexBasis-number-input',
    // )) as HTMLInputElement
    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    // const flexShrink = (await renderResult.renderedDOM.findByTestId(
    //   'position-flexShrink-number-input',
    // )) as HTMLInputElement

    // matchInlineSnapshotBrowser(metadata.computedStyle?.['flexBasis'], `"15px"`)
    // matchInlineSnapshotBrowser(flexBasis.value, `"15"`)
    // matchInlineSnapshotBrowser(
    //   flexBasis.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    //   `"simple"`,
    // )
    matchInlineSnapshotBrowser(metadata.computedStyle?.['flexGrow'], `"1"`)
    matchInlineSnapshotBrowser(widthControl.value, `"400"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
    // matchInlineSnapshotBrowser(metadata.computedStyle?.['flexShrink'], `"0"`)
    // matchInlineSnapshotBrowser(flexShrink.value, `"0"`)
    // matchInlineSnapshotBrowser(
    //   flexShrink.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    //   `"simple"`,
    // )
  })
  it('Flex longhand in style props using a simple expression', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{ ...props.style, position: 'absolute', display: 'flex', backgroundColor: '#FFFFFF' }}
          data-uid={'aaa'}
        >
          <div
            style={{
              backgroundColor: '#DDDDDD',
              flexGrow: 0.5+0.5,
              height: 30+100,
            }}
            data-uid={'bbb'}
          ></div>
        </div>
      `),
      'await-first-dom-report',
    )

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch(
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'frame-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'frame-height-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"400"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"130"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('Shows multifile selected element properties', async () => {
    let projectContents: ProjectContents = {
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.CodeAhead,
        ),
        null,
        null,
        0,
      ),
      '/src': directory(),
      '/utopia': directory(),
      [StoryboardFilePath]: createCodeFile(
        StoryboardFilePath,
        `
  import * as React from 'react'
  import Utopia, {
    Scene,
    Storyboard,
  } from 'utopia-api'
  import { App } from '/src/app.js'

  export var storyboard = (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        data-uid='${TestSceneUID}'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <App data-uid='${TestAppUID}' />
      </Scene>
    </Storyboard>
  )`,
      ),
      '/src/app.js': createCodeFile(
        '/src/app.js',
        `
  import * as React from 'react'
  export var App = (props) => {
    return <div data-uid='app-outer-div' style={{position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF'}}>
      <div data-uid='app-inner-div' style={{padding: 8, paddingLeft: 10, width: '100%', minHeight: 25}}/>
    </div>
  }`,
      ),
    }

    const renderResult = await renderTestEditorWithProjectContent(
      contentsToTree(projectContents),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['app-outer-div', 'app-inner-div'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const paddingTopControl = (await renderResult.renderedDOM.findByTestId(
      'padding-T',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement

    // Padding top is coming from the shorthand `padding` value.
    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingTop'], `"8px"`)
    matchInlineSnapshotBrowser(paddingTopControl.value, `"8"`)
    matchInlineSnapshotBrowser(
      paddingTopControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    // Padding left is coming from the `paddingLeft` value.
    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingLeft'], `"10px"`)
    matchInlineSnapshotBrowser(paddingLeftControl.value, `"10"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })

  describe('border radius controls', () => {
    it('applied border radius to the selected element', async () => {
      const editor = await renderTestEditorWithCode(projectWithTwoDivs, 'await-first-dom-report')
      const one = editor.renderedDOM.getByTestId('one')
      const two = editor.renderedDOM.getByTestId('two')

      await selectComponentsForTest(editor, [EP.fromString('sb/one')])
      await selectComponentsForTest(editor, [EP.fromString('sb/two')])

      await setControlValue('radius-one', '20', editor.renderedDOM)

      expect(one.style.borderRadius).toEqual('')

      expect(two.style.borderRadius).toEqual('20px')
    })
  })

  describe('border radius controls from the inspector', () => {
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
        name: 'single value shows the highlight control',
        startSnippet: makeCodeSnippetWithKeyValue({
          border: '1px solid black',
          borderRadius: 5,
          width: 20,
          height: 20,
        }),
        controlTestID: 'radius-one',
        hoveredCanvasControls: [SubduedBorderRadiusControlTestId('hovered')],
        focusedCanvasControls: [SubduedBorderRadiusControlTestId('focused')],
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

  describe('conditionals', () => {
    it('overrides conditional branch', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? (
          <div data-uid='bbb' data-testid='bbb'>foo</div>
        ) : (
          <div data-uid='ccc' data-testid='ccc'>bar</div>
        )}
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])
      const bbbPath = EP.appendToPath(targetPath, 'bbb')
      const cccPath = EP.appendToPath(targetPath, 'ccc')
      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      // check the status of the override buttons
      {
        const trueButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'true'),
        )
        const falseButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'false'),
        )

        // the `true` button should be checked because the condition is true and there is no override
        expect(trueButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual('true')
        expect(falseButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual(
          'false',
        )

        // the button status is "simple" because there is no override
        expect(trueButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'simple',
        )
        expect(falseButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'simple',
        )

        // 'bbb' should be present in the metadata and 'ccc' should not.
        expect(renderResult.getEditorState().editor.jsxMetadata).toHaveProperty(
          EP.toString(bbbPath),
        )
        expect(renderResult.getEditorState().editor.jsxMetadata).not.toHaveProperty(
          EP.toString(cccPath),
        )
      }

      // override to false
      {
        await clickButtonAndSelectTarget(
          renderResult,
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'false'),
          [targetPath],
        )

        expect(renderResult.renderedDOM.getByTestId('ccc')).not.toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('bbb')).toBeNull()

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                // @utopia/uid=conditional
                // @utopia/conditional=false
                [].length === 0 ? (
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                ) : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                )
              }
            </div>
          `),
        )

        const trueButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'true'),
        )
        const falseButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'false'),
        )

        // the `false` button should be checked because that is the override
        expect(trueButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual('false')
        expect(falseButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual('true')

        // the button status is "overridden"
        expect(trueButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'overridden',
        )
        expect(falseButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'overridden',
        )

        // 'ccc' should be present in the metadata and 'bbb' should not.
        expect(renderResult.getEditorState().editor.jsxMetadata).not.toHaveProperty(
          EP.toString(bbbPath),
        )
        expect(renderResult.getEditorState().editor.jsxMetadata).toHaveProperty(
          EP.toString(cccPath),
        )
      }

      // override to true
      {
        await clickButtonAndSelectTarget(renderResult, ConditionalOverrideControlTrueTestId, [
          targetPath,
        ])

        expect(renderResult.renderedDOM.queryByTestId('ccc')).toBeNull()
        expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                // @utopia/uid=conditional
                // @utopia/conditional=true
                [].length === 0 ? (
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                ) : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                )
              }
            </div>
          `),
        )

        const trueButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'true'),
        )
        const falseButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'false'),
        )

        // the `true` button should be checked because that is the override
        expect(trueButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual('true')
        expect(falseButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual(
          'false',
        )

        // the button status is "overridden"
        expect(trueButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'overridden',
        )
        expect(falseButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'overridden',
        )

        // 'bbb' should be present in the metadata and 'ccc' should not.
        expect(renderResult.getEditorState().editor.jsxMetadata).toHaveProperty(
          EP.toString(bbbPath),
        )
        expect(renderResult.getEditorState().editor.jsxMetadata).not.toHaveProperty(
          EP.toString(cccPath),
        )
      }

      // disable override
      {
        await clickButtonAndSelectTarget(renderResult, ConditionalOverrideControlToggleTestId, [
          targetPath,
        ])

        expect(renderResult.renderedDOM.queryByTestId('ccc')).toBeNull()
        expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
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
          `),
        )

        const trueButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'true'),
        )
        const falseButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'false'),
        )

        // the `true` button should be checked because the condition is true and there is no override
        expect(trueButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual('true')
        expect(falseButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual(
          'false',
        )

        // the button status is "simple" because there is no override
        expect(trueButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'simple',
        )
        expect(falseButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'simple',
        )

        // 'bbb' should be present in the metadata and 'ccc' should not.
        expect(renderResult.getEditorState().editor.jsxMetadata).toHaveProperty(
          EP.toString(bbbPath),
        )
        expect(renderResult.getEditorState().editor.jsxMetadata).not.toHaveProperty(
          EP.toString(cccPath),
        )
      }

      // override to the current condition value
      {
        await clickButtonAndSelectTarget(renderResult, ConditionalOverrideControlToggleTestId, [
          targetPath,
        ])

        expect(renderResult.renderedDOM.queryByTestId('ccc')).toBeNull()
        expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
                  <div data-uid='aaa'>
                    {
                      // @utopia/uid=conditional
                      // @utopia/conditional=true
                      [].length === 0 ? (
                        <div data-uid='bbb' data-testid='bbb'>foo</div>
                      ) : (
                        <div data-uid='ccc' data-testid='ccc'>bar</div>
                      )
                    }
                  </div>
                `),
        )

        const trueButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'true'),
        )
        const falseButton = await renderResult.renderedDOM.findByTestId(
          getOptionControlTestId(ConditionalOverrideControlTestIdPrefix, 'false'),
        )

        // the `true` button should be checked because that is the override
        expect(trueButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual('true')
        expect(falseButton.attributes.getNamedItemNS(null, 'data-ischecked')?.value).toEqual(
          'false',
        )

        // the button status is "overridden"
        expect(trueButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'overridden',
        )
        expect(falseButton.attributes.getNamedItemNS(null, 'data-controlstatus')?.value).toEqual(
          'overridden',
        )

        // 'bbb' should be present in the metadata and 'ccc' should not.
        expect(renderResult.getEditorState().editor.jsxMetadata).toHaveProperty(
          EP.toString(bbbPath),
        )
        expect(renderResult.getEditorState().editor.jsxMetadata).not.toHaveProperty(
          EP.toString(cccPath),
        )
      }
    })
    it('switches conditional branches', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? (
          <div data-uid='bbb' data-testid='bbb'>foo</div>
        ) : (
          <div data-uid='ccc' data-testid='ccc'>bar</div>
        )}
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])
      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      // switch branches
      {
        await clickButtonAndSelectTarget(renderResult, ConditionalsControlSwitchBranchesTestId, [
          targetPath,
        ])

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
            {
              // @utopia/uid=conditional
              [].length === 0 ? (
              <div data-uid='ccc' data-testid='ccc'>bar</div>
            ) : (
              <div data-uid='bbb' data-testid='bbb'>foo</div>
            )}
            </div>
          `),
        )
      }
    })
    it('rearranges comments so that the conditional flag is at the top', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          // hello
          [].length === 0 /*inside*/ ? (
          <div data-uid='bbb' data-testid='bbb'>foo</div>
        ) : (
          <div data-uid='ccc' data-testid='ccc'>bar</div>
        )
          /* this is a test */
          // @utopia/conditional=true
          // and another comment
        }
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])
      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      await clickButtonAndSelectTarget(renderResult, ConditionalOverrideControlFalseTestId, [
        targetPath,
      ])

      expect(renderResult.renderedDOM.getByTestId('ccc')).not.toBeNull()
      expect(renderResult.renderedDOM.queryByTestId('bbb')).toBeNull()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                // @utopia/uid=conditional
                // hello
                // @utopia/conditional=false
                [].length === 0 /*inside*/ ? (
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                ) : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                ) /* this is a test */
                // and another comment
              }
            </div>
         `),
      )
    })
    // Conditional section should not be closeable, so no UI currently to override multiple conditionals at the same time
    xit('toggles multiple conditional branches', async () => {
      const startSnippet = `
        <div data-uid='aaa'>
          {
            // @utopia/uid=conditional1
            true ? (
            <div data-uid='bbb' data-testid='bbb'>foo</div>
          ) : (
            <div data-uid='ccc' data-testid='ccc'>bar</div>
          )}
          {
            // @utopia/uid=conditional2
            true ? (
            <div data-uid='ddd' data-testid='ddd'>baz</div>
          ) : (
            <div data-uid='eee' data-testid='eee'>qux</div>
          )}
        </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

      const firstConditional = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional1'])
      const secondConditional = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional2'])

      await act(async () => {
        await renderResult.dispatch([selectComponents([firstConditional], false)], false)
      })

      // open the section in the inspector
      {
        await clickButtonAndSelectTarget(renderResult, ConditionalsControlSectionOpenTestId, [
          firstConditional,
        ])
        expect(renderResult.renderedDOM.queryByTestId('bbb')).not.toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('ccc')).toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('ddd')).not.toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('eee')).toBeNull()

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                // @utopia/uid=conditional1
                // @utopia/conditional=true
                true ? (
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                ) : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                )
              }
              {
                // @utopia/uid=conditional2
                true ? (
                <div data-uid='ddd' data-testid='ddd'>baz</div>
              ) : (
                <div data-uid='eee' data-testid='eee'>qux</div>
              )}
            </div>
          `),
        )
      }

      const bothConditionals = [firstConditional, secondConditional]

      await act(async () => {
        await renderResult.dispatch([selectComponents(bothConditionals, false)], false)
      })

      // toggle both to false
      {
        await clickButtonAndSelectTarget(
          renderResult,
          ConditionalOverrideControlFalseTestId,
          bothConditionals,
        )

        expect(renderResult.renderedDOM.queryByTestId('bbb')).toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('ccc')).not.toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('ddd')).toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('eee')).not.toBeNull()

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
            {
              // @utopia/uid=conditional1
              // @utopia/conditional=false
              true ? (
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                ) : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                )
              }
              {
                // @utopia/uid=conditional2
                // @utopia/conditional=false
                true ? (
                  <div data-uid='ddd' data-testid='ddd'>baz</div>
                  ) : (
                    <div data-uid='eee' data-testid='eee'>qux</div>
                    )}
                    </div>
                    `),
        )
      }

      // toggle to true
      {
        await clickButtonAndSelectTarget(
          renderResult,
          ConditionalOverrideControlTrueTestId,
          bothConditionals,
        )

        expect(renderResult.renderedDOM.queryByTestId('bbb')).not.toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('ccc')).toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('ddd')).not.toBeNull()
        expect(renderResult.renderedDOM.queryByTestId('eee')).toBeNull()

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(`
            <div data-uid='aaa'>
              {
                // @utopia/uid=conditional1
                // @utopia/conditional=true
                true ? (
                  <div data-uid='bbb' data-testid='bbb'>foo</div>
                ) : (
                  <div data-uid='ccc' data-testid='ccc'>bar</div>
                )
              }
              {
                // @utopia/uid=conditional2
                // @utopia/conditional=true
                true ? (
                <div data-uid='ddd' data-testid='ddd'>baz</div>
              ) : (
                <div data-uid='eee' data-testid='eee'>qux</div>
              )}
            </div>
          `),
        )
      }
    })
    it('displays the condition', async () => {
      const startSnippet = `
      <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? (
          <div data-uid='bbb' data-testid='bbb'>foo</div>
        ) : (
          <div data-uid='ccc' data-testid='ccc'>bar</div>
        )}
      </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])
      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      const expressionElement = renderResult.renderedDOM.getByTestId(
        ConditionalsControlSectionExpressionTestId,
      )
      expect((expressionElement as HTMLInputElement).value).toEqual('[].length === 0')
    })
    it('allows changing the expression', async () => {
      const startSnippet = `
      <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? (
            <div data-uid='bbb' data-testid='bbb'>foo</div>
          ) : (
            <div data-uid='ccc' data-testid='ccc'>bar</div>
          )}
      </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      await setControlValue(
        ConditionalsControlSectionExpressionTestId,
        '40 + 2 < 42',
        renderResult.renderedDOM,
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(renderResult.renderedDOM.queryByTestId('bbb')).toBeNull()
      expect(renderResult.renderedDOM.queryByTestId('ccc')).not.toBeNull()

      const expressionElement = renderResult.renderedDOM.getByTestId(
        ConditionalsControlSectionExpressionTestId,
      )
      expect((expressionElement as HTMLInputElement).value).toEqual('40 + 2 < 42')
    })
    it('changing the expression disables override', async () => {
      const startSnippet = `
      <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          // @utopia/conditional=true
          [].length > 0 ? (
            <div data-uid='bbb' data-testid='bbb'>foo</div>
          ) : (
            <div data-uid='ccc' data-testid='ccc'>bar</div>
          )}
      </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])

      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      await setControlValue(
        ConditionalsControlSectionExpressionTestId,
        '40 + 2 < 42',
        renderResult.renderedDOM,
      )
      await renderResult.getDispatchFollowUpActionsFinished()

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa'>
          {
            // @utopia/uid=conditional
              40 + 2 < 42 ? (
              <div data-uid='bbb' data-testid='bbb'>foo</div>
            ) : (
              <div data-uid='ccc' data-testid='ccc'>bar</div>
            )}
        </div>
        `),
      )
    })
    it('shows the branches in the inspector', async () => {
      const startSnippet = `
      <div data-uid='aaa'>
        {
          // @utopia/uid=conditional
          [].length === 0 ? (
          <div data-uid='bbb' data-testid='bbb' style={{ position: 'absolute', width: 22, height: 22 }}><div>Another div</div></div>
        ) : (
          <h1 data-uid='ccc' data-testid='ccc'>hello there</h1>
        )}
      </div>
      `
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(startSnippet),
        'await-first-dom-report',
      )

      expect(renderResult.renderedDOM.getByTestId('bbb')).not.toBeNull()

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'conditional'])
      await act(async () => {
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
      })

      const branchElementTrue = renderResult.renderedDOM.getByTestId(
        ConditionalsControlBranchTrueTestId,
      )
      const branchElementFalse = renderResult.renderedDOM.getByTestId(
        ConditionalsControlBranchFalseTestId,
      )

      expect(branchElementTrue.innerText).toEqual('div')
      expect(branchElementFalse.innerText).toEqual('h1')
    })
  })

  describe('groups', () => {
    it('ignores removing pins from a group child', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippetStyledComponents(`
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 10,
                  bottom: 20,
                  left: 30,
                  right: 40,
                }}
              />
            </Group>
          </div>
      `),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo'])

      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
        await dispatchDone
      })

      const leftControl = (await renderResult.renderedDOM.findByTestId(
        'frame-left-number-input',
      )) as HTMLInputElement

      expect(leftControl.value).toBe('30')

      const elementFrame = getFrame(targetPath, renderResult)

      await setControlValue('frame-left-number-input', '', renderResult.renderedDOM)

      // Nothing changed
      expect(getFrame(targetPath, renderResult)).toBe(elementFrame)
    })
    it('ignores setting percentage pins on a group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippetStyledComponents(`
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
              }}
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 10,
                  bottom: 20,
                  left: 30,
                  right: 40,
                }}
              />
            </Group>
          </div>
      `),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'group'])

      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
        await dispatchDone
      })

      const leftControl = (await renderResult.renderedDOM.findByTestId(
        'frame-left-number-input',
      )) as HTMLInputElement

      expect(leftControl.value).toBe('10')

      const elementFrame = getFrame(targetPath, renderResult)

      await setControlValue('frame-left-number-input', '25%', renderResult.renderedDOM)

      // Nothing changed
      expect(getFrame(targetPath, renderResult)).toBe(elementFrame)
    })
    it('ignores settings percentage pins on a group child if the parent has no explicit width and height', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippetStyledComponents(`
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 100,
              }}
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 10,
                  bottom: 20,
                  left: 30,
                  right: 40,
                }}
              />
            </Group>
          </div>
      `),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo'])

      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
        await dispatchDone
      })

      const leftControl = (await renderResult.renderedDOM.findByTestId(
        'frame-left-number-input',
      )) as HTMLInputElement

      expect(leftControl.value).toBe('30')

      const elementFrame = getFrame(targetPath, renderResult)

      await setControlValue('frame-left-number-input', '25%', renderResult.renderedDOM)

      // Nothing changed
      expect(getFrame(targetPath, renderResult)).toBe(elementFrame)
    })
    it('ignores settings percentage pins on a group child if the parent has explicit width and height', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippetStyledComponents(`
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 100,
                height: 100,
              }}
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 10,
                  bottom: 20,
                  left: 30,
                  right: 40,
                }}
              />
            </Group>
          </div>
      `),
        'await-first-dom-report',
      )

      const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo'])

      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetPath], false)], false)
        await dispatchDone
      })

      const leftControl = (await renderResult.renderedDOM.findByTestId(
        'frame-left-number-input',
      )) as HTMLInputElement

      expect(leftControl.value).toBe('30')

      const elementFrame = getFrame(targetPath, renderResult)

      await setControlValue('frame-left-number-input', '25%', renderResult.renderedDOM)

      // Nothing changed
      expect(getFrame(targetPath, renderResult)).toBe(elementFrame)
    })

    describe('group children', () => {
      // TODO rewrite this to use the dropdown

      function runTest(test: {
        input: string
        selection: ElementPath[]
        logic: (renderResult: EditorRenderResult) => Promise<void>
        want: string
      }): () => Promise<void> {
        return async function testBody() {
          const renderResult = await renderTestEditorWithCode(
            formatTestProjectCode(makeTestProjectCodeWithSnippet(test.input)),
            'await-first-dom-report',
          )
          await act(async () => {
            const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
            await renderResult.dispatch([selectComponents(test.selection, false)], false)
            await dispatchDone
          })
          await test.logic(renderResult)
          expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
            formatTestProjectCode(makeTestProjectCodeWithSnippet(test.want)),
          )
        }
      }

      it(
        'set constraint for Top, Height using the visual pin control',
        runTest({
          input: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                  backgroundColor: 'blue'
                }}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
          selection: [EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo'])],
          logic: async (renderResult) => {
            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-height-popuplist',
              'Scale',
              'Top and Height',
            )
          },
          want: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  left: 0,
                  width: 100,
                  backgroundColor: 'blue',
                  top: 0,
                  height: 100,
                }}
                data-constraints={['top', 'height']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
        }),
      )

      it(
        'unset constraint for TLBR',
        runTest({
          input: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                  backgroundColor: 'blue'
                }}
                data-constraints={['top']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
          selection: [EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo'])],
          logic: async (renderResult) => {
            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-height-popuplist',
              'Top',
              'Scale',
            )
          },
          want: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  left: 0,
                  width: 100,
                  backgroundColor: 'blue',
                  top: 0,
                  height: 100,
                }}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
        }),
      )

      it(
        'set constraint for TLBR, multiple values',
        runTest({
          input: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                  backgroundColor: 'blue'
                }}
                data-constraints={['left', 'width']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
          selection: [EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo'])],
          logic: async (renderResult) => {
            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-height-popuplist',
              'Scale',
              'Top and Height',
            )

            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-width-popuplist',
              'Left and Width',
              'Left and Width',
            )
          },
          want: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  left: 0,
                  width: 100,
                  backgroundColor: 'blue',
                  top: 0,
                  height: 100,
                }}
                data-constraints={['left', 'width', 'top', 'height']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
        }),
      )

      it(
        'unset constraint for TLBR, multiple values',
        runTest({
          input: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                  backgroundColor: 'blue'
                }}
                data-constraints={['left', 'top', 'width']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
          selection: [EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo'])],
          logic: async (renderResult) => {
            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-height-popuplist',
              'Top',
              'Scale',
            )
          },
          want: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  left: 0,
                  width: 100,
                  backgroundColor: 'blue',
                  top: 0,
                  height: 100,
                }}
                data-constraints={['left', 'width']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
        }),
      )

      it(
        'set constraint for TLBR, multiselect',
        runTest({
          input: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                  backgroundColor: 'blue'
                }}
                data-constraints={['left', 'width']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
          selection: [
            EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo']),
            EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'bar']),
          ],
          logic: async (renderResult) => {
            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-height-popuplist',
              'Scale',
              'Top and Height',
            )
          },
          want: `
         <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  left: 0,
                  width: 100,
                  backgroundColor: 'blue',
                  top: 0,
                  height: 100,
                }}
                data-constraints={['left', 'width', 'top', 'height']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  left: 200,
                  backgroundColor: 'red',
                  top: 200,
                  height: 0,
                }}
                data-constraints={['top', 'height']}
              />
            </Group>
          </div>
        `,
        }),
      )

      it(
        'set constraint for TLBR, multiselect with already-present values',
        runTest({
          input: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                  backgroundColor: 'blue'
                }}
                data-constraints={['left', 'width', 'top', 'height']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
          selection: [
            EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo']),
            EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'bar']),
          ],
          logic: async (renderResult) => {
            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-height-popuplist',
              'Mixed',
              'Top and Height',
            )
          },
          want: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  left: 0,
                  width: 100,
                  backgroundColor: 'blue',
                  top: 0,
                  height: 100,
                }}
                data-constraints={['left', 'width', 'top', 'height']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  left: 200,
                  backgroundColor: 'red',
                  top: 200,
                  height: 0,
                }}
                data-constraints={['top', 'height']}
              />
            </Group>
          </div>
        `,
        }),
      )

      it(
        'unset constraint for TH, multiselect',
        runTest({
          input: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                  backgroundColor: 'blue'
                }}
                data-constraints={['left', 'top']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
                data-constraints={['top']}
              />
            </Group>
          </div>
        `,
          selection: [
            EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo']),
            EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'bar']),
          ],
          logic: async (renderResult) => {
            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-height-popuplist',
              'Top',
              'Scale',
            )
          },
          want: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  left: 0,
                  width: 100,
                  backgroundColor: 'blue',
                  top: 0,
                  height: 100,
                }}
                data-constraints={['left']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  left: 200,
                  backgroundColor: 'red',
                  top: 200,
                  height: 0,
                }}
              />
            </Group>
          </div>
        `,
        }),
      )

      it(
        'set constraint for Left + Width',
        runTest({
          input: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                  backgroundColor: 'blue'
                }}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
          selection: [EP.appendNewElementPath(TestScenePath, ['aaa', 'group', 'foo'])],
          logic: async (renderResult) => {
            await pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-width-popuplist',
              'Scale',
              'Left and Width',
            )
          },
          want: `
          <div
            style={{ position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid='aaa'
          >
            <Group
              data-uid='group'
            >
              <div
                data-uid='foo'
                style={{
                  position: 'absolute',
                  top: 0,
                  height: 100,
                  backgroundColor: 'blue',
                  left: 0,
                  width: 100,
                }}
                data-constraints={['left', 'width']}
              />
              <div
                data-uid='bar'
                style={{
                  position: 'absolute',
                  top: 200,
                  left: 200,
                  backgroundColor: 'red'
                }}
              />
            </Group>
          </div>
        `,
        }),
      )

      it(
        'set hugging text to scale',
        runTest({
          input: `<div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='root'
      >
        <span
          style={{
            position: 'absolute',
            wordBreak: 'break-word',
            left: 261,
            top: 139,
            height: 19,
            width: 'max-content',
          }}
          data-uid='text'
        >
          span
        </span>
      </div>`,
          selection: [EP.appendNewElementPath(TestScenePath, ['root', 'text'])],
          logic: async (renderResult) =>
            pickFromReactSelectPopupList(
              renderResult,
              'frame-child-constraint-width-popuplist',
              'Left',
              'Scale',
            ),
          want: `<div
          style={{
            height: '100%',
            width: '100%',
            contain: 'layout',
          }}
          data-uid='root'
        >
          <span
            style={{
              position: 'absolute',
              wordBreak: 'break-word',
              top: 139,
              height: 19,
              left: '65.3%',
              width: '7.5%',
            }}
            data-uid='text'
          >
            span
          </span>
        </div>`,
        }),
      )
    })
  })
})

describe('Inspector fields and code remain in sync', () => {
  const propsToTest = [
    {
      stylePropKey: 'top',
      controlTestId: 'frame-top-number-input',
      startValue: '200em',
      endValue: 4800,
      startValueDisplayed: '3200',
      endValueDisplayed: '4800',
    },
    {
      stylePropKey: 'width',
      controlTestId: 'frame-width-number-input',
      startValue: '200em',
      endValue: 4800,
      startValueDisplayed: '3200',
      endValueDisplayed: '4800',
    },
    {
      stylePropKey: 'left',
      controlTestId: 'frame-left-number-input',
      startValue: '200cm',
      endValue: 11338,
      startValueDisplayed: '7559',
      endValueDisplayed: '11338',
    },
    {
      stylePropKey: 'width',
      controlTestId: 'frame-width-number-input',
      startValue: '200pt',
      endValue: 400,
      startValueDisplayed: '266.5',
      endValueDisplayed: '400',
    },
    {
      stylePropKey: 'height',
      controlTestId: 'frame-height-number-input',
      startValue: 200,
      endValue: 300,
      startValueDisplayed: '200',
      endValueDisplayed: '300',
    },
    {
      stylePropKey: 'height',
      controlTestId: 'frame-height-number-input',
      startValue: '200pt',
      endValue: 400,
      startValueDisplayed: '266.5',
      endValueDisplayed: '400',
    },
  ]

  function makeCodeSnippetWithKeyValue(stylePropKey: string, value: unknown): string {
    const printedValue = typeof value === 'number' ? value : JSON.stringify(value)

    return `
      <div
        style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
        data-uid='aaa'
      >
        <div
          style={{
            // Comment to force this onto multiple lines when printing
            position: 'absolute',
            ${stylePropKey}: ${printedValue},
          }}
          data-uid='bbb'
        />
      </div>
    `
  }
  const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

  propsToTest.forEach(
    ({
      stylePropKey,
      controlTestId,
      startValue,
      startValueDisplayed,
      endValue,
      endValueDisplayed,
    }) => {
      it(`Updating the code updates the Inspector for prop ${stylePropKey}`, async () => {
        const startCodeSnippet = makeCodeSnippetWithKeyValue(stylePropKey, startValue)
        const endCodeSnippet = makeCodeSnippetWithKeyValue(stylePropKey, endValue)

        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(startCodeSnippet),
          'await-first-dom-report',
        )

        await selectElement(targetPath, renderResult)

        // Capture the starting value
        const startControlValue = await getControlValue(controlTestId, renderResult.renderedDOM)

        // Simulate a code change
        const updateActions = actionsForUpdatedCode(endCodeSnippet)
        await dispatchActionsAndWaitUntilComplete(updateActions, renderResult)

        // Capture the new value
        const endControlValue = await getControlValue(controlTestId, renderResult.renderedDOM)

        expect(startControlValue).toEqual(`${startValueDisplayed}`)
        expect(endControlValue).toEqual(`${endValueDisplayed}`)
      })

      it(`Updating the Inspector updates the code for prop ${stylePropKey}`, async () => {
        const startCodeSnippet = makeCodeSnippetWithKeyValue(stylePropKey, startValue)
        const endCodeSnippet = makeCodeSnippetWithKeyValue(stylePropKey, endValue)

        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(startCodeSnippet),
          'await-first-dom-report',
        )

        await selectElement(targetPath, renderResult)

        // Update the value via the Inspector control
        await setControlValue(controlTestId, `${endValueDisplayed}`, renderResult.renderedDOM)

        // Ensure the printed code is as correct
        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          makeTestProjectCodeWithSnippet(endCodeSnippet),
        )
      })
    },
  )
})

describe('Undo behavior in inspector', () => {
  it('Pressing Undo while an inspector field is active triggers Utopia global undo', async () => {
    const startingCodeSnippet = `
    <div
      style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
      data-uid={'aaa'}
    >
      <div
        style={{
          position: 'absolute',
          backgroundColor: '#DDDDDD',
          width: 203,
          height: 102,
          padding: 16,
          paddingRight: 12,
          opacity: 0.5,
        }}
        data-uid={'bbb'}
      ></div>
    </div>
  `
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(startingCodeSnippet),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
    })

    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity',
    )) as HTMLInputElement
    matchInlineSnapshotBrowser(opacityControl.value, `"50%"`)

    // change the opacity of the selected element
    await setControlValue('opacity', '60', renderResult.renderedDOM)

    matchInlineSnapshotBrowser(opacityControl.value, `"60%"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    // focus on opacityControl
    await act(async () => {
      fireEvent.focus(opacityControl)
    })

    // type a new value '0.8' to opacityControl
    await act(async () => {
      fireEvent.change(opacityControl, { target: { value: '80' } })
    })

    // press Cmd + Z on the opacityControl
    await act(async () => {
      fireEvent.keyDown(opacityControl, { key: 'z', keyCode: 90, metaKey: true })
      fireEvent.keyUp(opacityControl, { key: 'z', keyCode: 90, metaKey: true })
    })

    // the control's value should now be undone
    matchInlineSnapshotBrowser(
      ((await renderResult.renderedDOM.findByTestId('opacity')) as HTMLInputElement).value,
      `"50%"`,
    )

    // await for the code to be updated
    await renderResult.getDispatchFollowUpActionsFinished()

    // the code should be the same as the original
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(startingCodeSnippet),
    )
  })
})

const projectWithTwoDivs = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid={"one"}
      data-testid={"one"}
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 149,
        top: 195,
        width: 412,
        height: 447,
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: '100%',
          height: '100%',
          contain: 'layout',
        }}
      />
    </div>
    <div
      data-uid={"two"}
      data-testid={"two"}
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 717,
        top: 195,
        width: 412,
        height: 447,
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: '100%',
          height: '100%',
          contain: 'layout',
        }}
      />
    </div>
  </Storyboard>
)
`

function getFrame(targetPath: ElementPath, renderResult: EditorRenderResult) {
  return MetadataUtils.getFrameInCanvasCoords(
    targetPath,
    renderResult.getEditorState().editor.jsxMetadata,
  )
}
