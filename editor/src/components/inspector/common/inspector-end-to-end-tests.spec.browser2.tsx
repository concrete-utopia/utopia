/* eslint-disable jest/expect-expect */
import React from 'react'
import { fireEvent, RenderResult, screen } from '@testing-library/react'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  makeTestProjectCodeWithSnippetStyledComponents,
  renderTestEditorWithCode,
  renderTestEditorWithProjectContent,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../canvas/ui-jsx.test-utils'
import {
  selectComponents,
  sendLinterRequestMessage,
  updateFromCodeEditor,
} from '../../editor/actions/action-creators'
import { PrettierConfig } from 'utopia-vscode-common'
import * as Prettier from 'prettier/standalone'
import { act } from '@testing-library/react'
import { contentsToTree } from '../../assets'
import {
  ElementPath,
  ProjectContents,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../../../core/shared/project-file-types'
import { directory } from '../../../core/model/project-file-utils'
import { DefaultPackageJson, StoryboardFilePath } from '../../editor/store/editor-state'
import { createCodeFile } from '../../custom-code/code-file.test-utils'
import { matchInlineSnapshotBrowser } from '../../../../test/karma-snapshots'
import { EditorAction } from '../../editor/action-types'

async function getControl(
  controlTestId: string,
  renderedDOM: RenderResult,
): Promise<HTMLInputElement> {
  return (await renderedDOM.findByTestId(controlTestId)) as HTMLInputElement
}

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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const bottomControl = (await renderResult.renderedDOM.findByTestId(
      'position-bottom-number-input',
    )) as HTMLInputElement
    const rightControl = (await renderResult.renderedDOM.findByTestId(
      'position-right-number-input',
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

    matchInlineSnapshotBrowser(bottomControl.value, `"178"`)
    matchInlineSnapshotBrowser(
      bottomControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected"`,
    )

    matchInlineSnapshotBrowser(rightControl.value, `"79"`)
    matchInlineSnapshotBrowser(
      rightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected"`,
    )
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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const bottomControl = (await renderResult.renderedDOM.findByTestId(
      'position-bottom-number-input',
    )) as HTMLInputElement
    const rightControl = (await renderResult.renderedDOM.findByTestId(
      'position-right-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"335"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"102"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected"`,
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

    matchInlineSnapshotBrowser(metadata.computedStyle?.['bottom'], `"200px"`)
    matchInlineSnapshotBrowser(bottomControl.value, `"200"`)
    matchInlineSnapshotBrowser(
      bottomControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['right'], `"10px"`)
    matchInlineSnapshotBrowser(rightControl.value, `"10"`)
    matchInlineSnapshotBrowser(
      rightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const bottomControl = (await renderResult.renderedDOM.findByTestId(
      'position-bottom-number-input',
    )) as HTMLInputElement
    const rightControl = (await renderResult.renderedDOM.findByTestId(
      'position-right-number-input',
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

    matchInlineSnapshotBrowser(topControl.value, `"98"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"187"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['bottom'], `"200px"`)
    matchInlineSnapshotBrowser(bottomControl.value, `"200"`)
    matchInlineSnapshotBrowser(
      bottomControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['right'], `"10px"`)
    matchInlineSnapshotBrowser(rightControl.value, `"10"`)
    matchInlineSnapshotBrowser(
      rightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"10vw"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"124pt"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(topControl.value, `"1.4cm"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"2em"`)
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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
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

    matchInlineSnapshotBrowser(opacityControl.value, `"0.5"`)
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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement
    const minWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-minWidth-number-input',
    )) as HTMLInputElement
    const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-maxWidth-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(topControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(paddingLeftControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingRightControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(radiusControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(opacityControl.value, `"1"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['minWidth'], `"0px"`)
    matchInlineSnapshotBrowser(minWidthControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['maxWidth'], `"none"`)
    matchInlineSnapshotBrowser(maxWidthControl.value, `""`)
    matchInlineSnapshotBrowser(
      maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"80%"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"65%"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(topControl.value, `"25%"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"10%"`)
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
          data-uid={'aaa'}
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
        [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
        false,
      )
      await dispatchDone
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"150"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"70"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(topControl.value, `"220"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"100"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"150"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"130"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )

    matchInlineSnapshotBrowser(topControl.value, `"33"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"74"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
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
      import { Scene, Storyboard, View } from 'utopia-api'

      export var App = (props) => {
        return (
          <div
            style={{ ...props.style, position: 'absolute', backgroundColor: '#FFFFFF' }}
            data-uid={'aaa'}
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

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetPath], false)], false)
      await dispatchDone
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-top-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-left-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
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

    matchInlineSnapshotBrowser(topControl.value, `"100"`)
    matchInlineSnapshotBrowser(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(leftControl.value, `"30"`)
    matchInlineSnapshotBrowser(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"controlled"`,
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
    matchInlineSnapshotBrowser(opacityControl.value, `"0.5"`)
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
      'position-width-number-input',
    )) as HTMLInputElement
    const earlyHeightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const earlyPaddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const earlyPaddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const earlyOpacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(earlyMetadata.computedStyle?.['width'], `"203px"`)
    matchInlineSnapshotBrowser(earlyWidthControl.value, `"203"`)
    matchInlineSnapshotBrowser(
      earlyWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(earlyMetadata.computedStyle?.['height'], `"102px"`)
    matchInlineSnapshotBrowser(earlyHeightControl.value, `"102"`)
    matchInlineSnapshotBrowser(
      earlyHeightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

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

    matchInlineSnapshotBrowser(earlyOpacityControl.value, `"0.5"`)
    matchInlineSnapshotBrowser(
      earlyOpacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    await act(async () => {
      await screen.findByTestId('target-selector-style')
      fireEvent.click(screen.getByTestId('target-selector'))
      await screen.findByTestId('target-list-item-css')
      fireEvent.mouseDown(screen.getByTestId('target-list-item-css'))
      await screen.findByTestId('target-selector-css')
    })

    const laterMetadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const laterWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-width-number-input',
    )) as HTMLInputElement
    const laterHeightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const laterPaddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const laterPaddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const laterOpacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
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

    matchInlineSnapshotBrowser(laterOpacityControl.value, `"0.5"`)
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
      await screen.findByTestId('target-selector-style')
      fireEvent.click(screen.getByTestId('target-selector'))
      await screen.findByTestId('target-list-item-css')
      fireEvent.mouseDown(screen.getByTestId('target-list-item-css'))
      await screen.findByTestId('target-selector-css')
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'padding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(widthControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(heightControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple-unknown-css"`,
    )

    matchInlineSnapshotBrowser(paddingLeftControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(paddingRightControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(radiusControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )

    matchInlineSnapshotBrowser(opacityControl.value, `"1"`)
    matchInlineSnapshotBrowser(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
  })
  it('Style is using css className', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'

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
      'position-width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-height-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['width'], `"250px"`)
    matchInlineSnapshotBrowser(widthControl.value, `"250"`)
    matchInlineSnapshotBrowser(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['height'], `"250px"`)
    matchInlineSnapshotBrowser(heightControl.value, `"250"`)
    matchInlineSnapshotBrowser(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingLeft'], `"14px"`)
    matchInlineSnapshotBrowser(paddingLeftControl.value, `"14"`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['borderRadius'], `"10px"`)
    matchInlineSnapshotBrowser(radiusControl.value, `"10"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['opacity'], `"0.3"`)
    matchInlineSnapshotBrowser(opacityControl.value, `"0.3"`)
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
      import { Scene, Storyboard, View } from 'utopia-api'

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

    await act(async () => {
      await screen.findByTestId('toggle-min-max-button')
      fireEvent.click(screen.getByTestId('toggle-min-max-button'))
      await screen.findByTestId('position-maxWidth-number-input')
      await screen.findByTestId('padding-L')
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const minWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-minWidth-number-input',
    )) as HTMLInputElement
    const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-maxWidth-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['minWidth'], `"0px"`)
    matchInlineSnapshotBrowser(minWidthControl.value, `""`)
    matchInlineSnapshotBrowser(
      minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"trivial-default"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['maxWidth'], `"none"`)
    matchInlineSnapshotBrowser(maxWidthControl.value, `""`)
    matchInlineSnapshotBrowser(
      maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"trivial-default"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingLeft'], `"0px"`)
    matchInlineSnapshotBrowser(paddingLeftControl.value, `""`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"trivial-default"`,
    ) // this will be `detected-fromcss` once we use the padding shorthand

    matchInlineSnapshotBrowser(metadata.computedStyle?.['borderRadius'], `"0px"`)
    matchInlineSnapshotBrowser(radiusControl.value, `"0"`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"detected-fromcss"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['opacity'], `"1"`)
    matchInlineSnapshotBrowser(opacityControl.value, `"1"`)
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
      import { Scene, Storyboard, View } from 'utopia-api'

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

    await act(async () => {
      await screen.findByTestId('toggle-min-max-button')
      fireEvent.click(screen.getByTestId('toggle-min-max-button'))
      await screen.findByTestId('position-maxWidth-number-input')
      await screen.findByTestId('padding-L')
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[EP.toString(targetPath)]

    const minWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-minWidth-number-input',
    )) as HTMLInputElement
    const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-maxHeight-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'padding-L',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['minWidth'], `"0px"`)
    matchInlineSnapshotBrowser(minWidthControl.value, `""`)
    matchInlineSnapshotBrowser(
      minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"trivial-default"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['maxWidth'], `"none"`)
    matchInlineSnapshotBrowser(maxWidthControl.value, `""`)
    matchInlineSnapshotBrowser(
      maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"trivial-default"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['paddingLeft'], `"0px"`)
    matchInlineSnapshotBrowser(paddingLeftControl.value, `""`)
    matchInlineSnapshotBrowser(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"trivial-default"`,
    ) // this will be `detected-fromcss` once we use the padding shorthand

    matchInlineSnapshotBrowser(metadata.computedStyle?.['borderRadius'], `"0px"`)
    matchInlineSnapshotBrowser(radiusControl.value, `""`)
    matchInlineSnapshotBrowser(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"trivial-default"`,
    )

    matchInlineSnapshotBrowser(metadata.computedStyle?.['opacity'], `"1"`)
    matchInlineSnapshotBrowser(opacityControl.value, `"1"`)
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
          <div data-uid={'bbb'}>hello</div>
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
  })
  it('Flex shorthand properties', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div
          style={{
            display: 'flex',
          }}
          data-uid={'aaa'}
        >
          <div data-uid={'bbb'} style={{flex: '1 0 15px'}}>hello</div>
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

    await act(async () => {
      await screen.findByTestId('layout-system-expand')
      fireEvent.click(screen.getByTestId('layout-system-expand'))
    })

    const flexBasis = (await renderResult.renderedDOM.findByTestId(
      'position-flexBasis-number-input',
    )) as HTMLInputElement
    const flexGrow = (await renderResult.renderedDOM.findByTestId(
      'position-flexGrow-number-input',
    )) as HTMLInputElement
    const flexShrink = (await renderResult.renderedDOM.findByTestId(
      'position-flexShrink-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['flexBasis'], `"15px"`)
    matchInlineSnapshotBrowser(flexBasis.value, `"15"`)
    matchInlineSnapshotBrowser(
      flexBasis.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
    matchInlineSnapshotBrowser(metadata.computedStyle?.['flexGrow'], `"1"`)
    matchInlineSnapshotBrowser(flexGrow.value, `"1"`)
    matchInlineSnapshotBrowser(
      flexGrow.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
    matchInlineSnapshotBrowser(metadata.computedStyle?.['flexShrink'], `"0"`)
    matchInlineSnapshotBrowser(flexShrink.value, `"0"`)
    matchInlineSnapshotBrowser(
      flexShrink.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
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

    const flexBasis = (await renderResult.renderedDOM.findByTestId(
      'position-flexBasis-number-input',
    )) as HTMLInputElement
    const flexGrow = (await renderResult.renderedDOM.findByTestId(
      'position-flexGrow-number-input',
    )) as HTMLInputElement
    const flexShrink = (await renderResult.renderedDOM.findByTestId(
      'position-flexShrink-number-input',
    )) as HTMLInputElement

    matchInlineSnapshotBrowser(metadata.computedStyle?.['flexBasis'], `"15px"`)
    matchInlineSnapshotBrowser(flexBasis.value, `"15"`)
    matchInlineSnapshotBrowser(
      flexBasis.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
    matchInlineSnapshotBrowser(metadata.computedStyle?.['flexGrow'], `"1"`)
    matchInlineSnapshotBrowser(flexGrow.value, `"1"`)
    matchInlineSnapshotBrowser(
      flexGrow.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
      `"simple"`,
    )
    matchInlineSnapshotBrowser(metadata.computedStyle?.['flexShrink'], `"0"`)
    matchInlineSnapshotBrowser(flexShrink.value, `"0"`)
    matchInlineSnapshotBrowser(
      flexShrink.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
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
    registerModule,
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
})

describe('Inspector fields and code remain in sync', () => {
  const propsToTest = [
    {
      stylePropKey: 'top',
      controlTestId: 'position-top-number-input',
      startValue: '200em',
      endValue: '300em',
    },
    {
      stylePropKey: 'left',
      controlTestId: 'position-left-number-input',
      startValue: '200cm',
      endValue: '300cm',
    },
    {
      stylePropKey: 'bottom',
      controlTestId: 'position-bottom-number-input',
      startValue: '200vw',
      endValue: '300vw',
    },
    {
      stylePropKey: 'right',
      controlTestId: 'position-right-number-input',
      startValue: '200%',
      endValue: '300%',
    },
    {
      stylePropKey: 'width',
      controlTestId: 'position-width-number-input',
      startValue: '200pt',
      endValue: '300pt',
    },
    {
      stylePropKey: 'height',
      controlTestId: 'position-height-number-input',
      startValue: 200,
      endValue: 300,
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

  propsToTest.forEach(({ stylePropKey, controlTestId, startValue, endValue }) => {
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

      expect(startControlValue).toEqual(`${startValue}`)
      expect(endControlValue).toEqual(`${endValue}`)
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
      await setControlValue(controlTestId, `${endValue}`, renderResult.renderedDOM)

      // Ensure the printed code is as correct
      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(endCodeSnippet),
      )
    })
  })
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
      'opacity-number-control',
    )) as HTMLInputElement
    matchInlineSnapshotBrowser(opacityControl.value, `"0.5"`)

    // change the opacity of the selected element
    await setControlValue('opacity-number-control', '0.6', renderResult.renderedDOM)

    matchInlineSnapshotBrowser(opacityControl.value, `"0.6"`)
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
      fireEvent.change(opacityControl, { target: { value: '0.8' } })
    })

    // press Cmd + Z on the opacityControl
    await act(async () => {
      fireEvent.keyDown(opacityControl, { key: 'z', keyCode: 90, metaKey: true })
      fireEvent.keyUp(opacityControl, { key: 'z', keyCode: 90, metaKey: true })
    })

    // the control's value should now be undone
    matchInlineSnapshotBrowser(
      ((await renderResult.renderedDOM.findByTestId('opacity-number-control')) as HTMLInputElement)
        .value,
      `"0.5"`,
    )

    // await for the code to be updated
    await renderResult.getDispatchFollowUpActionsFinished()

    // the code should be the same as the original
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(startingCodeSnippet),
    )
  })
})
