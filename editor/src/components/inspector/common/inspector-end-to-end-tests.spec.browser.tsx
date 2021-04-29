import * as React from 'react'
import { fireEvent, screen } from '@testing-library/react'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import * as TP from '../../../core/shared/template-path'
import {
  makeTestProjectCodeWithSnippet,
  makeTestProjectCodeWithSnippetStyledComponents,
  renderTestEditorWithCode,
  renderTestEditorWithProjectContent,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'
import { PrettierConfig } from 'utopia-vscode-common'
import * as Prettier from 'prettier'
import { act } from 'react-test-renderer'
import { contentsToTree } from '../../assets'
import {
  ProjectContents,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../../../core/shared/project-file-types'
import { directory } from '../../../core/model/project-file-utils'
import { DefaultPackageJson, StoryboardFilePath } from '../../editor/store/editor-state'
import { createCodeFile } from '../../custom-code/code-file.test-utils'

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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const flexPaddingTopControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-T',
    )) as HTMLInputElement
    const flexPaddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement

    // Padding top is coming from the shorthand `padding` value.
    expect(metadata.computedStyle?.['paddingTop']).toMatchInlineSnapshot(`"20px"`)
    expect(flexPaddingTopControl.value).toMatchInlineSnapshot(`"20"`)
    expect(
      flexPaddingTopControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    // Padding left is coming from the `paddingLeft` value.
    expect(metadata.computedStyle?.['paddingLeft']).toMatchInlineSnapshot(`"15px"`)
    expect(flexPaddingLeftControl.value).toMatchInlineSnapshot(`"15"`)
    expect(
      flexPaddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const bottomControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedBottom-number-input',
    )) as HTMLInputElement
    const rightControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedRight-number-input',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['width']).toMatchInlineSnapshot(`"266px"`)
    expect(widthControl.value).toMatchInlineSnapshot(`"266"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['height']).toMatchInlineSnapshot(`"124px"`)
    expect(heightControl.value).toMatchInlineSnapshot(`"124"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['top']).toMatchInlineSnapshot(`"98px"`)
    expect(topControl.value).toMatchInlineSnapshot(`"98"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['left']).toMatchInlineSnapshot(`"55px"`)
    expect(leftControl.value).toMatchInlineSnapshot(`"55"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(bottomControl.value).toMatchInlineSnapshot(`"178"`)
    expect(
      bottomControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(rightControl.value).toMatchInlineSnapshot(`"79"`)
    expect(
      rightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const bottomControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedBottom-number-input',
    )) as HTMLInputElement
    const rightControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedRight-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toMatchInlineSnapshot(`"335"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"102"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['top']).toMatchInlineSnapshot(`"98px"`)
    expect(topControl.value).toMatchInlineSnapshot(`"98"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['left']).toMatchInlineSnapshot(`"55px"`)
    expect(leftControl.value).toMatchInlineSnapshot(`"55"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['bottom']).toMatchInlineSnapshot(`"200px"`)
    expect(bottomControl.value).toMatchInlineSnapshot(`"200"`)
    expect(
      bottomControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['right']).toMatchInlineSnapshot(`"10px"`)
    expect(rightControl.value).toMatchInlineSnapshot(`"10"`)
    expect(
      rightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const bottomControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedBottom-number-input',
    )) as HTMLInputElement
    const rightControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedRight-number-input',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['width']).toMatchInlineSnapshot(`"203px"`)
    expect(widthControl.value).toMatchInlineSnapshot(`"203"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['height']).toMatchInlineSnapshot(`"102px"`)
    expect(heightControl.value).toMatchInlineSnapshot(`"102"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(topControl.value).toMatchInlineSnapshot(`"98"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"187"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['bottom']).toMatchInlineSnapshot(`"200px"`)
    expect(bottomControl.value).toMatchInlineSnapshot(`"200"`)
    expect(
      bottomControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['right']).toMatchInlineSnapshot(`"10px"`)
    expect(rightControl.value).toMatchInlineSnapshot(`"10"`)
    expect(
      rightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    await renderResult.dispatch(
      [selectComponents([TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toMatchInlineSnapshot(`"10vw"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"124pt"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(topControl.value).toMatchInlineSnapshot(`"1.4cm"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"2em"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['width']).toMatchInlineSnapshot(`"203px"`)
    expect(widthControl.value).toMatchInlineSnapshot(`"203"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['height']).toMatchInlineSnapshot(`"102px"`)
    expect(heightControl.value).toMatchInlineSnapshot(`"102"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"16"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"12"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(opacityControl.value).toMatchInlineSnapshot(`"0.5"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)
    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
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

    expect(widthControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(topControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(radiusControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(opacityControl.value).toMatchInlineSnapshot(`"1"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['minWidth']).toMatchInlineSnapshot(`"0px"`)
    expect(minWidthControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(metadata.computedStyle?.['maxWidth']).toMatchInlineSnapshot(`"none"`)
    expect(maxWidthControl.value).toMatchInlineSnapshot(`""`)
    expect(
      maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    await renderResult.dispatch(
      [selectComponents([TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toMatchInlineSnapshot(`"203"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"102"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(topControl.value).toMatchInlineSnapshot(`"25"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"14"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"4"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"8"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(radiusControl.value).toMatchInlineSnapshot(`"2"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    await renderResult.dispatch(
      [selectComponents([TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toMatchInlineSnapshot(`"80%"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"65%"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(topControl.value).toMatchInlineSnapshot(`"25%"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"10%"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"4%"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"8%"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(radiusControl.value).toMatchInlineSnapshot(`"50%"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    await renderResult.dispatch(
      [selectComponents([TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toMatchInlineSnapshot(`"150"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"70"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(topControl.value).toMatchInlineSnapshot(`"220"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"100"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"44"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"42"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(radiusControl.value).toMatchInlineSnapshot(`"15%"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)
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
    )

    await renderResult.dispatch(
      [selectComponents([TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement

    expect(widthControl.value).toMatchInlineSnapshot(`"150"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"130"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(topControl.value).toMatchInlineSnapshot(`"33"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"74"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"4"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"5"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(radiusControl.value).toMatchInlineSnapshot(`"7"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const topControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedTop-number-input',
    )) as HTMLInputElement
    const leftControl = (await renderResult.renderedDOM.findByTestId(
      'position-PinnedLeft-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    expect(widthControl.value).toMatchInlineSnapshot(`"100"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"50"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(topControl.value).toMatchInlineSnapshot(`"100"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"30"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"5"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"10"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(metadata.computedStyle?.['borderRadius']).toMatchInlineSnapshot(`"50%"`)
    expect(radiusControl.value).toMatchInlineSnapshot(`"50%"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)

    expect(metadata.computedStyle?.['opacity']).toMatchInlineSnapshot(`"0.5"`)
    expect(opacityControl.value).toMatchInlineSnapshot(`"0.5"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"controlled"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    await act(async () => {
      await screen.findByTestId('target-selector-style')
      fireEvent.click(screen.getByTestId('target-selector'))
      await screen.findByTestId('target-list-item-css')
      fireEvent.mouseDown(screen.getByTestId('target-list-item-css'))
      await screen.findByTestId('target-selector-css')
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['width']).toMatchInlineSnapshot(`"203px"`)
    expect(widthControl.value).toMatchInlineSnapshot(`"203"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(metadata.computedStyle?.['height']).toMatchInlineSnapshot(`"102px"`)
    expect(heightControl.value).toMatchInlineSnapshot(`"102"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"16"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"12"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(opacityControl.value).toMatchInlineSnapshot(`"0.5"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    await renderResult.dispatch(
      [selectComponents([TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    await act(async () => {
      await screen.findByTestId('target-selector-style')
      fireEvent.click(screen.getByTestId('target-selector'))
      await screen.findByTestId('target-list-item-css')
      fireEvent.mouseDown(screen.getByTestId('target-list-item-css'))
      await screen.findByTestId('target-selector-css')
    })

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const paddingRightControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-R',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    expect(widthControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(radiusControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    expect(opacityControl.value).toMatchInlineSnapshot(`"1"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const widthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const heightControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['width']).toMatchInlineSnapshot(`"250px"`)
    expect(widthControl.value).toMatchInlineSnapshot(`"250"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(metadata.computedStyle?.['height']).toMatchInlineSnapshot(`"250px"`)
    expect(heightControl.value).toMatchInlineSnapshot(`"250"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(metadata.computedStyle?.['paddingLeft']).toMatchInlineSnapshot(`"14px"`)
    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"14"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(metadata.computedStyle?.['borderRadius']).toMatchInlineSnapshot(`"10px"`)
    expect(radiusControl.value).toMatchInlineSnapshot(`"10"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(metadata.computedStyle?.['opacity']).toMatchInlineSnapshot(`"0.3"`)
    expect(opacityControl.value).toMatchInlineSnapshot(`"0.3"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    await act(async () => {
      await screen.findByTestId('toggle-min-max-button')
      fireEvent.click(screen.getByTestId('toggle-min-max-button'))
      await screen.findByTestId('position-maxWidth-number-input')
      await screen.findByTestId('layout-system-expand')
      fireEvent.click(screen.getByTestId('layout-system-expand'))
      await screen.findByTestId('flexPadding-L')
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const minWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-minWidth-number-input',
    )) as HTMLInputElement
    const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-maxWidth-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['minWidth']).toMatchInlineSnapshot(`"0px"`)
    expect(minWidthControl.value).toMatchInlineSnapshot(`""`)
    expect(
      minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"trivial-default"`)

    expect(metadata.computedStyle?.['maxWidth']).toMatchInlineSnapshot(`"none"`)
    expect(maxWidthControl.value).toMatchInlineSnapshot(`""`)
    expect(
      maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"trivial-default"`)

    expect(metadata.computedStyle?.['paddingLeft']).toMatchInlineSnapshot(`"0px"`)
    expect(paddingLeftControl.value).toMatchInlineSnapshot(`""`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"trivial-default"`) // this will be `detected-fromcss` once we use the padding shorthand

    expect(metadata.computedStyle?.['borderRadius']).toMatchInlineSnapshot(`"0px"`)
    expect(radiusControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)

    expect(metadata.computedStyle?.['opacity']).toMatchInlineSnapshot(`"1"`)
    expect(opacityControl.value).toMatchInlineSnapshot(`"1"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected-fromcss"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    await act(async () => {
      await screen.findByTestId('toggle-min-max-button')
      fireEvent.click(screen.getByTestId('toggle-min-max-button'))
      await screen.findByTestId('position-maxWidth-number-input')
      await screen.findByTestId('layout-system-expand')
      fireEvent.click(screen.getByTestId('layout-system-expand'))
      await screen.findByTestId('flexPadding-L')
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const minWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-minWidth-number-input',
    )) as HTMLInputElement
    const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-maxHeight-number-input',
    )) as HTMLInputElement
    const paddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement
    const radiusControl = (await renderResult.renderedDOM.findByTestId(
      'radius-all-number-input',
    )) as HTMLInputElement
    const opacityControl = (await renderResult.renderedDOM.findByTestId(
      'opacity-number-control',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['minWidth']).toMatchInlineSnapshot(`"0px"`)
    expect(minWidthControl.value).toMatchInlineSnapshot(`""`)
    expect(
      minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"trivial-default"`)

    expect(metadata.computedStyle?.['maxWidth']).toMatchInlineSnapshot(`"none"`)
    expect(maxWidthControl.value).toMatchInlineSnapshot(`""`)
    expect(
      maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"trivial-default"`)

    expect(metadata.computedStyle?.['paddingLeft']).toMatchInlineSnapshot(`"0px"`)
    expect(paddingLeftControl.value).toMatchInlineSnapshot(`""`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"trivial-default"`) // this will be `detected-fromcss` once we use the padding shorthand

    expect(metadata.computedStyle?.['borderRadius']).toMatchInlineSnapshot(`"0px"`)
    expect(radiusControl.value).toMatchInlineSnapshot(`""`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"trivial-default"`)

    expect(metadata.computedStyle?.['opacity']).toMatchInlineSnapshot(`"1"`)
    expect(opacityControl.value).toMatchInlineSnapshot(`"1"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const fontSizeControl = (await renderResult.renderedDOM.findByTestId(
      'fontSize',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['fontSize']).toMatchInlineSnapshot(`"24px"`)
    expect(fontSizeControl.value).toMatchInlineSnapshot(`"24"`)
    expect(
      fontSizeControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const flexBasis = (await renderResult.renderedDOM.findByTestId(
      'position-flexBasis-number-input',
    )) as HTMLInputElement
    const flexGrow = (await renderResult.renderedDOM.findByTestId(
      'position-flexGrow-number-input',
    )) as HTMLInputElement
    const flexShrink = (await renderResult.renderedDOM.findByTestId(
      'position-flexShrink-number-input',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['flexBasis']).toMatchInlineSnapshot(`"15px"`)
    expect(flexBasis.value).toMatchInlineSnapshot(`"15"`)
    expect(
      flexBasis.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
    expect(metadata.computedStyle?.['flexGrow']).toMatchInlineSnapshot(`"1"`)
    expect(flexGrow.value).toMatchInlineSnapshot(`"1"`)
    expect(
      flexGrow.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
    expect(metadata.computedStyle?.['flexShrink']).toMatchInlineSnapshot(`"0"`)
    expect(flexShrink.value).toMatchInlineSnapshot(`"0"`)
    expect(
      flexShrink.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
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
    )

    const targetPath = TP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const flexBasis = (await renderResult.renderedDOM.findByTestId(
      'position-flexBasis-number-input',
    )) as HTMLInputElement
    const flexGrow = (await renderResult.renderedDOM.findByTestId(
      'position-flexGrow-number-input',
    )) as HTMLInputElement
    const flexShrink = (await renderResult.renderedDOM.findByTestId(
      'position-flexShrink-number-input',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['flexBasis']).toMatchInlineSnapshot(`"15px"`)
    expect(flexBasis.value).toMatchInlineSnapshot(`"15"`)
    expect(
      flexBasis.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
    expect(metadata.computedStyle?.['flexGrow']).toMatchInlineSnapshot(`"1"`)
    expect(flexGrow.value).toMatchInlineSnapshot(`"1"`)
    expect(
      flexGrow.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
    expect(metadata.computedStyle?.['flexShrink']).toMatchInlineSnapshot(`"0"`)
    expect(flexShrink.value).toMatchInlineSnapshot(`"0"`)
    expect(
      flexShrink.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
  })
  it('Shows multifile selected element properties', async () => {
    let projectContents: ProjectContents = {
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.BothMatch,
        ),
        null,
        0,
      ),
      '/src': directory(),
      '/utopia': directory(),
      [StoryboardFilePath]: createCodeFile(
        StoryboardFilePath,
        `
  import * as React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
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
  import { jsx } from 'utopia-api'
  export var App = (props) => {
    return <div data-uid='app-outer-div' style={{position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF'}}>
      <div data-uid='app-inner-div' style={{padding: 8, paddingLeft: 10, width: '100%', minHeight: 25}}/>
    </div>
  }`,
      ),
    }

    const renderResult = await renderTestEditorWithProjectContent(contentsToTree(projectContents))

    const targetPath = TP.appendNewElementPath(TestScenePath, ['app-outer-div', 'app-inner-div'])

    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const metadata = renderResult.getEditorState().editor.jsxMetadata[TP.toString(targetPath)]

    const flexPaddingTopControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-T',
    )) as HTMLInputElement
    const flexPaddingLeftControl = (await renderResult.renderedDOM.findByTestId(
      'flexPadding-L',
    )) as HTMLInputElement

    // Padding top is coming from the shorthand `padding` value.
    expect(metadata.computedStyle?.['paddingTop']).toMatchInlineSnapshot(`"8px"`)
    expect(flexPaddingTopControl.value).toMatchInlineSnapshot(`"8"`)
    expect(
      flexPaddingTopControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)

    // Padding left is coming from the `paddingLeft` value.
    expect(metadata.computedStyle?.['paddingLeft']).toMatchInlineSnapshot(`"10px"`)
    expect(flexPaddingLeftControl.value).toMatchInlineSnapshot(`"10"`)
    expect(
      flexPaddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
  })
})
