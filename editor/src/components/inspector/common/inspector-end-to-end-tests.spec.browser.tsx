import * as React from 'react'
import { fireEvent, screen } from '@testing-library/react'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import * as TP from '../../../core/shared/template-path'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'
import { PrettierConfig } from '../../../core/workers/parser-printer/prettier-utils'
import * as Prettier from 'prettier'
import { act } from 'react-test-renderer'

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

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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
    ).toMatchInlineSnapshot(`"detected"`)

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

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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
    ).toMatchInlineSnapshot(`"detected"`)

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

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )
    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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
      'position-Width-number-input',
    )) as HTMLInputElement
    const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Height-number-input',
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
    ).toMatchInlineSnapshot(`"detected"`)

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
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(metadata.computedStyle?.['maxWidth']).toMatchInlineSnapshot(`"none"`)
    expect(maxWidthControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)
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
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
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
    ).toMatchInlineSnapshot(`"detected"`)

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
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
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

    expect(widthControl.value).toMatchInlineSnapshot(`"320"`)
    expect(
      widthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"260"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(topControl.value).toMatchInlineSnapshot(`"100"`)
    expect(
      topControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(leftControl.value).toMatchInlineSnapshot(`"40"`)
    expect(
      leftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"16"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"32"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

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
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
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
    ).toMatchInlineSnapshot(`"detected"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"42"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple-unknown-css"`)

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
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
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
    ).toMatchInlineSnapshot(`"detected"`)

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
        `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
    
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
              component={App}
              static
              props={{ 
                style: { position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 },
                padding: 5,
                paddingRight: 10,
                opacity: 0.5,
                left: 30,
                border: '50%',
              }}
              data-uid='scene-aaa'
            />
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
    )

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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
    ).toMatchInlineSnapshot(`"detected"`)

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
      makeTestProjectCodeWithSnippet(`
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

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    await act(async () => {
      await screen.findByTestId('target-selector-style')
      fireEvent.click(screen.getByTestId('target-selector'))
      await screen.findByTestId('target-list-item-css')
      fireEvent.mouseDown(screen.getByTestId('target-list-item-css'))
      await screen.findByTestId('target-selector-css')
    })

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['height']).toMatchInlineSnapshot(`"102px"`)
    expect(heightControl.value).toMatchInlineSnapshot(`"102"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"16"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"12"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(opacityControl.value).toMatchInlineSnapshot(`"0.5"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"simple"`)
  })
  it('CSS using default values set inline', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
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
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
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
    ).toMatchInlineSnapshot(`"detected"`)

    expect(heightControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(paddingRightControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      paddingRightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

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
        `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
    
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
              component={App}
              static
              props={{ style: { position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid='scene-aaa'
            />
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
    )

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

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
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['height']).toMatchInlineSnapshot(`"250px"`)
    expect(heightControl.value).toMatchInlineSnapshot(`"250"`)
    expect(
      heightControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['paddingLeft']).toMatchInlineSnapshot(`"14px"`)
    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"14"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['borderRadius']).toMatchInlineSnapshot(`"10px"`)
    expect(radiusControl.value).toMatchInlineSnapshot(`"10"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['opacity']).toMatchInlineSnapshot(`"0.3"`)
    expect(opacityControl.value).toMatchInlineSnapshot(`"0.3"`)
    expect(
      opacityControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)
  })
  it('Style is using css className, with default values', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
    
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
              component={App}
              static
              props={{ style: { position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid='scene-aaa'
            />
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
    )

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

    const minWidthControl = (await renderResult.renderedDOM.findByTestId(
      'position-Width-number-input',
    )) as HTMLInputElement
    const maxWidthControl = (await renderResult.renderedDOM.findByTestId(
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

    expect(metadata.computedStyle?.['minWidth']).toMatchInlineSnapshot(`"0px"`)
    expect(minWidthControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      minWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['maxWidth']).toMatchInlineSnapshot(`"none"`)
    expect(maxWidthControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      maxWidthControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['paddingLeft']).toMatchInlineSnapshot(`"0px"`)
    expect(paddingLeftControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      paddingLeftControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

    expect(metadata.computedStyle?.['borderRadius']).toMatchInlineSnapshot(`"0px"`)
    expect(radiusControl.value).toMatchInlineSnapshot(`"0"`)
    expect(
      radiusControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)

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

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const metadata = renderResult.getEditorState().editor.jsxMetadataKILLME.elements[
      'utopia-storyboard-uid/scene-aaa:aaa/bbb'
    ]

    const fontSizeControl = (await renderResult.renderedDOM.findByTestId(
      'fontSize',
    )) as HTMLInputElement

    expect(metadata.computedStyle?.['fontSize']).toMatchInlineSnapshot(`"24px"`)
    expect(fontSizeControl.value).toMatchInlineSnapshot(`"24"`)
    expect(
      fontSizeControl.attributes.getNamedItemNS(null, 'data-controlstatus')?.value,
    ).toMatchInlineSnapshot(`"detected"`)
  })
})
