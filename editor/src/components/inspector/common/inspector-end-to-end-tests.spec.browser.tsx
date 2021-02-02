import * as React from 'react'
import { render } from '@testing-library/react'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import * as TP from '../../../core/shared/template-path'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'

describe('inspector tests with real metadata', () => {
  it('placeholder', () => {
    // the tests will come here
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
})
