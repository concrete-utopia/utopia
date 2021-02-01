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
})
