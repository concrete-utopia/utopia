/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "testGroupChild"] }] */

import * as EP from '../../core/shared/element-path'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { selectComponents } from '../editor/actions/action-creators'
import { wait } from '../../utils/utils.test-utils'

describe('Constraints Section', () => {
  describe('Frame Children', () => {
    it('Span without size shows up as Left constrained', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='app-root' style={{ width: 300, height: 300 }}>
          <div data-uid='div' style={{ position: 'absolute', width: 150, height: 150 }}>
            <span data-uid='span' style={{ position: 'absolute', left: 15, top: 15 }}>An implicitly constrained span</span>
          </div>
        </div>
      `),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [
          selectComponents(
            [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:app-root/div/span')],
            false,
          ),
        ],
        true,
      )

      const widthConstraintDropdown = renderResult.renderedDOM.getByTestId(
        'frame-child-constraint-width-label',
      )
      expect(widthConstraintDropdown.textContent).toEqual('Left')

      const heightConstraintDropdown = renderResult.renderedDOM.getByTestId(
        'frame-child-constraint-height-label',
      )
      expect(heightConstraintDropdown.textContent).toEqual('Top')
    })
  })
  describe('Group Children', () => {
    const testGroupChild = (params: {
      snippet: string
      expectedWidthConstraintDropdownOption: string
      expectedHeightConstraintDropdownOption: string
    }) =>
      async function testBody() {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div data-uid='app-root' style={{ width: 300, height: 300 }}>
          <Group data-uid='group'>
            ${params.snippet}
          </Group>
        </div>
      `),
          'await-first-dom-report',
        )

        await renderResult.dispatch(
          [
            selectComponents(
              [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:app-root/group/span')],
              false,
            ),
          ],
          true,
        )

        const widthConstraintDropdown = renderResult.renderedDOM.getByTestId(
          'frame-child-constraint-width-label',
        )
        expect(widthConstraintDropdown.textContent).toEqual(
          params.expectedWidthConstraintDropdownOption,
        )

        const heightConstraintDropdown = renderResult.renderedDOM.getByTestId(
          'frame-child-constraint-height-label',
        )
        expect(heightConstraintDropdown.textContent).toEqual(
          params.expectedHeightConstraintDropdownOption,
        )
      }

    it(
      'Span without size shows up as Width / Height constrained',
      testGroupChild({
        snippet: `<span data-uid='span'>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Width',
        expectedHeightConstraintDropdownOption: 'Height',
      }),
    )

    it(
      'Span with only width shows up as Scale width, Height height',
      testGroupChild({
        snippet: `<span data-uid='span' style={{ width: 150 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Height',
      }),
    )

    it(
      'Span with size shows up as Scale',
      testGroupChild({
        snippet: `<span data-uid='span' style={{ width: 150, height: 30 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )
  })
})
