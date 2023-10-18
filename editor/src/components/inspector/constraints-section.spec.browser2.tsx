/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "testGroupChild", "testFrameChild"] }] */

import * as EP from '../../core/shared/element-path'
import type { ElementPath } from '../../core/shared/project-file-types'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { selectComponents } from '../editor/actions/action-creators'

const testChild = (params: {
  snippet: string
  elementPath: ElementPath
  expectedWidthConstraintDropdownOption: string
  expectedHeightConstraintDropdownOption: string
}) =>
  async function testBody() {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(params.snippet),
      'await-first-dom-report',
    )

    await renderResult.dispatch([selectComponents([params.elementPath], false)], true)

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

const testFrameChild = (params: {
  snippet: string
  expectedWidthConstraintDropdownOption: string
  expectedHeightConstraintDropdownOption: string
}) =>
  testChild({
    ...params,
    elementPath: EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:app-root/div/target'),
    snippet: `
  <div data-uid='app-root' style={{ width: 300, height: 300 }}>
    <div data-uid='div' style={{ position: 'absolute', width: 150, height: 150 }}>
      ${params.snippet}
    </div>
  </div>
`,
  })

const testGroupChild = (params: {
  snippet: string
  expectedWidthConstraintDropdownOption: string
  expectedHeightConstraintDropdownOption: string
}) =>
  testChild({
    ...params,
    elementPath: EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:app-root/group/target'),
    snippet: `
  <div data-uid='app-root' style={{ width: 300, height: 300 }}>
    <Group data-uid='group'>
      ${params.snippet}
    </Group>
  </div>
`,
  })

describe('Constraints Section', () => {
  describe('Frame Children', () => {
    it(
      'Span without size shows up as Left / Top constrained',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', left: 15, top: 15 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Left',
        expectedHeightConstraintDropdownOption: 'Top',
      }),
    )
    it(
      'Span with width shows up as Left / Top constrained',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', left: 15, top: 15, width: 150 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Left',
        expectedHeightConstraintDropdownOption: 'Top',
      }),
    )
    it(
      'Span with width,height shows up as Left / Top constrained',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', left: 15, top: 15, width: 150, height: 50 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Left',
        expectedHeightConstraintDropdownOption: 'Top',
      }),
    )

    it(
      'Span without size bottom/right pinned shows up as Right / Bottom constrained',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', right: 15, bottom: 15 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Right',
        expectedHeightConstraintDropdownOption: 'Bottom',
      }),
    )

    it(
      'Span with size bottom/right pinned shows up as Right / Bottom constrained',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', right: 15, bottom: 15, width: 100, height: 30 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Right',
        expectedHeightConstraintDropdownOption: 'Bottom',
      }),
    )

    it(
      'Span with left, width, top, height as percentage shows up as Scale',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', left: '15%', top: '10%', width: '50%', height: '30%' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )
    it(
      'Span with left %, width fixed shows up as Mixed',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', left: '15%', top: '10%', width: 50, height: '30%' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Mixed',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Span with left %, width missing shows up as Mixed',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', left: '15%', top: '10%', width: 50, height: '30%' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Mixed',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Span with left top right bottom px shows up as Left and Right, Bottom and Top',
      testFrameChild({
        snippet: `<span data-uid='target' style={{ position: 'absolute', left: 15, top: 30, right: 15, bottom: 50 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Left and Right',
        expectedHeightConstraintDropdownOption: 'Top and Bottom',
      }),
    )
  })

  describe('Group Children', () => {
    it(
      'Span without size shows up as Scale / Scale constrained',
      testGroupChild({
        snippet: `<span data-uid='target'>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Span with only width shows up as Scale / Scale',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ width: 150 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Span with max-content width shows up as Width / Scale',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ width: 'max-content' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Width',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Span with max-content width height shows up as Width / Height',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ width: 'max-content', height: 'max-content' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Width',
        expectedHeightConstraintDropdownOption: 'Height',
      }),
    )

    it(
      'Span with size shows up as Scale',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ width: 150, height: 30 }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Div with right and bottom pins and size shows up as Right / Bottom',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ right: 150, bottom: 50, width: 150, height: 30 }} data-constraints={['right', 'bottom']}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Right',
        expectedHeightConstraintDropdownOption: 'Bottom',
      }),
    )
  })
})
