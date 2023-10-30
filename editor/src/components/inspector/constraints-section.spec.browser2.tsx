/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "testGroupChild", "testFrameChild"] }] */

import { screen } from '@testing-library/react'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from '../../core/shared/project-file-types'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  formatTestProjectCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { selectComponents } from '../editor/actions/action-creators'
import { InspectorSectionConstraintsTestId } from './constraints-section'
import { pickFromReactSelectPopupList } from '../canvas/event-helpers.test-utils'

const testChild = (params: {
  snippet: string
  elementPath: ElementPath
  expectedWidthConstraintDropdownOption: string
  expectedHeightConstraintDropdownOption: string
  after?: (renderResult: EditorRenderResult) => Promise<void>
}) =>
  async function testBody() {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(params.snippet),
      'await-first-dom-report',
    )

    await renderResult.dispatch([selectComponents([params.elementPath], false)], true)

    const widthConstraintDropdown = renderResult.renderedDOM.getByTestId(
      'frame-child-constraint-width-popuplist',
    )
    expect(widthConstraintDropdown.textContent).toEqual(
      params.expectedWidthConstraintDropdownOption,
    )

    const heightConstraintDropdown = renderResult.renderedDOM.getByTestId(
      'frame-child-constraint-height-popuplist',
    )
    expect(heightConstraintDropdown.textContent).toEqual(
      params.expectedHeightConstraintDropdownOption,
    )

    if (params.after != null) {
      await params.after(renderResult)
    }
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
  after?: (renderResult: EditorRenderResult) => Promise<void>
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
        snippet: `<span data-uid='target' style={{ position: 'absolute' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Span with only width shows up as Scale / Scale',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ width: 150, position: 'absolute' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Span with max-content width shows up as Width / Scale',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ width: 'max-content', position: 'absolute' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Width',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Element with max-content width does not change if selecting the same option again',
      testGroupChild({
        snippet: `<span data-uid='target' data-testid='target' style={{ width: 'max-content', position: 'absolute' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Width',
        expectedHeightConstraintDropdownOption: 'Scale',
        after: async (renderResult) => {
          await renderResult.dispatch(
            [
              selectComponents(
                [EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:app-root/group/target`)],
                false,
              ),
            ],
            true,
          )
          await pickFromReactSelectPopupList(
            renderResult,
            'frame-child-constraint-width-popuplist',
            'Width',
            'Width',
          )
          const target = await renderResult.renderedDOM.findByTestId('target')
          expect(target.style.width).toBe('max-content')
        },
      }),
    )

    it(
      'Span with max-content width height shows up as Width / Height',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ width: 'max-content', height: 'max-content', position: 'absolute' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Width',
        expectedHeightConstraintDropdownOption: 'Height',
      }),
    )

    it(
      'Span with size shows up as Scale',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ width: 150, height: 30, position: 'absolute' }}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Scale',
        expectedHeightConstraintDropdownOption: 'Scale',
      }),
    )

    it(
      'Div with right and bottom pins and size shows up as Right / Bottom',
      testGroupChild({
        snippet: `<span data-uid='target' style={{ right: 150, bottom: 50, width: 150, height: 30, position: 'absolute' }} data-constraints={['right', 'bottom']}>An implicitly constrained span</span>`,
        expectedWidthConstraintDropdownOption: 'Right',
        expectedHeightConstraintDropdownOption: 'Bottom',
      }),
    )
  })

  describe('hidden behaviour', () => {
    it('is hidden when the selection is made of groups', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
		  import * as React from 'react'
		  import { Group, Storyboard } from 'utopia-api'

		  var storyboard = () => {
			return (
				<Storyboard data-uid='sb'>
					<Group data-uid='group' style={{ position: 'absolute', left: 0, top: 0, width: 164, height: 129 }}>
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 70, height: 70 }} />
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 84, top: 49, width: 80, height: 80 }} />
    				</Group>
				</Storyboard>
			)
		  }
	  `),
        'await-first-dom-report',
      )

      await renderResult.dispatch([selectComponents([EP.fromString('sb/group')], true)], true)

      const section = screen.queryByTestId(InspectorSectionConstraintsTestId)
      expect(section).toBeNull()
    })
    it('is hidden when the selection contains groups', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
		  import * as React from 'react'
		  import { Group, Storyboard } from 'utopia-api'

		  var storyboard = () => {
			return (
				<Storyboard data-uid='sb'>
					<Group data-uid='group' style={{ position: 'absolute', left: 0, top: 0, width: 164, height: 129 }}>
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 70, height: 70 }} />
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 84, top: 49, width: 80, height: 80 }} />
    				</Group>
					<div data-uid='foo' style={{ position: 'absolute', left: 200, top: 200, width: 50, height: 50, background: 'red' }} />
				</Storyboard>
			)
		  }
	  `),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [selectComponents([EP.fromString('sb/group'), EP.fromString('sb/foo')], true)],
        true,
      )

      const section = screen.queryByTestId(InspectorSectionConstraintsTestId)
      expect(section).toBeNull()
    })
    it('is hidden when the selection contains group children and non-group-children', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
		  import * as React from 'react'
		  import { Group, Storyboard } from 'utopia-api'

		  var storyboard = () => {
			return (
				<Storyboard data-uid='sb'>
					<Group data-uid='group' style={{ position: 'absolute', left: 0, top: 0, width: 164, height: 129 }}>
      					<div data-uid='child1' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 70, height: 70 }} />
      					<div data-uid='child2' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 84, top: 49, width: 80, height: 80 }} />
    				</Group>
					<div data-uid='foo' style={{ position: 'absolute', left: 200, top: 200, width: 50, height: 50, background: 'red' }} />
				</Storyboard>
			)
		  }
	  `),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [selectComponents([EP.fromString('sb/group/child1'), EP.fromString('sb/foo')], true)],
        true,
      )

      const section = screen.queryByTestId(InspectorSectionConstraintsTestId)
      expect(section).toBeNull()
    })
    it('is hidden when the selection contains non-absolute elements', async () => {
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
		  import * as React from 'react'
		  import { Group, Storyboard } from 'utopia-api'

		  var storyboard = () => {
			return (
				<Storyboard data-uid='sb'>
					<Group data-uid='group' style={{ position: 'absolute', left: 0, top: 0, width: 164, height: 129 }}>
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 70, height: 70 }} />
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 84, top: 49, width: 80, height: 80 }} />
    				</Group>
					<div data-uid='flex' style={{ position: 'absolute', left: 200, top: 200, width: 'max-content', height: 'max-content', display: 'flex', gap: 10 }}>
						<div data-uid='foo1' style={{ backgroundColor: '#f0f', width: 70, height: 70 }} />
						<div data-uid='foo2' style={{ backgroundColor: '#f0f', width: 70, height: 70 }} />
						<div data-uid='foo3' style={{ backgroundColor: '#f0f', width: 70, height: 70 }} />
					</div>
				</Storyboard>
			)
		  }
	  `),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [selectComponents([EP.fromString('sb/group'), EP.fromString('sb/flex/foo2')], true)],
        true,
      )

      const section = screen.queryByTestId(InspectorSectionConstraintsTestId)
      expect(section).toBeNull()
    })
    it('is hidden when the selection contains non-absolute elements (bad group child)', async () => {
      // most likely this will never happen in reality, because group children are always absolute, but
      // this is to validate the intersection with the other conditions.
      const renderResult = await renderTestEditorWithCode(
        formatTestProjectCode(`
		  import * as React from 'react'
		  import { Group, Storyboard } from 'utopia-api'

		  var storyboard = () => {
			return (
				<Storyboard data-uid='sb'>
					<Group data-uid='group1' style={{ position: 'absolute', left: 0, top: 0, width: 164, height: 129 }}>
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 70, height: 70 }} />
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 84, top: 49, width: 80, height: 80 }} />
    				</Group>
					<Group data-uid='group2' style={{ position: 'absolute', left: 0, top: 0, width: 164, height: 129 }}>
      					<div style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 70, height: 70 }} />
      					<div data-uid='bad' style={{ backgroundColor: '#f00', position: 'relative', left: 84, top: 49, width: 80, height: 80 }} />
    				</Group>
				</Storyboard>
			)
		  }
	  `),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [selectComponents([EP.fromString('sb/group'), EP.fromString('sb/grop2/bad')], true)],
        true,
      )

      const section = screen.queryByTestId(InspectorSectionConstraintsTestId)
      expect(section).toBeNull()
    })
  })
})
