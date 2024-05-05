import * as EP from '../../../core/shared/element-path'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import {
  TestAppUID,
  TestSceneUID,
  formatTestProjectCode,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../ui-jsx.test-utils'
import { deleteSelected } from '../../editor/actions/action-creators'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { selectComponentsForTest } from '../../../utils/utils.test-utils'

function makeScenePath(trailingPath: string): ElementPath {
  return EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:${trailingPath}`)
}

describe('push intended bounds', () => {
  describe('hugging elements', () => {
    describe('flex containers', () => {
      xit('keeps the container dimensions when becoming empty (flex) (delete, single element)', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
				<div data-uid='container' data-testid='container' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 100, top: 100, width: 'max-content', height: 'max-content', display: 'flex', flexDirection: 'row', padding: '73px 73px', gap: 44 }}>
					<div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100, contain: 'layout', }} />
					<div data-uid='child2' data-testid='child2' style={{ backgroundColor: '#aaaaaa33', width: 150, height: 120, contain: 'layout', }} />
					<div data-uid='child3' data-testid='child3' style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100, contain: 'layout', }} />
				</div>
        `),
          ),
          'await-first-dom-report',
        )

        async function getRect(testId: string) {
          return (await renderResult.renderedDOM.findByTestId(testId)).getBoundingClientRect()
        }

        const originalRect = await getRect('container')

        await selectComponentsForTest(renderResult, [
          makeScenePath(`container/child1`),
          makeScenePath(`container/child3`),
        ])
        await renderResult.dispatch([deleteSelected()], true)

        const withOneElementRemaining = await getRect('container')
        expect(withOneElementRemaining.width).not.toEqual(originalRect.width)
        expect(withOneElementRemaining.height).toEqual(originalRect.height)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
				<div data-uid='container' data-testid='container' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 100, top: 100, width: 'max-content', height: 'max-content', display: 'flex', flexDirection: 'row', padding: '73px 73px', gap: 44 }}>
					<div data-uid='child2' data-testid='child2' style={{ backgroundColor: '#aaaaaa33', width: 150, height: 120, contain: 'layout', }} />
				</div>
       	   `),
          ),
        )

        await selectComponentsForTest(renderResult, [makeScenePath(`container/child2`)])
        await renderResult.dispatch([deleteSelected()], true)

        const withNoElementsRemaining = await getRect('container')
        expect(withNoElementsRemaining.width).toEqual(withOneElementRemaining.width)
        expect(withNoElementsRemaining.height).toEqual(withOneElementRemaining.height)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
				<div data-uid='container' data-testid='container' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 100, top: 100, width: 296, height: 266, display: 'flex', flexDirection: 'row', padding: '73px 73px', gap: 44 }} />
       	   `),
          ),
        )
      })
      xit('keeps the container dimensions when becoming empty (flex) (delete, multiple elements)', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
				<div data-uid='container' data-testid='container' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 100, top: 100, width: 'max-content', height: 'max-content', display: 'flex', flexDirection: 'row', padding: '73px 73px', gap: 44 }}>
					<div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100, contain: 'layout', }} />
					<div data-uid='child2' data-testid='child2' style={{ backgroundColor: '#aaaaaa33', width: 150, height: 120, contain: 'layout', }} />
					<div data-uid='child3' data-testid='child3' style={{ backgroundColor: '#aaaaaa33', width: 100, height: 100, contain: 'layout', }} />
				</div>
        `),
          ),
          'await-first-dom-report',
        )

        async function getRect(testId: string) {
          return (await renderResult.renderedDOM.findByTestId(testId)).getBoundingClientRect()
        }

        const originalRect = await getRect('container')

        await selectComponentsForTest(renderResult, [
          makeScenePath(`container/child1`),
          makeScenePath(`container/child2`),
          makeScenePath(`container/child3`),
        ])
        await renderResult.dispatch([deleteSelected()], true)

        const withNoElementsRemaining = await getRect('container')
        expect(withNoElementsRemaining.width).toEqual(originalRect.width)
        expect(withNoElementsRemaining.height).toEqual(originalRect.height)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
				<div data-uid='container' data-testid='container' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 100, top: 100, width: 584, height: 266, display: 'flex', flexDirection: 'row', padding: '73px 73px', gap: 44 }} />
       	   `),
          ),
        )
      })
      it('keeps the container dimensions when deleting an element but there are absolutely positioned siblings', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
				<div data-uid='container' data-testid='container' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 10, top: 10, width: 'max-content', height: 'max-content', display: 'flex', flexDirection: 'row', padding: '73px 73px', gap: 44 }}>
					<div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', width: 20, height: 20, contain: 'layout', }} />
					<div data-uid='child2' data-testid='child2' style={{ backgroundColor: '#aaaaaa33', width: 50, height: 20, contain: 'layout', }} />
					<div data-uid='absolute-child' data-testid='absolute-child' style={{ backgroundColor: '#f0f', width: 15, height: 15, contain: 'layout', position: 'absolute', top: 80, left: 10 }} />
					<div data-uid='child3' data-testid='child3' style={{ backgroundColor: '#aaaaaa33', width: 20, height: 20, contain: 'layout', }} />
				</div>
        `),
          ),
          'await-first-dom-report',
        )

        async function getRect(testId: string) {
          return (await renderResult.renderedDOM.findByTestId(testId)).getBoundingClientRect()
        }

        const originalRect = await getRect('container')

        await selectComponentsForTest(renderResult, [
          makeScenePath(`container/child2`),
          makeScenePath(`container/child3`),
        ])

        await renderResult.dispatch([deleteSelected()], true)

        const withTwoElementsRemaining = await getRect('container')
        expect(withTwoElementsRemaining.width).not.toEqual(originalRect.width)
        expect(withTwoElementsRemaining.height).toEqual(originalRect.height)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
				<div data-uid='container' data-testid='container' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 10, top: 10, width: 'max-content', height: 'max-content', display: 'flex', flexDirection: 'row', padding: '73px 73px', gap: 44 }}>
					<div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', width: 20, height: 20, contain: 'layout', }} />
					<div data-uid='absolute-child' data-testid='absolute-child' style={{ backgroundColor: '#f0f', width: 15, height: 15, contain: 'layout', position: 'absolute', top: 80, left: 10 }} />
				</div>
       	   `),
          ),
        )

        await selectComponentsForTest(renderResult, [
          makeScenePath(`container/absolute-child`),
          makeScenePath(`container/absolute-child`),
        ])

        await renderResult.dispatch([deleteSelected()], true)

        const withOneElementRemaining = await getRect('container')
        expect(withOneElementRemaining.width).not.toEqual(originalRect.width)
        expect(withOneElementRemaining.height).toEqual(originalRect.height)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(
            makeTestProjectCodeWithSnippet(`
        		<div data-uid='container' data-testid='container' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 10, top: 10, width: 'max-content', height: 'max-content', display: 'flex', flexDirection: 'row', padding: '73px 73px', gap: 44 }}>
        			<div data-uid='child1' data-testid='child1' style={{ backgroundColor: '#aaaaaa33', width: 20, height: 20, contain: 'layout', }} />
        		</div>
           `),
          ),
        )
      })
      xit('keeps the container dimensions when becoming empty (zero-sized)', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
				import * as React from 'react'
				import { Storyboard, Group } from 'utopia-api'

				export var storyboard = (
					<Storyboard data-uid='sb'>
						<div data-uid='container' data-testid='container' style={{ backgroundColor: 'red' }}>
							<div data-uid='delete-me' style={{ position: 'absolute', top: 100, left: 100, backgroundColor: 'yellow', width: 80, height: 80 }}>delete me pls</div>
						</div>
					</Storyboard>
				)
			`),
          'await-first-dom-report',
        )

        await selectComponentsForTest(renderResult, [EP.fromString(`sb/container/delete-me`)])

        await renderResult.dispatch([deleteSelected()], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(`
			import * as React from 'react'
			import { Storyboard, Group } from 'utopia-api'

			export var storyboard = (
				<Storyboard data-uid='sb'>
					<div data-uid='container' data-testid='container' style={{ backgroundColor: 'red', width: 80, height: 80, position: 'absolute', left: 100, top: 100 }} />
				</Storyboard>
			)
		`),
        )
      })
      xit('keeps the container dimensions when becoming empty (inside a flex container)', async () => {
        const renderResult = await renderTestEditorWithCode(
          formatTestProjectCode(`
				import * as React from 'react'
				import { Storyboard, Group } from 'utopia-api'

				export var storyboard = (
					<Storyboard data-uid='sb'>
						<div data-uid='flex' style={{ display: 'flex', alignItems: 'center', justifyContent: 'center', padding: 10, gap: 10, backgroundColor: 'white', width: 100, height: 100, position: 'absolute', left: 21, top: 17 }}>
							<div data-uid='container' data-testid='container' style={{ backgroundColor: 'red' }}>
								<div data-uid='delete-me' style={{ backgroundColor: 'yellow', width: 80, height: 80 }}>
									delete me pls
								</div>
							</div>
						</div>
					</Storyboard>
				)
			`),
          'await-first-dom-report',
        )

        await selectComponentsForTest(renderResult, [EP.fromString(`sb/flex/container/delete-me`)])

        await renderResult.dispatch([deleteSelected()], true)

        expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
          formatTestProjectCode(`
			import * as React from 'react'
			import { Storyboard, Group } from 'utopia-api'

			export var storyboard = (
				<Storyboard data-uid='sb'>
					<div data-uid='flex' style={{ display: 'flex', alignItems: 'center', justifyContent: 'center', padding: 10, gap: 10, backgroundColor: 'white', width: 100, height: 100, position: 'absolute', left: 21, top: 17 }}>
						<div data-uid='container' data-testid='container' style={{ backgroundColor: 'red', width: 80, height: 80 }} />
					</div>
				</Storyboard>
			)
		`),
        )
      })
    })
  })
})
