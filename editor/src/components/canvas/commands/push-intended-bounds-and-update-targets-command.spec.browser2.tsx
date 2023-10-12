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

describe('PushIntendedBoundsAndUpdateTargets', () => {
  describe('empty elements', () => {
    describe('flex containers', () => {
      it('keeps the container dimensions when becoming empty (delete, single element)', async () => {
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
      it('keeps the container dimensions when becoming empty (delete, multiple elements)', async () => {
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
    })
  })
})
