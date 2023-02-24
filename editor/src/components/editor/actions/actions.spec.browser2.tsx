/* eslint-disable jest/expect-expect */
import * as EP from '../../../core/shared/element-path'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestSceneUID,
} from '../../../components/canvas/ui-jsx.test-utils'
import { deleteSelected, selectComponents } from './action-creators'
import { ElementPath } from '../../../core/shared/project-file-types'

async function deleteFromScene(
  inputSnippet: string,
  targets: ElementPath[],
): Promise<{ code: string; selection: ElementPath[] }> {
  const renderResult = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(inputSnippet),
    'await-first-dom-report',
  )
  await renderResult.dispatch([selectComponents(targets, true)], true)
  await renderResult.dispatch([deleteSelected()], true)

  return {
    code: getPrintedUiJsCode(renderResult.getEditorState()),
    selection: renderResult.getEditorState().editor.selectedViews,
  }
}

function makeTargetPath(suffix: string): ElementPath {
  return EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:${suffix}`)
}

describe('actions', () => {
  describe('DELETE_SELECTED', () => {
    const tests: {
      name: string
      input: string
      targets: ElementPath[]
      wantCode: string
      wantSelection: ElementPath[]
    }[] = [
      {
        name: 'delete selected element',
        input: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#09f', width: 50, height: 50 }}
				data-uid='bbb'
				data-testid='bbb'
			/>
			<View
				style={{ background: '#f90', width: 50, height: 50 }}
				data-uid='ccc'
				data-testid='ccc'
			/>
		</View>
		`,
        targets: [makeTargetPath('aaa/bbb')],
        wantCode: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#f90', width: 50, height: 50 }}
				data-uid='ccc'
				data-testid='ccc'
			/>
		</View>
		`,
        wantSelection: [makeTargetPath('aaa')],
      },
      {
        name: 'delete multiple elements',
        input: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#09f', width: 50, height: 50 }}
				data-uid='bbb'
				data-testid='bbb'
			/>
			<View
				style={{ background: '#f90', width: 50, height: 50 }}
				data-uid='ccc'
				data-testid='ccc'
			/>
			<View
				style={{ background: '#f09', width: 50, height: 50 }}
				data-uid='ddd'
				data-testid='ddd'
			/>
		</View>
		`,
        targets: [makeTargetPath('aaa/bbb'), makeTargetPath('aaa/ddd')],
        wantCode: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#f90', width: 50, height: 50 }}
				data-uid='ccc'
				data-testid='ccc'
			/>
		</View>
		`,
        wantSelection: [makeTargetPath('aaa')],
      },
      {
        name: 'delete empty fragments (single fragment)',
        input: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#09f', width: 50, height: 50 }}
				data-uid='bbb'
				data-testid='bbb'
			/>
			<React.Fragment data-uid='000'>
				<View
					style={{ background: '#f90', width: 50, height: 50 }}
					data-uid='ccc'
					data-testid='ccc'
				/>
			</React.Fragment>
			<View
				style={{ background: '#f09', width: 50, height: 50 }}
				data-uid='ddd'
				data-testid='ddd'
			/>
		</View>
		`,
        targets: [makeTargetPath('aaa/000/ccc')],
        wantCode: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#09f', width: 50, height: 50 }}
				data-uid='bbb'
				data-testid='bbb'
			/>
			<View
				style={{ background: '#f09', width: 50, height: 50 }}
				data-uid='ddd'
				data-testid='ddd'
			/>
		</View>
		`,
        wantSelection: [makeTargetPath('aaa')],
      },
      {
        name: "don't delete fragments if not empty",
        input: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#09f', width: 50, height: 50 }}
				data-uid='bbb'
				data-testid='bbb'
			/>
			<React.Fragment data-uid='000'>
				<View
					style={{ background: '#f90', width: 50, height: 50 }}
					data-uid='ccc'
					data-testid='ccc'
				/>
				<View
					style={{ background: '#90f', width: 50, height: 50 }}
					data-uid='eee'
					data-testid='eee'
				/>
			</React.Fragment>
			<View
				style={{ background: '#f09', width: 50, height: 50 }}
				data-uid='ddd'
				data-testid='ddd'
			/>
		</View>
		`,
        targets: [makeTargetPath('aaa/000/eee')],
        wantCode: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#09f', width: 50, height: 50 }}
				data-uid='bbb'
				data-testid='bbb'
			/>
			<React.Fragment>
				<View
					style={{ background: '#f90', width: 50, height: 50 }}
					data-uid='ccc'
					data-testid='ccc'
				/>
			</React.Fragment>
			<View
				style={{ background: '#f09', width: 50, height: 50 }}
				data-uid='ddd'
				data-testid='ddd'
			/>
		</View>
		`,
        wantSelection: [makeTargetPath('aaa/000')],
      },
      {
        name: 'delete empty fragments (multiple targets)',
        input: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#09f', width: 50, height: 50 }}
				data-uid='bbb'
				data-testid='bbb'
			/>
			<React.Fragment data-uid='000'>
				<View
					style={{ background: '#f90', width: 50, height: 50 }}
					data-uid='ccc'
					data-testid='ccc'
				/>
				<View
					style={{ background: '#90f', width: 50, height: 50 }}
					data-uid='eee'
					data-testid='eee'
				/>
			</React.Fragment>
			<View
				style={{ background: '#f09', width: 50, height: 50 }}
				data-uid='ddd'
				data-testid='ddd'
			/>
			<React.Fragment data-uid='001'>
				<View
					style={{ background: '#0f9', width: 50, height: 50 }}
					data-uid='fff'
					data-testid='fff'
				/>
				<View
					style={{ background: '#9f0', width: 50, height: 50 }}
					data-uid='ggg'
					data-testid='ggg'
				/>
			</React.Fragment>
		</View>
		`,
        targets: [
          makeTargetPath('aaa/000/eee'),
          makeTargetPath('aaa/001/fff'),
          makeTargetPath('aaa/001/ggg'),
        ],
        wantCode: `
		<View data-uid='aaa'>
			<View
				style={{ background: '#09f', width: 50, height: 50 }}
				data-uid='bbb'
				data-testid='bbb'
			/>
			<React.Fragment>
				<View
					style={{ background: '#f90', width: 50, height: 50 }}
					data-uid='ccc'
					data-testid='ccc'
				/>
			</React.Fragment>
			<View
				style={{ background: '#f09', width: 50, height: 50 }}
				data-uid='ddd'
				data-testid='ddd'
			/>
		</View>
		`,
        wantSelection: [makeTargetPath('aaa/000'), makeTargetPath('aaa')],
      },
    ]
    tests.forEach((tt, idx) => {
      it(`(${idx + 1}) ${tt.name}`, async () => {
        const got = await deleteFromScene(tt.input, tt.targets)
        expect(got.code).toEqual(makeTestProjectCodeWithSnippet(tt.wantCode))
        expect(got.selection).toEqual(tt.wantSelection)
      })
    })
  })
})
