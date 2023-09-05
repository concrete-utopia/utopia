import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import {
  DefaultStartingFeatureSwitches,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { duplicateElement, runDuplicateElement } from './duplicate-element-command'

describe('runDuplicateElement', () => {
  it('duplicate works inside component', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
    )

    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div', 'card-inner-spring'],
    ])

    const originalEditorState = renderResult.getEditorState().editor

    const newUid = 'new-uid'
    const newPath = EP.appendToPath(EP.parentPath(targetPath), newUid)

    const duplicateCommand = duplicateElement('always', targetPath, newUid)

    const result = runDuplicateElement(originalEditorState, duplicateCommand)

    const patchedEditor = updateEditorStateWithPatches(
      originalEditorState,
      result.editorStatePatches,
    )

    const newElement = withUnderlyingTargetFromEditorState(
      newPath,
      patchedEditor,
      null,
      (_, element) => element,
    )

    expect(newElement).not.toBeNull()
  })

  it('duplicate works for component', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
    )

    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])

    const originalEditorState = renderResult.getEditorState().editor

    const newUid = 'new-uid'
    const newPath = EP.appendToPath(EP.parentPath(targetPath), newUid)

    const duplicateCommand = duplicateElement('always', targetPath, newUid)

    const result = runDuplicateElement(originalEditorState, duplicateCommand)

    const patchedEditor = updateEditorStateWithPatches(
      originalEditorState,
      result.editorStatePatches,
    )

    const newElement = withUnderlyingTargetFromEditorState(
      newPath,
      patchedEditor,
      null,
      (_, element) => element,
    )

    expect(newElement).not.toBeNull()
  })

  it('duplicate works for element with children', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `
          <div
            data-uid="app-outer"
            style={{
              display: "flex",
              gap: 10,
            }}
          >
            <div
              data-uid="parent-1"
              style={{
                width: 200,
                height: 300,
              }}
            >
              <div
                data-uid="child-1"
                style={{
                  width: 200,
                  height: 300,
                }}
              >
                <div
                  data-uid="child-2"
                  style={{
                    width: 200,
                    height: 300,
                  }}
                />
              </div>
            </div>
          </div>
  `,
      ),
      'dont-await-first-dom-report',
    )

    const targetPath = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['app-outer', 'parent-1'],
    ])

    const originalEditorState = renderResult.getEditorState().editor

    const newUid = 'new-uid'
    const newPath = EP.appendToPath(EP.parentPath(targetPath), newUid)

    const duplicateCommand = duplicateElement('always', targetPath, newUid)

    const result = runDuplicateElement(originalEditorState, duplicateCommand)

    const patchedEditor = updateEditorStateWithPatches(
      originalEditorState,
      result.editorStatePatches,
    )

    const newElement = withUnderlyingTargetFromEditorState(
      newPath,
      patchedEditor,
      null,
      (_, element) => element,
    )

    expect(newElement).not.toBeNull()
  })
})
