import { getProjectFileByFilePath } from '../../../components/assets'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { childInsertionPath } from '../../editor/store/insertion-path'
import { DefaultStartingFeatureSwitches, renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { reparentElement, runReparentElement } from './reparent-element-command'
import {
  importAlias,
  importDetails,
  isParseSuccess,
  type TextFile,
} from '../../../core/shared/project-file-types'

describe('runReparentElement', () => {
  it('reparent works', async () => {
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

    const newParentPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div', 'card-inner-div'],
    ])
    const originalEditorState = renderResult.getEditorState().editor

    const reparentCommand = reparentElement('always', targetPath, childInsertionPath(newParentPath))

    const result = runReparentElement(originalEditorState, reparentCommand)

    const patchedEditor = updateEditorStateWithPatches(
      originalEditorState,
      result.editorStatePatches,
    )
    const newPath = EP.appendToPath(newParentPath, EP.toUid(targetPath))

    const newElement = withUnderlyingTargetFromEditorState(
      newPath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return element
      },
    )

    const oldElement = withUnderlyingTargetFromEditorState(
      targetPath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return element
      },
    )

    expect(newElement).not.toBeNull()
    expect(oldElement).toBeNull()
  })
  it('reparent works across files', async () => {
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

    const newParentPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div'],
    ])
    const originalEditorState = renderResult.getEditorState().editor

    const reparentCommand = reparentElement('always', targetPath, childInsertionPath(newParentPath))

    const result = runReparentElement(originalEditorState, reparentCommand)

    const { oldFile, oldFilePreviousImports } = withUnderlyingTargetFromEditorState(
      targetPath,
      originalEditorState,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return { oldFile: underlyingFilePath, oldFilePreviousImports: success.imports }
      },
    )!

    const patchedEditor = updateEditorStateWithPatches(
      originalEditorState,
      result.editorStatePatches,
    )
    const newPath = EP.appendToPath(newParentPath, EP.toUid(targetPath))

    const { newElement, newFile } = withUnderlyingTargetFromEditorState(
      newPath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return {
          newElement: element,
          newFile: underlyingFilePath,
        }
      },
    )!

    const oldElement = withUnderlyingTargetFromEditorState(
      targetPath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return element
      },
    )!

    const oldFileParsed = (
      getProjectFileByFilePath(patchedEditor.projectContents, oldFile) as TextFile
    ).fileContents.parsed
    const oldFileImports = isParseSuccess(oldFileParsed) ? oldFileParsed.imports : null

    expect(oldElement).toBeNull()
    expect(newElement).not.toBeNull()
    expect(oldFile).toBe('/src/card.js')
    expect(newFile).toBe('/src/app.js')
    // make sure we removed the unnecessary import
    expect(oldFilePreviousImports?.['non-existant-dummy-library']).toEqual(
      importDetails(null, [importAlias('Spring')], null),
    )
    expect(oldFileImports?.['non-existant-dummy-library']).toBeUndefined()
  })
})
