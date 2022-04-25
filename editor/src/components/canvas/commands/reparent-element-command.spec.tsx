import update from 'immutability-helper'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { reparentElement, runReparentElement } from './reparent-element-command'

describe('runReparentElement', () => {
  it('reparent works', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      createBuiltInDependenciesList(null),
    )

    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div', 'card-inner-rectangle'],
    ])

    const newParentPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div', 'card-inner-div'],
    ])
    const originalEditorState = renderResult.getEditorState().editor

    const reparentCommand = reparentElement('permanent', targetPath, newParentPath)

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
})
