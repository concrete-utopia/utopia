import update from 'immutability-helper'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { DefaultStartingFeatureSwitches, renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { runUpdateSelectedViews, updateSelectedViews } from './update-selected-views-command'

describe('updateSelectedViews', () => {
  it('updating selected views work', async () => {
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

    const updateSelectedViewsCommand = updateSelectedViews('always', [targetPath])

    const result = runUpdateSelectedViews(originalEditorState, updateSelectedViewsCommand)

    const patchedEditor = updateEditorStateWithPatches(
      originalEditorState,
      result.editorStatePatches,
    )

    expect(patchedEditor.selectedViews).toEqual([targetPath])
  })
})
