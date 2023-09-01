import update from 'immutability-helper'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { selectComponents, setFocusedElement } from '../../editor/actions/action-creators'
import { DefaultStartingFeatureSwitches, renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { runWildcardPatch, wildcardPatch } from './wildcard-patch-command'

describe('wildcardPatch', () => {
  it('works for a basic pinned element', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
    )

    const cardInstancePath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])

    await renderResult.dispatch(
      [selectComponents([cardInstancePath], false), setFocusedElement(cardInstancePath)],
      false,
    )

    const wildcardCommand = wildcardPatch('always', { selectedViews: { $set: [] } })

    const result = runWildcardPatch(renderResult.getEditorState().editor, wildcardCommand)

    const patchedEditor = updateEditorStateWithPatches(
      renderResult.getEditorState().editor,
      result.editorStatePatches,
    )
    expect(patchedEditor.selectedViews).toEqual([])
  })
})
