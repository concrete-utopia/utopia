import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { selectComponents, setFocusedElement } from '../../editor/actions/action-creators'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { runWildcardPatch } from './command-runners'
import { applyStatePatches, wildcardPatch } from './commands'

describe('wildcardPatch', () => {
  it('works for a basic pinned element', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
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

    const wildcardCommand = wildcardPatch('permanent', { selectedViews: { $set: [] } })

    const result = runWildcardPatch(renderResult.getEditorState().editor, [], wildcardCommand)

    const patchedEditor = applyStatePatches(
      renderResult.getEditorState().editor,
      renderResult.getEditorState().editor,
      [result.editorStatePatch],
    )
    expect(patchedEditor.selectedViews).toEqual([])
  })
})
