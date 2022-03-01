import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { getNumberPropertyFromProps } from '../../../core/shared/jsx-attributes'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { selectComponents, setFocusedElement } from '../../editor/actions/action-creators'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { adjustNumberProperty, runAdjustNumberProperty } from './adjust-number-command'
import { applyStatePatches } from './commands'
import { reparentElement, runReparentElement } from './reparent-element-command'
import { runUpdateSelectedViews, updateSelectedViews } from './update-selected-views-command'
import { runWildcardPatch, wildcardPatch } from './wildcard-patch-command'

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

describe('adjustNumberProperty', () => {
  it('works for left style prop', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      createBuiltInDependenciesList(null),
    )

    const cardInstancePath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])

    const originalEditorState = renderResult.getEditorState().editor

    const originalLeftStyleProp = withUnderlyingTargetFromEditorState(
      cardInstancePath,
      originalEditorState,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return getNumberPropertyFromProps(element.props, stylePropPathMappingFn('left', ['style']))
      },
    )!

    const delta = 10

    const adjustNumberPropertyCommand = adjustNumberProperty(
      'permanent',
      cardInstancePath,
      stylePropPathMappingFn('left', ['style']),
      delta,
      true,
    )

    const result = runAdjustNumberProperty(
      renderResult.getEditorState().editor,
      [],
      adjustNumberPropertyCommand,
    )

    const patchedEditor = applyStatePatches(
      renderResult.getEditorState().editor,
      renderResult.getEditorState().editor,
      [result.editorStatePatch],
    )

    const updatedLeftStyleProp = withUnderlyingTargetFromEditorState(
      cardInstancePath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return getNumberPropertyFromProps(element.props, stylePropPathMappingFn('left', ['style']))
      },
    )

    expect(updatedLeftStyleProp).toEqual(originalLeftStyleProp + delta)
  })
})

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

    const result = runReparentElement(originalEditorState, [], reparentCommand)

    const patchedEditor = applyStatePatches(originalEditorState, originalEditorState, [
      result.editorStatePatch,
    ])
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

describe('updateSelectedViews', () => {
  it('updating selected views work', async () => {
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

    const originalEditorState = renderResult.getEditorState().editor

    const updateSelectedViewsCommand = updateSelectedViews('permanent', [targetPath])

    const result = runUpdateSelectedViews(originalEditorState, [], updateSelectedViewsCommand)

    const patchedEditor = applyStatePatches(originalEditorState, originalEditorState, [
      result.editorStatePatch,
    ])

    expect(patchedEditor.selectedViews).toEqual([targetPath])
  })
})
