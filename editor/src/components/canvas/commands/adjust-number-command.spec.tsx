import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { getNumberPropertyFromProps } from '../../../core/shared/jsx-attributes'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { adjustNumberProperty, runAdjustNumberProperty } from './adjust-number-command'
import { applyStatePatches } from './commands'

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
