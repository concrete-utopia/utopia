import update from 'immutability-helper'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { getNumberPropertyFromProps } from '../../../core/shared/jsx-attributes'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  renderTestEditorWithModelContext,
} from '../ui-jsx.test-utils'
import { adjustNumberProperty, runAdjustNumberProperty } from './adjust-number-command'
import { updateEditorStateWithPatches } from './commands'

describe('adjustNumberProperty', () => {
  it('works for left style prop', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      renderTestEditorWithModelContext({
        mockBuiltInDependencies: createBuiltInDependenciesList(null),
      }),
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
      'always',
      cardInstancePath,
      stylePropPathMappingFn('left', ['style']),
      delta,
      true,
    )

    const result = runAdjustNumberProperty(
      renderResult.getEditorState().editor,
      adjustNumberPropertyCommand,
    )

    const patchedEditor = updateEditorStateWithPatches(
      renderResult.getEditorState().editor,
      result.editorStatePatches,
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
  it('works for missing left style prop', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(` <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', top: 50, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`),
      'dont-await-first-dom-report',
    )

    const elementPath = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const delta = 10

    // left prop is missing, it should be just set to the delta value
    const adjustNumberPropertyCommand = adjustNumberProperty(
      'always',
      elementPath,
      stylePropPathMappingFn('left', ['style']),
      delta,
      true,
    )

    const result = runAdjustNumberProperty(
      renderResult.getEditorState().editor,
      adjustNumberPropertyCommand,
    )

    const patchedEditor = updateEditorStateWithPatches(
      renderResult.getEditorState().editor,
      result.editorStatePatches,
    )
    const updatedLeftStyleProp = withUnderlyingTargetFromEditorState(
      elementPath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return getNumberPropertyFromProps(element.props, stylePropPathMappingFn('left', ['style']))
      },
    )

    expect(updatedLeftStyleProp).toEqual(delta)
  })
})
