import update from 'immutability-helper'
import { styleStringInArray } from '../../../utils/common-constants'
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
} from '../ui-jsx.test-utils'
import { adjustNumberProperty, runAdjustNumberProperty } from './adjust-number-command'
import { updateEditorStateWithPatches } from './commands'
import { isJSXElement } from '../../../core/shared/element-template'

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
        if (isJSXElement(element)) {
          return getNumberPropertyFromProps(
            element.props,
            stylePropPathMappingFn('left', styleStringInArray),
          )
        } else {
          return null
        }
      },
    )!

    const delta = 10

    const adjustNumberPropertyCommand = adjustNumberProperty(
      'always',
      cardInstancePath,
      stylePropPathMappingFn('left', styleStringInArray),
      delta,
      true,
    )

    const result = runAdjustNumberProperty(
      renderResult.getEditorState().editor,
      adjustNumberPropertyCommand,
      { reparentedPathsLookup: {} },
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
        if (isJSXElement(element)) {
          return getNumberPropertyFromProps(
            element.props,
            stylePropPathMappingFn('left', styleStringInArray),
          )
        } else {
          return null
        }
      },
    )

    expect(updatedLeftStyleProp).toEqual(originalLeftStyleProp + delta)
  })
  it('works for missing left style prop', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(` <View style={{ ...(props.style || {}) }} data-uid='parent'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', top: 50, width: 250, height: 300 }}
          data-uid='child'
        />
      </View>`),
      'dont-await-first-dom-report',
    )

    const elementPath = EP.elementPath([
      ['scene-aaa', 'app-entity'],
      ['parent', 'child'],
    ])

    const delta = 10

    // left prop is missing, it should be just set to the delta value
    const adjustNumberPropertyCommand = adjustNumberProperty(
      'always',
      elementPath,
      stylePropPathMappingFn('left', styleStringInArray),
      delta,
      true,
    )

    const result = runAdjustNumberProperty(
      renderResult.getEditorState().editor,
      adjustNumberPropertyCommand,
      { reparentedPathsLookup: {} },
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
        if (isJSXElement(element)) {
          return getNumberPropertyFromProps(
            element.props,
            stylePropPathMappingFn('left', styleStringInArray),
          )
        } else {
          return null
        }
      },
    )

    expect(updatedLeftStyleProp).toEqual(delta)
  })
})
