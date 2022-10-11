import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { isRight } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  getJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attributes'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { renderTestEditorWithModel, renderTestEditorWithModelContext } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { convertToAbsolute, runConvertToAbsolute } from './convert-to-absolute-command'

describe('convertToAbsolute', () => {
  it('sets position absolute', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      renderTestEditorWithModelContext({
        mockBuiltInDependencies: createBuiltInDependenciesList(null),
      }),
    )

    const appInstancePath = EP.elementPath([['storyboard-entity', 'scene-1-entity', 'app-entity']])

    const originalEditorState = renderResult.getEditorState().editor

    const convertToAbsoluteCommand = convertToAbsolute('always', appInstancePath)
    const result = runConvertToAbsolute(
      renderResult.getEditorState().editor,
      convertToAbsoluteCommand,
    )

    const patchedEditor = updateEditorStateWithPatches(
      renderResult.getEditorState().editor,
      result.editorStatePatches,
    )

    const originalPositionProp = withUnderlyingTargetFromEditorState(
      appInstancePath,
      originalEditorState,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        const jsxAttributeResult = getJSXAttributeAtPath(
          element.props,
          stylePropPathMappingFn('position', ['style']),
        )
        const currentValue = jsxSimpleAttributeToValue(jsxAttributeResult.attribute)
        if (currentValue !== null && isRight(currentValue)) {
          return currentValue.value
        } else {
          return null
        }
      },
    )

    const updatedPositionProp = withUnderlyingTargetFromEditorState(
      appInstancePath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        const jsxAttributeResult = getJSXAttributeAtPath(
          element.props,
          stylePropPathMappingFn('position', ['style']),
        )
        const currentValue = jsxSimpleAttributeToValue(jsxAttributeResult.attribute)
        if (currentValue !== null && isRight(currentValue)) {
          return currentValue.value
        } else {
          return null
        }
      },
    )

    expect(originalPositionProp).toEqual(undefined)
    expect(updatedPositionProp).toEqual('absolute')
  })
})
