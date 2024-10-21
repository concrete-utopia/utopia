import { styleStringInArray } from '../../../utils/common-constants'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { isRight } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import {
  getJSXAttributesAtPath,
  jsxSimpleAttributeToValue,
} from '../../../core/shared/jsx-attribute-utils'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { DefaultStartingFeatureSwitches, renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { convertToAbsolute, runConvertToAbsolute } from './convert-to-absolute-command'
import { isJSXElement } from '../../../core/shared/element-template'

describe('convertToAbsolute', () => {
  it('sets position absolute', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
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
        if (isJSXElement(element)) {
          const jsxAttributeResult = getJSXAttributesAtPath(
            element.props,
            stylePropPathMappingFn('position', styleStringInArray),
          )
          const currentValue = jsxSimpleAttributeToValue(jsxAttributeResult.attribute)
          if (currentValue !== null && isRight(currentValue)) {
            return currentValue.value
          } else {
            return null
          }
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
        if (isJSXElement(element)) {
          const jsxAttributeResult = getJSXAttributesAtPath(
            element.props,
            stylePropPathMappingFn('position', styleStringInArray),
          )
          const currentValue = jsxSimpleAttributeToValue(jsxAttributeResult.attribute)
          if (currentValue !== null && isRight(currentValue)) {
            return currentValue.value
          } else {
            return null
          }
        } else {
          return null
        }
      },
    )

    expect(originalPositionProp).toEqual(undefined)
    expect(updatedPositionProp).toEqual('absolute')
  })
})
