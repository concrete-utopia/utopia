import { styleStringInArray } from '../../../utils/common-constants'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { getNumberPropertyFromProps } from '../../../core/shared/jsx-attributes'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import {
  runSetCssLengthProperty,
  setCssLengthProperty,
  setValueKeepingOriginalUnit,
} from './set-css-length-command'

describe('setCssLengthProperty', () => {
  it('works for height style prop', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      createBuiltInDependenciesList(null),
    )

    const cardInstancePath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])

    const valueToSet = 321
    const setCSSPropertyCommand = setCssLengthProperty(
      'always',
      cardInstancePath,
      stylePropPathMappingFn('height', styleStringInArray),
      setValueKeepingOriginalUnit(valueToSet, 400),
    )

    const result = runSetCssLengthProperty(
      renderResult.getEditorState().editor,
      setCSSPropertyCommand,
    )

    const patchedEditor = updateEditorStateWithPatches(
      renderResult.getEditorState().editor,
      result.editorStatePatches,
    )
    const updatedHeightProp = withUnderlyingTargetFromEditorState(
      cardInstancePath,
      patchedEditor,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return getNumberPropertyFromProps(
          element.props,
          stylePropPathMappingFn('height', styleStringInArray),
        )
      },
    )

    expect(updatedHeightProp).toEqual(valueToSet)
  })
})
