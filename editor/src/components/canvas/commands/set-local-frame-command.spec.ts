import update from 'immutability-helper'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import { getNumberPropertyFromProps } from '../../../core/shared/jsx-attributes'
import { localRectangle } from '../../../core/shared/math-utils'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { runSetLocalFrame, setLocalFrame } from './set-local-frame-command'

describe('setLocalFrameCommand', () => {
  it('sets TLWH properties', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      createBuiltInDependenciesList(null),
    )

    const cardInstancePath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])

    const frame = localRectangle({ x: 12, y: 28, width: 123, height: 321 })
    const setLocalFrameCommand = setLocalFrame('permanent', cardInstancePath, frame)

    const result = runSetLocalFrame(renderResult.getEditorState().editor, setLocalFrameCommand)

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
          stylePropPathMappingFn('height', ['style']),
        )
      },
    )

    expect(updatedHeightProp).toEqual(frame.height)
  })
})
