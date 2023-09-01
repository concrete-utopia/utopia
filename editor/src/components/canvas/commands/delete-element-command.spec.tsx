import { getProjectFileByFilePath } from '../../../components/assets'
import { applyCommandsAction } from '../../../components/editor/actions/action-creators'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { safeIndex } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { isArbitraryJSBlock } from '../../../core/shared/element-template'
import {
  fromField,
  fromTypeGuard,
  notNull,
  traverseArray,
} from '../../../core/shared/optics/optic-creators'
import { unsafeGet } from '../../../core/shared/optics/optic-utilities'
import { forceNotNull } from '../../../core/shared/optional-utils'
import type { ProjectFile } from '../../../core/shared/project-file-types'
import { isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import { simpleDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import {
  DefaultStartingFeatureSwitches,
  getPrintedUiJsCodeWithoutUIDs,
  renderTestEditorWithModel,
} from '../ui-jsx.test-utils'
import { deleteElement } from './delete-element-command'

describe('runDeleteElement', () => {
  it('removes a root element and replaces it with a null', async () => {
    const renderResult = await renderTestEditorWithModel(
      simpleDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
    )

    const originalEditorState = renderResult.getEditorState().editor

    const allPathsBefore = MetadataUtils.getAllPaths(
      originalEditorState.jsxMetadata,
      originalEditorState.elementPathTree,
    )
    const rootElementsOfInstancesBefore = allPathsBefore.filter(EP.isRootElementOfInstance)
    expect(rootElementsOfInstancesBefore).toHaveLength(1)
    const appRootBefore = forceNotNull(
      'Root element not at index 0!',
      safeIndex(rootElementsOfInstancesBefore, 0),
    )

    const deleteElementCommand = deleteElement('always', appRootBefore)

    await renderResult.dispatch([applyCommandsAction([deleteElementCommand])], true)

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState(), '/src/app.js'))
      .toEqual(`import * as React from 'react'
export var App = (props) => {
  return null
}
`)
    const appFile = getProjectFileByFilePath(
      renderResult.getEditorState().editor.projectContents,
      '/src/app.js',
    )
    const toArbitraryJSJavaScriptOptic = notNull<ProjectFile>()
      .compose(fromTypeGuard(isTextFile))
      .compose(fromField('fileContents'))
      .compose(fromField('parsed'))
      .compose(fromTypeGuard(isParseSuccess))
      .compose(fromField('topLevelElements'))
      .compose(traverseArray())
      .compose(fromTypeGuard(isArbitraryJSBlock))
      .compose(fromField('javascript'))
    expect(unsafeGet(toArbitraryJSJavaScriptOptic, appFile)).toEqual(`export var App = (props) => {
  return null
}`)
  })
})
