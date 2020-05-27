import { createEditorStates } from '../../utils/test-utils'
import { addUtopiaUtilsImportIfUsed } from './import-utils'
import {
  getOpenImportsFromState,
  modifyOpenParseSuccess,
  EditorState,
  transformElementAtPath,
  modifyOpenScenesAndJSXElements,
  removeElementAtPath,
} from './store/editor-state'
import { Imports, ImportDetails, importAlias } from '../../core/shared/project-file-types'
import * as TP from '../../core/shared/template-path'
import { ScenePathForTestUiJsFile } from '../../core/model/test-ui-js-file'

function editorStateWithoutUtopiaUtils(removeFunctionCalls: boolean): EditorState {
  const { editor } = createEditorStates()
  const withImportRemoved = modifyOpenParseSuccess((parseSuccess) => {
    let updatedImports: Imports = {
      ...parseSuccess.imports,
    }
    if ('utopia-api' in updatedImports) {
      const utopiaAPIImport = updatedImports['utopia-api']
      const updatedUtopiaAPIImport: ImportDetails = {
        ...utopiaAPIImport,
        importedFromWithin: utopiaAPIImport.importedFromWithin.filter(
          (fromWithin) => fromWithin.name !== 'UtopiaUtils',
        ),
      }
      updatedImports['utopia-api'] = updatedUtopiaAPIImport
    }
    return {
      ...parseSuccess,
      imports: updatedImports,
    }
  }, editor)

  if (removeFunctionCalls) {
    return modifyOpenScenesAndJSXElements((components) => {
      return removeElementAtPath(
        TP.instancePath(ScenePathForTestUiJsFile, ['aaa', 'ddd']),
        components,
        editor.jsxMetadataKILLME,
      )
    }, withImportRemoved)
  } else {
    return withImportRemoved
  }
}

describe('addUtopiaUtilsImportIfUsed', () => {
  it('import is added if a function is used in the project', () => {
    const editor = editorStateWithoutUtopiaUtils(false)
    const result = addUtopiaUtilsImportIfUsed(editor)
    const imports = getOpenImportsFromState(result)
    if ('utopia-api' in imports) {
      expect(imports['utopia-api'].importedFromWithin).toContainEqual(importAlias('UtopiaUtils'))
    }
  })
  it('import is not added if a function is not used in the project', () => {
    const editor = editorStateWithoutUtopiaUtils(true)
    const result = addUtopiaUtilsImportIfUsed(editor)
    const imports = getOpenImportsFromState(result)
    if ('utopia-api' in imports) {
      expect(imports['utopia-api'].importedFromWithin).not.toContainEqual(
        importAlias('UtopiaUtils'),
      )
    }
  })
})
