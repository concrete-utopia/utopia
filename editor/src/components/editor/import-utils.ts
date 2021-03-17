import { addImport } from '../../core/workers/common/project-file-utils'
import {
  modifyOpenParseSuccess,
  EditorState,
  getOpenUtopiaJSXComponentsFromState,
  getOpenImportsFromState,
} from './store/editor-state'
import {
  isJSXElement,
  isJSXAttributeFunctionCall,
  walkElements,
  JSXElementName,
} from '../../core/shared/element-template'
import { walkAttributes } from '../../core/shared/jsx-attributes'
import { pluck } from '../../core/shared/array-utils'
import { importAlias, Imports } from '../../core/shared/project-file-types'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'

// Adds an import for `UtopiaUtils` to the current open file.
export function addUtopiaUtilsImportIfUsed(editorState: EditorState): EditorState {
  let utopiaUtilsAlreadyImported: boolean = false
  const imports = getOpenImportsFromState(editorState)
  if ('utopia-api' in imports) {
    utopiaUtilsAlreadyImported = pluck(imports['utopia-api'].importedFromWithin, 'name').includes(
      'UtopiaUtils',
    )
  }
  if (utopiaUtilsAlreadyImported) {
    // If it's already imported not much point checking to see if it's used.
    return editorState
  } else {
    let utopiaUtilsUsed: boolean = false
    const openComponents = getOpenUtopiaJSXComponentsFromState(editorState)
    walkElements(openComponents, (element) => {
      if (isJSXElement(element)) {
        walkAttributes(element.props, (attr) => {
          if (isJSXAttributeFunctionCall(attr)) {
            utopiaUtilsUsed = true
          }
        })
      }
    })
    if (utopiaUtilsUsed) {
      // As it's not imported but something is using it,
      // then we need to add it to the imports.
      return modifyOpenParseSuccess((parseSuccess) => {
        const updatedImports = addImport(
          'utopia-api',
          null,
          [importAlias('UtopiaUtils')],
          null,
          parseSuccess.imports,
        )

        return {
          ...parseSuccess,
          imports: updatedImports,
        }
      }, editorState)
    } else {
      // Not imported already but also not used, so no need
      // to add it.
      return editorState
    }
  }
}

export function importedFromWhere(variableName: string, importsToSearch: Imports): string | null {
  for (const importSource of Object.keys(importsToSearch)) {
    const specificImport = importsToSearch[importSource]
    if (specificImport.importedAs === variableName) {
      return importSource
    }
    if (specificImport.importedWithName === variableName) {
      return importSource
    }
    if (specificImport.importedFromWithin.some((within) => within.alias === variableName)) {
      return importSource
    }
  }
  return null
}
