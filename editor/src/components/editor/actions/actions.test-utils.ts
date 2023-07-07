import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isLeft } from '../../../core/shared/either'
import type { JSXElementChild } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import * as EP from '../../../core/shared/element-path'

export function getElementFromRenderResult(
  renderResult: EditorRenderResult,
  path: ElementPath,
): JSXElementChild {
  const element = MetadataUtils.findElementByElementPath(
    renderResult.getEditorState().editor.jsxMetadata,
    path,
  )
  if (element == null) {
    throw new Error(`Could not find element ${EP.toString(path)}`)
  } else if (isLeft(element.element)) {
    throw new Error(`Element ${element.element} for ${EP.toString(path)} is invalid.`)
  }
  return element.element.value
}
