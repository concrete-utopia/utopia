import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isLeft } from '../../../core/shared/either'
import { JSXElementChild } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'

export function getElementFromRenderResult(
  renderResult: EditorRenderResult,
  path: ElementPath,
): JSXElementChild {
  const element = MetadataUtils.findElementByElementPath(
    renderResult.getEditorState().editor.jsxMetadata,
    path,
  )
  if (element == null || isLeft(element.element)) {
    throw new Error('element is invalid')
  }
  return element.element.value
}
