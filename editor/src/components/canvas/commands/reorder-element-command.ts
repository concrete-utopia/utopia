import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorState, withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { reorderComponent } from '../canvas-utils'
import {
  BaseCommand,
  CommandFunction,
  getPatchForComponentChange,
  TransientOrNot,
} from './commands'

export interface ReorderElement extends BaseCommand {
  type: 'REORDER_ELEMENT'
  target: ElementPath
  newIndex: number
}

export function reorderElement(
  transient: TransientOrNot,
  target: ElementPath,
  newIndex: number,
): ReorderElement {
  return {
    type: 'REORDER_ELEMENT',
    transient: transient,
    target: target,
    newIndex: newIndex,
  }
}

export const runReorderElement: CommandFunction<ReorderElement> = (
  editorState: EditorState,
  command: ReorderElement,
) => {
  const patch = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    {},
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const withReorder = reorderComponent(
        editorState.projectContents,
        editorState.canvas.openFile?.filename ?? null,
        components,
        command.target,
        command.newIndex,
      )
      return getPatchForComponentChange(
        success.topLevelElements,
        withReorder,
        success.imports,
        underlyingFilePath,
      )
    },
  )
  return {
    editorStatePatches: [patch],
    commandDescription: `Reorder Element ${EP.toUid(command.target)} to new index ${
      command.newIndex
    }`,
  }
}
