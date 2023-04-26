import { IndexPosition } from '../../../utils/utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorState, withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { reorderComponent } from '../canvas-utils'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface ReorderElement extends BaseCommand {
  type: 'REORDER_ELEMENT'
  target: ElementPath
  indexPosition: IndexPosition
}

export function reorderElement(
  whenToRun: WhenToRun,
  target: ElementPath,
  indexPosition: IndexPosition,
): ReorderElement {
  return {
    type: 'REORDER_ELEMENT',
    whenToRun: whenToRun,
    target: target,
    indexPosition: indexPosition,
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
      const withReorder = reorderComponent(components, command.target, command.indexPosition)
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
    commandDescription: `Reorder Element ${EP.toUid(command.target)} to new index ${JSON.stringify(
      command.indexPosition,
    )}`,
  }
}
