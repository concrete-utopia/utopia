import type { IndexPosition } from '../../../utils/utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { reorderComponent } from '../canvas-utils'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { getPatchForComponentChange } from './commands'

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
      const withReorder = reorderComponent(
        editorState.projectContents,
        components,
        command.target,
        command.indexPosition,
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
    commandDescription: `Reorder Element ${EP.toUid(command.target)} to new index ${JSON.stringify(
      command.indexPosition,
    )}`,
  }
}
