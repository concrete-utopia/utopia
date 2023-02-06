import { IndexPosition } from '../../../utils/utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorState, withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { reorderComponent } from '../canvas-utils'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface ReorderChildren extends BaseCommand {
  type: 'REORDER_CHILDREN'
  target: ElementPath
  rearrangedChildrenOrder: Array<ElementPath>
}

export function reorderChildren(
  whenToRun: WhenToRun,
  target: ElementPath,
  rearrangedChildrenOrder: Array<ElementPath>,
): ReorderChildren {
  return {
    type: 'REORDER_CHILDREN',
    whenToRun: whenToRun,
    target: target,
    rearrangedChildrenOrder: rearrangedChildrenOrder,
  }
}

export const runReorderChildren: CommandFunction<ReorderChildren> = (
  editorState: EditorState,
  command: ReorderChildren,
) => {
  const patch = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    {},
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      return {}
    },
  )
  return {
    editorStatePatches: [patch],
    commandDescription: `Rearranged Children fo ${EP.toUid(command.target)}`,
  }
}
