import { IndexPosition } from '../../../utils/utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath, StaticElementPath } from '../../../core/shared/project-file-types'
import { EditorState, withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { reorderComponent } from '../canvas-utils'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'
import { rearrangeJsxChildren } from '../../../core/model/element-template-utils'

export interface RearrangeChildren extends BaseCommand {
  type: 'REARRANGE_CHILDREN'
  target: ElementPath
  rearrangedChildrenOrder: Array<StaticElementPath>
}

export function rearrangeChildren(
  whenToRun: WhenToRun,
  target: ElementPath,
  rearrangedChildrenOrder: Array<StaticElementPath>,
): RearrangeChildren {
  return {
    type: 'REARRANGE_CHILDREN',
    whenToRun: whenToRun,
    target: target,
    rearrangedChildrenOrder: rearrangedChildrenOrder,
  }
}

export const runRearrangeChildren: CommandFunction<RearrangeChildren> = (
  editorState: EditorState,
  command: RearrangeChildren,
) => {
  const patch = withUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    {},
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const withRearrange = rearrangeJsxChildren(
        EP.dynamicPathToStaticPath(command.target),
        command.rearrangedChildrenOrder,
        components,
      )
      return getPatchForComponentChange(
        success.topLevelElements,
        withRearrange,
        success.imports,
        underlyingFilePath,
      )
    },
  )
  return {
    editorStatePatches: [patch],
    commandDescription: `Rearranged Children fo ${EP.toUid(command.target)}`,
  }
}
