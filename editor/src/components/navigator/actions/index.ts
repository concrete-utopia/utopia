import type { ElementPath } from '../../../core/shared/project-file-types'
import type { IndexPosition } from '../../../utils/utils'
import type { RenameComponent } from '../../editor/action-types'
import type { DropTargetType, NavigatorEntry } from '../../editor/store/editor-state'

export function renameComponent(target: ElementPath, newName: string | null): RenameComponent {
  return {
    action: 'RENAME_COMPONENT',
    target: target,
    name: newName,
  }
}

export type LocalNavigatorAction = ShowNavigatorDropTargetHint | HideNavigatorDropTargetHint

export interface ShowNavigatorDropTargetHint {
  action: 'SHOW_DROP_TARGET_HINT'
  type: DropTargetType
  targetParentEntry: NavigatorEntry
  displayAtEntry: NavigatorEntry
  indexInTargetParent: IndexPosition
}

export function showNavigatorDropTargetHint(
  type: DropTargetType,
  moveToElementPath: NavigatorEntry,
  displayAtElementPath: NavigatorEntry,
  indexPosition: IndexPosition,
): ShowNavigatorDropTargetHint {
  return {
    action: 'SHOW_DROP_TARGET_HINT',
    type: type,
    targetParentEntry: moveToElementPath,
    displayAtEntry: displayAtElementPath,
    indexInTargetParent: indexPosition,
  }
}

export interface HideNavigatorDropTargetHint {
  action: 'HIDE_DROP_TARGET_HINT'
}

export function hideNavigatorDropTargetHint(): HideNavigatorDropTargetHint {
  return {
    action: 'HIDE_DROP_TARGET_HINT',
  }
}
