import { TemplatePath } from '../../../core/shared/project-file-types'
import { NavigatorReorder, RenameComponent } from '../../editor/action-types'
import { DropTargetType } from '../navigator'

export function reparentComponents(
  draggedComponents: Array<TemplatePath>,
  targetParent: TemplatePath,
): NavigatorReorder {
  return {
    action: 'NAVIGATOR_REORDER',
    dragSources: draggedComponents,
    dropTarget: {
      type: 'REPARENT_ROW',
      target: targetParent,
    },
  }
}

export function reparentComponentsToIndex(
  draggedComponents: Array<TemplatePath>,
  targetParent: TemplatePath,
  index: number,
): NavigatorReorder {
  return {
    action: 'NAVIGATOR_REORDER',
    dragSources: draggedComponents,
    dropTarget: {
      type: 'REPARENT_TO_INDEX',
      target: targetParent,
      index: index,
    },
  }
}

export function placeComponentsAfter(
  draggedComponents: Array<TemplatePath>,
  targetSibling: TemplatePath,
): NavigatorReorder {
  return {
    action: 'NAVIGATOR_REORDER',
    dragSources: draggedComponents,
    dropTarget: {
      type: 'MOVE_ROW_AFTER',
      target: targetSibling,
    },
  }
}

export function placeComponentsBefore(
  draggedComponents: Array<TemplatePath>,
  targetSibling: TemplatePath,
): NavigatorReorder {
  return {
    action: 'NAVIGATOR_REORDER',
    dragSources: draggedComponents,
    dropTarget: {
      type: 'MOVE_ROW_BEFORE',
      target: targetSibling,
    },
  }
}

export function renameComponent(target: TemplatePath, newName: string | null): RenameComponent {
  return {
    action: 'RENAME_COMPONENT',
    target: target,
    name: newName,
  }
}

export type LocalNavigatorAction = ShowNavigatorDropTargetHint

export interface ShowNavigatorDropTargetHint {
  action: 'DROP_TARGET_HINT'
  type: DropTargetType
  target: TemplatePath | null
}

export function showNavigatorDropTargetHint(
  type: DropTargetType,
  target: TemplatePath | null,
): ShowNavigatorDropTargetHint {
  return {
    action: 'DROP_TARGET_HINT',
    type: type,
    target: target,
  }
}
