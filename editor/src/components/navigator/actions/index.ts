import { ElementPath } from '../../../core/shared/project-file-types'
import { NavigatorReorder, RenameComponent } from '../../editor/action-types'
import { DropTargetType } from '../navigator'

export function reparentComponents(
  draggedComponents: Array<ElementPath>,
  targetParent: ElementPath,
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
  draggedComponents: Array<ElementPath>,
  targetParent: ElementPath,
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
  draggedComponents: Array<ElementPath>,
  targetSibling: ElementPath,
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
  draggedComponents: Array<ElementPath>,
  targetSibling: ElementPath,
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

export function renameComponent(target: ElementPath, newName: string | null): RenameComponent {
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
  target: ElementPath | null
}

export function showNavigatorDropTargetHint(
  type: DropTargetType,
  target: ElementPath | null,
): ShowNavigatorDropTargetHint {
  return {
    action: 'DROP_TARGET_HINT',
    type: type,
    target: target,
  }
}
