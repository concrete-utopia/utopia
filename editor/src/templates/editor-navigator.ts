import type { ElementPath } from '../core/shared/project-file-types'
import type {
  DerivedState,
  EditorState,
  NavigatorEntry,
} from '../components/editor/store/editor-state'
import { regularNavigatorEntry } from '../components/editor/store/editor-state'
import type { LocalNavigatorAction } from '../components/navigator/actions'
import { NavigatorStateKeepDeepEquality } from '../components/editor/store/store-deep-equality-instances'

// Currently only "real" elements can be selected, we produce the selected entries
// directly from `selectedViews`.
export function getSelectedNavigatorEntries(
  selectedViews: Array<ElementPath>,
): Array<NavigatorEntry> {
  return selectedViews.map(regularNavigatorEntry)
}

export const runLocalNavigatorAction = function (
  model: EditorState,
  derivedState: DerivedState,
  action: LocalNavigatorAction,
): EditorState {
  switch (action.action) {
    case 'SHOW_DROP_TARGET_HINT':
      return {
        ...model,
        navigator: NavigatorStateKeepDeepEquality(model.navigator, {
          ...model.navigator,
          dropTargetHint: {
            type: action.type,
            displayAtEntry: action.displayAtEntry,
            targetParent: action.targetParentEntry,
            targetIndexPosition: action.indexInTargetParent,
          },
        }).value,
      }
    case 'HIDE_DROP_TARGET_HINT':
      return {
        ...model,
        navigator: NavigatorStateKeepDeepEquality(model.navigator, {
          ...model.navigator,
          dropTargetHint: null,
        }).value,
      }
    default:
      return model
  }
}
