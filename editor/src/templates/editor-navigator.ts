import { ElementPath } from '../core/shared/project-file-types'
import {
  DerivedState,
  EditorState,
  isRegularNavigatorEntry,
  NavigatorEntry,
  regularNavigatorEntry,
} from '../components/editor/store/editor-state'
import { LocalNavigatorAction } from '../components/navigator/actions'
import { DragSelection } from '../components/navigator/navigator-item/navigator-item-dnd-container'
import * as EP from '../core/shared/element-path'
import Utils from '../utils/utils'
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
    case 'DROP_TARGET_HINT':
      return {
        ...model,
        navigator: NavigatorStateKeepDeepEquality(model.navigator, {
          ...model.navigator,
          dropTargetHint: {
            displayAtEntry: action.displayAtElementPath,
            moveToEntry: action.moveToElementPath,
            type: action.type,
          },
        }).value,
      }
    default:
      return model
  }
}
