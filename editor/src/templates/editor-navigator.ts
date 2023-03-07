import { ElementPath } from '../core/shared/project-file-types'
import {
  DerivedState,
  EditorState,
  isRegularNavigatorEntry,
  NavigatorEntry,
} from '../components/editor/store/editor-state'
import { LocalNavigatorAction } from '../components/navigator/actions'
import { DragSelection } from '../components/navigator/navigator-item/navigator-item-dnd-container'
import * as EP from '../core/shared/element-path'
import Utils from '../utils/utils'
import { NavigatorStateKeepDeepEquality } from '../components/editor/store/store-deep-equality-instances'

export function createDragSelections(
  navigatorEntries: Array<NavigatorEntry>,
  selectedViews: ElementPath[],
): Array<DragSelection> {
  let selections: Array<DragSelection> = []
  Utils.fastForEach(selectedViews, (selectedView) => {
    selections.push({
      elementPath: selectedView,
      index: navigatorEntries.findIndex(
        (entry) => isRegularNavigatorEntry(entry) && EP.pathsEqual(entry.elementPath, selectedView),
      ),
    })
  })
  selections.sort((a, b) => b.index - a.index)
  return selections
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
            displayAtElementPath: action.displayAtElementPath,
            moveToElementPath: action.moveToElementPath,
            type: action.type,
          },
        }).value,
      }
    default:
      return model
  }
}
