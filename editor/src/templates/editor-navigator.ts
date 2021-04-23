import * as update from 'immutability-helper'
import { TemplatePath } from '../core/shared/project-file-types'
import { DerivedState, EditorState } from '../components/editor/store/editor-state'
import { LocalNavigatorAction } from '../components/navigator/actions'
import { DragSelection } from '../components/navigator/navigator-item/navigator-item-dnd-container'
import * as TP from '../core/shared/template-path'
import Utils from '../utils/utils'

export function createDragSelections(
  templatePaths: TemplatePath[],
  selectedViews: TemplatePath[],
): Array<DragSelection> {
  let selections: Array<DragSelection> = []
  Utils.fastForEach(selectedViews, (selectedView) => {
    selections.push({
      templatePath: selectedView,
      index: templatePaths.findIndex((tp) => TP.pathsEqual(tp, selectedView)),
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
      return update(model, {
        navigator: {
          dropTargetHint: {
            $set: {
              target: action.target,
              type: action.type,
            },
          },
        },
      })

    default:
      return model
  }
}
