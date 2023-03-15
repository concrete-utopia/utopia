import {
  EditorState,
  modifyUnderlyingForOpenFile,
  withUnderlyingTargetFromEditorState,
} from '../../components/editor/store/editor-state'
import { JSXElementChild } from '../shared/element-template'
import { Optic, traversal } from '../shared/optics/optics'
import { ElementPath } from '../shared/project-file-types'

export function forElementOptic(target: ElementPath): Optic<EditorState, JSXElementChild> {
  function from(editor: EditorState): Array<JSXElementChild> {
    return withUnderlyingTargetFromEditorState(target, editor, [], (_, element) => {
      return [element]
    })
  }
  function update(
    editor: EditorState,
    modify: (child: JSXElementChild) => JSXElementChild,
  ): EditorState {
    return modifyUnderlyingForOpenFile(target, editor, modify)
  }
  return traversal(from, update)
}
