import type { EditorState } from '../../components/editor/store/editor-state'
import {
  modifyUnderlyingForOpenFile,
  withUnderlyingTargetFromEditorState,
} from '../../components/editor/store/editor-state'
import type { JSXElementChild } from '../shared/element-template'
import type { Optic } from '../shared/optics/optics'
import { traversal } from '../shared/optics/optics'
import type { ElementPath } from '../shared/project-file-types'

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
