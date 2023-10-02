import type { EditorState, EditorStorePatched } from '../../components/editor/store/editor-state'
import {
  modifyUnderlyingForOpenFile,
  withUnderlyingTargetFromEditorState,
} from '../../components/editor/store/editor-state'
import type { JSXElementChild } from '../shared/element-template'
import { fromField } from '../shared/optics/optic-creators'
import type { Optic } from '../shared/optics/optics'
import { traversal } from '../shared/optics/optics'
import type { ElementPath } from '../shared/project-file-types'

export function editorStateToElementChildOptic(
  target: ElementPath,
): Optic<EditorState, JSXElementChild> {
  function from(editorState: EditorState): Array<JSXElementChild> {
    return withUnderlyingTargetFromEditorState(target, editorState, [], (_, element) => {
      return [element]
    })
  }
  function update(
    editorState: EditorState,
    modify: (child: JSXElementChild) => JSXElementChild,
  ): EditorState {
    return modifyUnderlyingForOpenFile(target, editorState, modify)
  }
  return traversal(from, update)
}

export function forElementChildOptic(
  target: ElementPath,
): Optic<EditorStorePatched, JSXElementChild> {
  return fromField<EditorStorePatched, 'editor'>('editor').compose(
    editorStateToElementChildOptic(target),
  )
}
