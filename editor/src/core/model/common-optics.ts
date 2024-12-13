import type { ParseSuccess } from 'utopia-shared/src/types'
import type { EditorState, EditorStorePatched } from '../../components/editor/store/editor-state'
import {
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
} from '../../components/editor/store/editor-state'
import type { JSXElementChild } from '../shared/element-template'
import { fromField } from '../shared/optics/optic-creators'
import { makeOptic, type Optic } from '../shared/optics/optics'
import type { ElementPath } from '../shared/project-file-types'

export function editorStateToElementChildOptic(
  target: ElementPath,
): Optic<EditorState, JSXElementChild> {
  function from(editorState: EditorState, callback: (child: JSXElementChild) => void): void {
    forUnderlyingTargetFromEditorState(
      target,
      editorState,
      (_: ParseSuccess, element: JSXElementChild) => {
        callback(element)
      },
    )
  }
  function update(
    editorState: EditorState,
    modify: (child: JSXElementChild) => JSXElementChild,
  ): EditorState {
    return modifyUnderlyingForOpenFile(target, editorState, modify)
  }
  return makeOptic(from, update)
}

export function forElementChildOptic(
  target: ElementPath,
): Optic<EditorStorePatched, JSXElementChild> {
  return fromField<EditorStorePatched, 'editor'>('editor').compose(
    editorStateToElementChildOptic(target),
  )
}
