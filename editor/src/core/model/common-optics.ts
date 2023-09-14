import type { EditorStorePatched } from '../../components/editor/store/editor-state'
import {
  modifyUnderlyingForOpenFile,
  withUnderlyingTargetFromEditorState,
} from '../../components/editor/store/editor-state'
import type { JSXElementChild } from '../shared/element-template'
import type { Optic } from '../shared/optics/optics'
import { traversal } from '../shared/optics/optics'
import type { ElementPath } from '../shared/project-file-types'

export function forElementOptic(target: ElementPath): Optic<EditorStorePatched, JSXElementChild> {
  function from(store: EditorStorePatched): Array<JSXElementChild> {
    return withUnderlyingTargetFromEditorState(target, store.editor, [], (_, element) => {
      return [element]
    })
  }
  function update(
    store: EditorStorePatched,
    modify: (child: JSXElementChild) => JSXElementChild,
  ): EditorStorePatched {
    return {
      ...store,
      editor: modifyUnderlyingForOpenFile(target, store.editor, modify),
    }
  }
  return traversal(from, update)
}
