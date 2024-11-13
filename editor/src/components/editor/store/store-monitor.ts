import type { ElementPath } from 'utopia-shared/src/types'
import { Substores, useEditorState } from './store-hook'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { withUnderlyingTarget } from './editor-state'

export function useMonitorChangesToElements(elementPaths: Array<ElementPath>): void {
  useEditorState(
    Substores.projectContents,
    (store) => {
      return mapDropNulls((elementPath) => {
        return withUnderlyingTarget(
          elementPath,
          store.editor.projectContents,
          null,
          (_, element) => {
            return element
          },
        )
      }, elementPaths)
    },
    'useMonitorChangesToElements',
  )
}

export function useMonitorChangesToEditor(): void {
  useEditorState(
    Substores.fullStore,
    (store) => {
      return { editor: store.editor, derived: store.derived }
    },
    'useMonitorChangesToEditor',
  )
}
