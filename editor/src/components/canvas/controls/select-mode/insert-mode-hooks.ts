import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as TP from '../../../../core/shared/template-path'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import {
  insertionSubjectIsDragAndDrop,
  insertionSubjectIsJSXElement,
  isInsertMode,
} from '../../../editor/editor-modes'
import { getOpenImportsFromState } from '../../../editor/store/editor-state'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { useHighlightCallbacks } from './select-mode-hooks'

function useGetHighlightableViewsForInsertMode() {
  const storeRef = useRefEditorState((store) => {
    return {
      componentMetadata: store.editor.jsxMetadata,
      mode: store.editor.mode,
      imports: getOpenImportsFromState(store.editor),
    }
  })
  return React.useCallback(() => {
    const { componentMetadata, mode, imports } = storeRef.current
    if (!isInsertMode(mode)) {
      throw new Error('insert highlight callback was called oustide of insert mode')
    }
    const allPaths = MetadataUtils.getAllPaths(componentMetadata)
    const insertTargets = allPaths.filter((path) => {
      return (
        (insertionSubjectIsJSXElement(mode.subject) ||
          insertionSubjectIsDragAndDrop(mode.subject)) &&
        MetadataUtils.targetSupportsChildren(imports, componentMetadata, path)
      )
    })
    return insertTargets
  }, [storeRef])
}

export function useInsertModeSelectAndHover(
  active: boolean,
  cmdPressed: boolean,
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
} {
  const getHiglightableViewsForInsertMode = useGetHighlightableViewsForInsertMode()
  const { onMouseMove } = useHighlightCallbacks(
    active,
    cmdPressed,
    true,
    getHiglightableViewsForInsertMode,
  )

  return useKeepShallowReferenceEquality({
    onMouseMove: onMouseMove,
    onMouseDown: NO_OP,
  })
}
