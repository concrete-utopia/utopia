import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import { isInsertMode } from '../../../editor/editor-modes'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { useHighlightCallbacks } from './select-mode-hooks'

function useGetHighlightableViewsForInsertMode() {
  const storeRef = useRefEditorState((store) => {
    const resolveFn = store.editor.codeResultCache.curriedResolveFn(store.editor.projectContents)
    return {
      componentMetadata: store.editor.jsxMetadata,
      mode: store.editor.mode,
      projectContents: store.editor.projectContents,
      nodeModules: store.editor.nodeModules.files,
      transientState: store.derived.transientState,
      resolve: resolveFn,
    }
  })
  return React.useCallback(() => {
    const { componentMetadata, mode, projectContents } = storeRef.current
    if (isInsertMode(mode)) {
      const allPaths = MetadataUtils.getAllPaths(componentMetadata)
      const insertTargets = allPaths.filter((path) => {
        return MetadataUtils.targetSupportsChildren(projectContents, componentMetadata, path)
      })
      return insertTargets
    } else {
      return []
    }
  }, [storeRef])
}

export function useInsertModeSelectAndHover(
  active: boolean,
  cmdPressed: boolean,
): {
  onMouseMove: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseDown: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onMouseUp: (event: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
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
    onMouseUp: NO_OP,
  })
}
