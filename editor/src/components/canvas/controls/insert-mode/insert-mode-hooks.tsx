import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import { isInsertMode } from '../../../editor/editor-modes'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { MouseCallbacks, useHighlightCallbacks } from '../select-mode/select-mode-hooks'

function useGetHighlightableViewsForInsertMode() {
  const storeRef = useRefEditorState((store) => {
    const resolveFn = store.editor.codeResultCache.curriedResolveFn(store.editor.projectContents)
    return {
      componentMetadata: store.editor.jsxMetadata,
      elementPathTree: store.editor.elementPathTree,
      mode: store.editor.mode,
      openFile: store.editor.canvas.openFile?.filename ?? null,
      projectContents: store.editor.projectContents,
      nodeModules: store.editor.nodeModules.files,
      transientState: store.derived.transientState,
      resolve: resolveFn,
    }
  })
  return React.useCallback(() => {
    const { componentMetadata, elementPathTree, mode, projectContents, nodeModules, openFile } =
      storeRef.current
    if (isInsertMode(mode)) {
      const allPaths = MetadataUtils.getAllPaths(componentMetadata, elementPathTree)
      const insertTargets = allPaths.filter((path) => {
        return MetadataUtils.targetSupportsChildren(
          projectContents,
          componentMetadata,
          nodeModules,
          openFile,
          path,
        )
      })
      return insertTargets
    } else {
      return []
    }
  }, [storeRef])
}

export function useInsertModeSelectAndHover(active: boolean, cmdPressed: boolean): MouseCallbacks {
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
