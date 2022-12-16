import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { point, windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import { EditorDispatch } from '../../../editor/action-types'
import { switchEditorMode } from '../../../editor/actions/action-creators'
import { EditorModes, isTextEditMode } from '../../../editor/editor-modes'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import {
  MouseCallbacks,
  useFindValidTarget,
  useHighlightCallbacks,
} from '../select-mode/select-mode-hooks'

function useGetTextEditableViews() {
  const storeRef = useRefEditorState((store) => {
    return {
      componentMetadata: store.editor.jsxMetadata,
      mode: store.editor.mode,
    }
  })
  return React.useCallback(() => {
    const { componentMetadata, mode } = storeRef.current
    if (isTextEditMode(mode)) {
      const allPaths = MetadataUtils.getAllPaths(componentMetadata)
      const textEditTargets = allPaths.filter((path) =>
        MetadataUtils.targetTextEditable(componentMetadata, path),
      )
      return textEditTargets
    } else {
      return []
    }
  }, [storeRef])
}

export function useTextEditModeSelectAndHover(active: boolean): MouseCallbacks {
  const getTextEditableViews = useGetTextEditableViews()

  const { onMouseMove } = useHighlightCallbacks(active, true, true, getTextEditableViews)

  const dispatch = useEditorState(
    (store) => store.dispatch,
    'useTextEditModeSelectAndHover dispatch',
  )
  const findValidTarget = useFindValidTarget()

  const onMouseUp = React.useCallback(
    (event: React.MouseEvent) => {
      const textEditableViews = getTextEditableViews()

      const foundTarget = findValidTarget(
        textEditableViews,
        windowPoint(point(event.clientX, event.clientY)),
        'dont-prefer-selected',
      )
      if (foundTarget == null) {
        return
      }
      dispatch([switchEditorMode(EditorModes.textEditMode(foundTarget.elementPath))])
    },
    [findValidTarget, getTextEditableViews, dispatch],
  )

  return useKeepShallowReferenceEquality({
    onMouseMove: onMouseMove,
    onMouseDown: NO_OP,
    onMouseUp: onMouseUp,
  })
}

export function scheduleTextEditForNextFrame(
  elementPath: ElementPath,
  dispatch: EditorDispatch,
): void {
  setTimeout(() => dispatch([switchEditorMode(EditorModes.textEditMode(elementPath))]))
}
