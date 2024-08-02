import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { point, windowPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import type { EditorDispatch } from '../../../editor/action-types'
import { selectComponents, switchEditorMode } from '../../../editor/actions/action-creators'
import type { Coordinates } from '../../../editor/editor-modes'
import { EditorModes, isTextEditMode } from '../../../editor/editor-modes'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../editor/store/store-hook'
import type { MouseCallbacks } from '../select-mode/select-mode-hooks'
import { useFindValidTarget, useHighlightCallbacks } from '../select-mode/select-mode-hooks'

function useGetTextEditableViews() {
  const storeRef = useRefEditorState((store) => {
    return {
      componentMetadata: store.editor.jsxMetadata,
      elementPathTree: store.editor.elementPathTree,
      mode: store.editor.mode,
    }
  })
  return React.useCallback(() => {
    const { componentMetadata, elementPathTree, mode } = storeRef.current
    if (isTextEditMode(mode)) {
      const allPaths = MetadataUtils.getAllPaths(componentMetadata, elementPathTree)
      const textEditTargets = allPaths.filter((path) =>
        MetadataUtils.targetTextEditable(componentMetadata, elementPathTree, path),
      )
      return textEditTargets
    } else {
      return []
    }
  }, [storeRef])
}

export function useTextEditModeSelectAndHover(active: boolean): MouseCallbacks {
  const getTextEditableViews = useGetTextEditableViews()

  const { onMouseMove } = useHighlightCallbacks(active, true, getTextEditableViews)

  const dispatch = useDispatch()
  const findValidTarget = useFindValidTarget()

  const onMouseUp = React.useCallback(
    (event: React.MouseEvent) => {
      const textEditableViews = getTextEditableViews()

      const foundTarget = findValidTarget(
        textEditableViews,
        windowPoint(point(event.clientX, event.clientY)),
        'prefer-more-specific-selection',
      )
      if (foundTarget == null) {
        return
      }
      activateTextEditing(foundTarget.elementPath, null, dispatch)
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
  cursorPosition: Coordinates | null,
  dispatch: EditorDispatch,
): void {
  setTimeout(() => activateTextEditing(elementPath, cursorPosition, dispatch))
}

function activateTextEditing(
  elementPath: ElementPath,
  cursorPosition: Coordinates | null,
  dispatch: EditorDispatch,
): void {
  dispatch([
    selectComponents([elementPath], false),
    switchEditorMode(
      EditorModes.textEditMode(elementPath, cursorPosition, 'existing', 'no-text-selection'),
    ),
  ])
}
