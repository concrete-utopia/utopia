import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { point, windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { useKeepShallowReferenceEquality } from '../../../../utils/react-performance'
import { EditorDispatch } from '../../../editor/action-types'
import { switchEditorMode } from '../../../editor/actions/action-creators'
import { Coordinates, EditorModes, isTextEditMode } from '../../../editor/editor-modes'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../editor/store/store-hook'
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

  const dispatch = useDispatch()
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
      dispatch([
        switchEditorMode(
          EditorModes.textEditMode(foundTarget.elementPath, null, 'existing', 'no-text-selection'),
        ),
      ])
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
  setTimeout(() =>
    dispatch([
      switchEditorMode(
        EditorModes.textEditMode(elementPath, cursorPosition, 'existing', 'no-text-selection'),
      ),
    ]),
  )
}
