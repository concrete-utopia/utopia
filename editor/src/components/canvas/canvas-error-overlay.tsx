import React from 'react'
import { useErrorOverlayRecords } from '../../core/shared/runtime-report-logs'
import { ReactErrorOverlay } from '../../third-party/react-error-overlay/react-error-overlay'
import { setFocus } from '../common/actions'
import {
  clearHighlightedViews,
  openCodeEditorFile,
  switchEditorMode,
} from '../editor/actions/action-creators'
import { EditorModes } from '../editor/editor-modes'
import { useDispatch } from '../editor/store/dispatch-context'
import { useRefEditorState } from '../editor/store/store-hook'
import CanvasActions from './canvas-actions'
import { shouldShowErrorOverlay } from './canvas-utils'

export const ErrorOverlayComponent = React.memo(() => {
  const { errorRecords, overlayErrors } = useErrorOverlayRecords()
  const overlayWillShow = shouldShowErrorOverlay(errorRecords, overlayErrors)

  const dispatch = useDispatch()

  const onOpenFile = React.useCallback(
    (path: string) => {
      dispatch([openCodeEditorFile(path, true), setFocus('codeEditor')])
    },
    [dispatch],
  )

  const postActionSessionInProgressRef = useRefEditorState(
    (store) => store.postActionInteractionSession != null,
  )

  React.useEffect(() => {
    if (overlayWillShow) {
      if (postActionSessionInProgressRef.current) {
        return
      }

      // If this is showing, we need to clear any canvas drag state and apply the changes it would have resulted in,
      // since that might have been the cause of the error being thrown, as well as switching back to select mode
      setTimeout(() => {
        // wrapping in a setTimeout so we don't dispatch from inside React lifecycle

        dispatch([
          CanvasActions.clearInteractionSession(true),
          switchEditorMode(EditorModes.selectMode(null, false, 'none')),
          clearHighlightedViews(),
        ])
      }, 0)
    }
  }, [dispatch, overlayWillShow, postActionSessionInProgressRef])

  return (
    <ReactErrorOverlay
      currentBuildErrorRecords={errorRecords}
      currentRuntimeErrorRecords={overlayErrors}
      onOpenFile={onOpenFile}
      overlayOffset={0}
    />
  )
})
ErrorOverlayComponent.displayName = 'ErrorOverlayComponent'
