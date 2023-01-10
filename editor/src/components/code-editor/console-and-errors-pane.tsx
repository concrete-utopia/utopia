import React from 'react'
import { useReadOnlyConsoleLogs } from '../../core/shared/runtime-report-logs'
import { setFocus } from '../common/actions'
import { openCodeEditorFile } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { getAllCodeEditorErrors } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { CodeEditorTabPane } from './code-problems'

export const ConsoleAndErrorsPane = React.memo(() => {
  const dispatch = useDispatch()

  const canvasConsoleLogs = useReadOnlyConsoleLogs()

  const errorMessages = useEditorState(
    'fullOldStore',
    (store) => {
      return getAllCodeEditorErrors(store.editor, 'warning', false)
    },
    'ConsoleAndErrorsPane errorMessages',
  )

  const onOpenFile = React.useCallback(
    (path: string) => {
      dispatch([openCodeEditorFile(path, true), setFocus('codeEditor')])
    },
    [dispatch],
  )

  return (
    <CodeEditorTabPane
      canvasConsoleLogs={canvasConsoleLogs}
      errorMessages={errorMessages}
      onOpenFile={onOpenFile}
    />
  )
})
