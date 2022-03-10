import React from 'react'
import { useReadOnlyConsoleLogs } from '../../core/shared/runtime-report-logs'
import { setFocus } from '../common/actions'
import { openCodeEditorFile } from '../editor/actions/action-creators'
import { EditorStorePatched, getAllCodeEditorErrors } from '../editor/store/editor-state'
import { useEditorDispatch, useEditorState } from '../editor/store/store-hook'
import { CodeEditorTabPane } from './code-problems'

const errorMessagesSelector = (store: EditorStorePatched) =>
  getAllCodeEditorErrors(store.editor, 'warning', false)

export const ConsoleAndErrorsPane = React.memo(() => {
  const dispatch = useEditorDispatch('ConsoleAndErrorsPane dispatch')

  const canvasConsoleLogs = useReadOnlyConsoleLogs()

  const errorMessages = useEditorState(errorMessagesSelector, 'ConsoleAndErrorsPane errorMessages')

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
