import * as React from 'react'
import { useReadOnlyConsoleLogs } from '../../core/shared/runtime-report-logs'
import { betterReactMemo } from '../../uuiui-deps'
import { setFocus } from '../common/actions'
import { openCodeEditorFile } from '../editor/actions/action-creators'
import { getAllCodeEditorErrors } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { CodeEditorTabPane } from './code-problems'

export const ConsoleAndErrorsPane = betterReactMemo('ConsoleAndErrorsPane', () => {
  const dispatch = useEditorState((store) => store.dispatch, 'ConsoleAndErrorsPane dispatch')

  const canvasConsoleLogs = useReadOnlyConsoleLogs()

  const errorMessages = useEditorState((store) => {
    return getAllCodeEditorErrors(store.editor, 'warning', false)
  }, 'ConsoleAndErrorsPane errorMessages')

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
