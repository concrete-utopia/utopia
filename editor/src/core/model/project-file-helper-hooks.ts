import React from 'react'
import { updateFile } from '../../components/editor/actions/action-creators'
import { useDispatch } from '../../components/editor/store/dispatch-context'
import { getOpenUIJSFile, getOpenUIJSFileKey } from '../../components/editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../components/editor/store/store-hook'
import { RevisionsState, textFile, textFileContents } from '../shared/project-file-types'

export function useReParseOpenProjectFile(): () => void {
  const dispatch = useDispatch()
  const refEditorState = useRefEditorState((store) => store.editor)
  return React.useCallback(() => {
    const openFile = getOpenUIJSFile(refEditorState.current)
    const openFilePath = getOpenUIJSFileKey(refEditorState.current)
    if (openFile != null && openFilePath != null) {
      const openFileCodeAhead = textFile(
        textFileContents(
          openFile.fileContents.code,
          openFile.fileContents.parsed,
          RevisionsState.CodeAhead,
        ),
        openFile.lastSavedContents,
        openFile.lastParseSuccess,
        openFile.versionNumber + 1,
      )
      dispatch([updateFile(openFilePath, openFileCodeAhead, false)])
    }
  }, [refEditorState, dispatch])
}
