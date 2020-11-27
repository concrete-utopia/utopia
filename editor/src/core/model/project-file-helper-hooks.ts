import * as React from 'react'
import { updateFile } from '../../components/editor/actions/action-creators'
import { getOpenUIJSFile, getOpenUIJSFileKey } from '../../components/editor/store/editor-state'
import { useEditorState, useRefEditorState } from '../../components/editor/store/store-hook'
import { RevisionsState, textFile, textFileContents } from '../shared/project-file-types'

export function useReParseOpenProjectFile(): () => void {
  const dispatch = useEditorState((s) => s.dispatch, 'useReParseOpenProjectFile dispatch')
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
        Date.now(),
      )
      dispatch([updateFile(openFilePath, openFileCodeAhead, false)])
    }
  }, [refEditorState, dispatch])
}
