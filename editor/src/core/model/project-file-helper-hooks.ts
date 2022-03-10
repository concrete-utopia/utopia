import React from 'react'
import { updateFile } from '../../components/editor/actions/action-creators'
import {
  EditorStorePatched,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
} from '../../components/editor/store/editor-state'
import { useEditorDispatch, useRefEditorState } from '../../components/editor/store/store-hook'
import { RevisionsState, textFile, textFileContents } from '../shared/project-file-types'

const editorSelector = (store: EditorStorePatched) => store.editor

export function useReParseOpenProjectFile(): () => void {
  const dispatch = useEditorDispatch('useReParseOpenProjectFile dispatch')
  const refEditorState = useRefEditorState(editorSelector)
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
        Date.now(),
      )
      dispatch([updateFile(openFilePath, openFileCodeAhead, false)])
    }
  }, [refEditorState, dispatch])
}
