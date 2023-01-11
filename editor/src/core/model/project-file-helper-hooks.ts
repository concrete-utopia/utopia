import React from 'react'
import { EditorDispatch } from '../../components/editor/action-types'
import { updateFile } from '../../components/editor/actions/action-creators'
import { useDispatch } from '../../components/editor/store/dispatch-context'
import {
  EditorState,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
} from '../../components/editor/store/editor-state'
import { useRefEditorState } from '../../components/editor/store/store-hook'
import { RevisionsState, textFile, textFileContents } from '../shared/project-file-types'

export function useReParseOpenProjectFile(): () => void {
  const dispatch = useDispatch()
  const refEditorState = useRefEditorState((store) => store.editor)
  return React.useCallback(() => {
    reparseOpenProjectFile(refEditorState.current, dispatch)
  }, [refEditorState, dispatch])
}

export function reparseOpenProjectFile(editor: EditorState, dispatch: EditorDispatch): void {
  const openFile = getOpenUIJSFile(editor)
  const openFilePath = getOpenUIJSFileKey(editor)
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
}
