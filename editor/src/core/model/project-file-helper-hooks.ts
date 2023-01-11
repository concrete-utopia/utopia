import React from 'react'
import { getContentsTreeFileFromString } from '../../components/assets'
import { EditorDispatch } from '../../components/editor/action-types'
import { updateFile } from '../../components/editor/actions/action-creators'
import { useDispatch } from '../../components/editor/store/dispatch-context'
import {
  EditorState,
  getFileForName,
  getOpenFilename,
} from '../../components/editor/store/editor-state'
import { useRefEditorState } from '../../components/editor/store/store-hook'
import {
  isTextFile,
  RevisionsState,
  textFile,
  textFileContents,
} from '../shared/project-file-types'

export function useReParseOpenProjectFile(): () => void {
  const dispatch = useDispatch()
  const refEditorState = useRefEditorState((store) => store.editor)
  return React.useCallback(() => {
    reparseProjectFile(getOpenFilename(refEditorState.current), refEditorState.current, dispatch)
  }, [refEditorState, dispatch])
}

export function reparseProjectFile(
  filePath: string | null,
  editor: EditorState,
  dispatch: EditorDispatch,
): void {
  if (filePath == null) {
    return
  }
  const file = getFileForName(filePath, editor)
  if (!isTextFile(file)) {
    return
  }
  const contentsFile = getContentsTreeFileFromString(editor.projectContents, filePath)
  if (!isTextFile(contentsFile)) {
    return
  }
  const openFileCodeAhead = textFile(
    textFileContents(file.fileContents.code, file.fileContents.parsed, RevisionsState.CodeAhead),
    file.lastSavedContents,
    file.lastParseSuccess,
    Date.now(),
  )
  dispatch([updateFile(filePath, openFileCodeAhead, false)])
}
