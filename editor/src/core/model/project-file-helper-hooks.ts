import React from 'react'
import { updateFile } from '../../components/editor/actions/action-creators'
import { useDispatch } from '../../components/editor/store/dispatch-context'
import { getOpenUIJSFile, getOpenUIJSFileKey } from '../../components/editor/store/editor-state'
import { useRefEditorState } from '../../components/editor/store/store-hook'
import { RevisionsState, textFile, textFileContents } from '../shared/project-file-types'
import type { SteganoTextData } from '../shared/stegano-text'
import type { EditorAction } from '../../components/editor/action-types'
import { applyPrettier } from 'utopia-vscode-common'
import { getTextFileByPath } from '../../components/assets'

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

function spliceCode(
  originalCode: string,
  originalStringData: SteganoTextData,
  updatedString: string,
): string {
  const originalString = originalCode.slice(
    originalStringData.startPosition,
    originalStringData.endPosition,
  )
  if (originalString !== originalStringData.originalString) {
    throw new Error(`Tried to rewrite string but it was not matching the last known value.
  Current value:    >>>${originalString}<<<
  Last known value: >>>${originalStringData.originalString}<<<
  `)
  }

  const originalBefore = originalCode.slice(0, originalStringData.startPosition)
  const originalAfter = originalCode.slice(originalStringData.endPosition)

  return applyPrettier('' + originalBefore + updatedString + originalAfter, false).formatted
}

export function useUpdateStringRun(): (
  originalStringData: SteganoTextData,
  updatedString: string,
) => EditorAction[] {
  const refEditorState = useRefEditorState((store) => store.editor)
  return React.useCallback(
    (originalStringData: SteganoTextData, updatedString: string): EditorAction[] => {
      const editor = refEditorState.current
      const sourceFile = getTextFileByPath(editor.projectContents, originalStringData.filePath)
      const updatedCode = spliceCode(
        sourceFile.fileContents.code,
        originalStringData,
        sanitizeString(updatedString),
      )
      const updatedFileCodeAhead = textFile(
        textFileContents(
          updatedCode,
          sourceFile.fileContents.parsed,
          RevisionsState.CodeAheadButPleaseTellVSCodeAboutIt,
        ),
        sourceFile.lastSavedContents,
        sourceFile.lastParseSuccess,
        sourceFile.versionNumber + 1,
      )
      return [updateFile(originalStringData.filePath, updatedFileCodeAhead, false)]
    },
    [refEditorState],
  )
}

function sanitizeString(s: string): string {
  return JSON.stringify(s)
}
