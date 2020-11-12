import * as React from 'react'
import { getDependencyTypeDefinitions } from '../../core/es-modules/package-manager/package-manager'
import { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { isParseSuccess } from '../../core/shared/project-file-types'
import { betterReactMemo } from '../../uuiui-deps'
import {
  ConsoleLog,
  getAllLintErrors,
  getOpenEditorTab,
  getOpenFile,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  isOpenFileTab,
  parseFailureAsErrorMessages,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { ScriptEditor } from './script-editor'

export const CodeEditorContainer = betterReactMemo('CodeEditorContainer', (props) => {
  const runtimeErrors: RuntimeErrorInfo[] = []
  const canvasConsoleLogs: ConsoleLog[] = []

  const selectedProps = useEditorState((store) => {
    const openEditorTab = getOpenEditorTab(store.editor)
    const openFilePath =
      openEditorTab != null && isOpenFileTab(openEditorTab) ? openEditorTab.filename : null
    const selectedFile = getOpenFile(store.editor)
    const openUIJSFile = getOpenUIJSFile(store.editor)
    const openUIJSFileKey = getOpenUIJSFileKey(store.editor)

    return {
      dispatch: store.dispatch,
      workers: store.workers,
      filePath: openFilePath,
      openFile: selectedFile,
      cursorPositionFromOpenFile:
        store.editor.selectedFile == null ? null : store.editor.selectedFile.initialCursorPosition,
      savedCursorPosition: openFilePath == null ? null : store.editor.cursorPositions[openFilePath],
      typeDefinitions: getDependencyTypeDefinitions(store.editor.nodeModules.files),
      lintErrors: getAllLintErrors(store.editor),
      parserPrinterErrors: parseFailureAsErrorMessages(openUIJSFileKey, openUIJSFile),
      projectContents: store.editor.projectContents,
      parsedHighlightBounds:
        openUIJSFile != null && isParseSuccess(openUIJSFile.fileContents.parsed)
          ? openUIJSFile.fileContents.parsed.highlightBounds
          : null,
      allTemplatePaths: store.derived.navigatorTargets,
      focusedPanel: store.editor.focusedPanel,
      codeEditorTheme: store.editor.codeEditorTheme,
      selectedViews: store.editor.selectedViews,
    }
  }, 'ScriptEditor')

  return (
    <ScriptEditor
      relevantPanel={'uicodeeditor'}
      runtimeErrors={runtimeErrors}
      canvasConsoleLogs={canvasConsoleLogs}
      dispatch={selectedProps.dispatch}
      workers={selectedProps.workers}
      filePath={selectedProps.filePath}
      openFile={selectedProps.openFile}
      cursorPositionFromOpenFile={selectedProps.cursorPositionFromOpenFile}
      savedCursorPosition={selectedProps.savedCursorPosition}
      typeDefinitions={selectedProps.typeDefinitions}
      lintErrors={selectedProps.lintErrors}
      parserPrinterErrors={selectedProps.parserPrinterErrors}
      projectContents={selectedProps.projectContents}
      parsedHighlightBounds={selectedProps.parsedHighlightBounds}
      allTemplatePaths={selectedProps.allTemplatePaths}
      focusedPanel={selectedProps.focusedPanel}
      codeEditorTheme={selectedProps.codeEditorTheme}
      selectedViews={selectedProps.selectedViews}
    />
  )
})
