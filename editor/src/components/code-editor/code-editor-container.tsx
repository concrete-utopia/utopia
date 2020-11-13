import * as React from 'react'
import * as TP from '../../core/shared/template-path'
import * as EditorActions from '../editor/actions/actions'
import { getDependencyTypeDefinitions } from '../../core/es-modules/package-manager/package-manager'
import { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import {
  AssetFile,
  Directory,
  ImageFile,
  isParseSuccess,
  isTextFile,
  ProjectFile,
  TemplatePath,
  TextFile,
} from '../../core/shared/project-file-types'
import { betterReactMemo, Utils } from '../../uuiui-deps'
import {
  ConsoleLog,
  EditorStore,
  EditorTab,
  getAllLintErrors,
  getOpenEditorTab,
  getOpenFile,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  isOpenFileTab,
  parseFailureAsErrorMessages,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { useKeepReferenceEqualityIfPossible } from '../inspector/common/property-path-hooks'
import { ScriptEditor } from './script-editor'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { Notice } from '../common/notices'
import { EditorAction } from '../editor/action-types'
import { CursorPosition } from './code-editor-utils'
import { EditorPanel, setFocus } from '../common/actions'

export const CodeEditorIFrame = betterReactMemo('CodeEditorContainer', (props) => {
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
  }, 'CodeEditorContainer')

  const selectedViewBounds = useKeepReferenceEqualityIfPossible(
    useEditorState((store) => {
      return Utils.stripNulls(
        store.editor.selectedViews.map((selectedView) =>
          getHighlightBoundsForTemplatePath(selectedView, store),
        ),
      )
    }, 'CodeEditorContainer selectedViewBounds'),
  )

  const highlightBounds = useKeepReferenceEqualityIfPossible(
    useEditorState((store) => {
      return Utils.stripNulls(
        store.editor.highlightedViews.map((highlightedView) =>
          getHighlightBoundsForTemplatePath(highlightedView, store),
        ),
      )
    }, 'CodeEditorContainer highlightBounds'),
  )

  const dispatch = useEditorState((store) => store.dispatch, 'CodeEditorContainer dispatch')

  const setHighlightedViews = React.useCallback(
    (targets: TemplatePath[]) => {
      const actions = targets.map((path) => EditorActions.setHighlightedView(path))
      dispatch(actions)
    },
    [dispatch],
  )

  const selectComponents = React.useCallback(
    (targets: TemplatePath[]) => {
      dispatch([EditorActions.selectComponents(targets, false)])
    },
    [dispatch],
  )

  const onSave = React.useCallback(
    (
      manualSave: boolean,
      toast: Notice | undefined,
      filePath: string,
      updatedFile: ProjectFile,
    ) => {
      const updateFileActions: EditorAction[] = [
        EditorActions.updateFile(filePath, updatedFile, false),
        EditorActions.setCodeEditorBuildErrors({}),
      ]
      const withToastAction =
        toast == null ? updateFileActions : updateFileActions.concat(EditorActions.pushToast(toast))

      const actions = manualSave
        ? withToastAction.concat(EditorActions.saveCurrentFile())
        : withToastAction
      dispatch(actions, 'everyone')
    },
    [dispatch],
  )

  const openEditorTab = React.useCallback(
    (editorTab: EditorTab, cursorPosition: CursorPosition | null) => {
      dispatch([EditorActions.openEditorTab(editorTab, cursorPosition)])
    },
    [dispatch],
  )

  const setCodeEditorVisibility = React.useCallback(
    (visible: boolean) => {
      dispatch([EditorActions.setCodeEditorVisibility(visible)])
    },
    [dispatch],
  )

  const setFocusCallback = React.useCallback(
    (panel: EditorPanel | null) => {
      dispatch([setFocus(panel)])
    },
    [dispatch],
  )

  const saveCursorPosition = React.useCallback(
    (filename: string, cursorPosition: CursorPosition) => {
      dispatch([EditorActions.saveCursorPosition(filename, cursorPosition)])
    },
    [dispatch],
  )

  return (
    <ScriptEditor
      setHighlightedViews={setHighlightedViews}
      selectComponents={selectComponents}
      onSave={onSave}
      openEditorTab={openEditorTab}
      setCodeEditorVisibility={setCodeEditorVisibility}
      setFocus={setFocusCallback}
      saveCursorPosition={saveCursorPosition}
      relevantPanel={'uicodeeditor'}
      runtimeErrors={runtimeErrors}
      canvasConsoleLogs={canvasConsoleLogs}
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
      selectedViewBounds={selectedViewBounds}
      highlightBounds={highlightBounds}
    />
  )
})

function getHighlightBoundsForTemplatePath(path: TemplatePath, store: EditorStore) {
  const selectedFile = getOpenFile(store.editor)
  if (isTextFile(selectedFile) && isParseSuccess(selectedFile.fileContents.parsed)) {
    const parseSuccess = selectedFile.fileContents.parsed
    if (TP.isInstancePath(path)) {
      const highlightedUID = Utils.optionalMap(
        TP.toUid,
        MetadataUtils.dynamicPathToStaticPath(path),
      )
      if (highlightedUID != null) {
        return parseSuccess.highlightBounds[highlightedUID]
      }
    }
  }
  return null
}
