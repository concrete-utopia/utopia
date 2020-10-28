/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { betterReactMemo } from 'uuiui-deps'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { ComponentMetadata } from '../../core/shared/element-template'
import {
  getHighlightBoundsFromParseResult,
  uiJsFile,
  updateLastSavedContents,
  updateParseResultCode,
} from '../../core/model/project-file-utils'
import { messageIsFatalOrError, messageIsWarning } from '../../core/shared/error-messages'
import {
  HighlightBoundsForUids,
  ParseFailure,
  ParseSuccess,
  ProjectFile,
  RevisionsState,
  TemplatePath,
  isUIJSFile,
  isParseSuccess,
} from '../../core/shared/project-file-types'
import { codeNeedsPrinting } from '../../core/workers/common/project-file-utils'
import { isJsFile } from '../../core/workers/ts/ts-worker'
import { foldEither, isRight } from '../../core/shared/either'
import Utils from '../../utils/utils'
import { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { runtimeErrorInfoToErrorMessage } from './monaco-wrapper'
import { EditorPanel, setFocus } from '../common/actions'
import { EditorAction } from '../editor/action-types'
import * as EditorActions from '../editor/actions/actions'
import {
  dependenciesFromPackageJson,
  usePossiblyResolvedPackageDependencies,
} from '../editor/npm-dependency/npm-dependency'
import {
  ConsoleLog,
  EditorStore,
  getAllLintErrors,
  getOpenEditorTab,
  getOpenFile,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  isOpenFileTab,
  openFileTab,
  packageJsonFileFromProjectContents,
  parseFailureAsErrorMessages,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { useKeepReferenceEqualityIfPossible } from '../inspector/common/property-path-hooks'
import * as TP from '../../core/shared/template-path'
import { CodeEditor } from './code-editor'
import { CursorPosition } from './code-editor-utils'
import { Notice } from '../common/notices'
import { getDependencyTypeDefinitions } from '../../core/es-modules/package-manager/package-manager'

function getFileContents(file: ProjectFile): string {
  switch (file.type) {
    case 'DIRECTORY':
    case 'IMAGE_FILE':
    case 'ASSET_FILE':
      return ''
    case 'CODE_FILE':
      return file.fileContents
    case 'UI_JS_FILE':
      return foldEither(
        (failure: ParseFailure) => {
          return failure.code
        },
        (success: ParseSuccess) => {
          return success.code ?? ''
        },
        file.fileContents,
      )
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
  }
}

function fileIsLocked(file: ProjectFile): boolean {
  return isUIJSFile(file) && codeNeedsPrinting(file.revisionsState)
}

function updateFileContents(contents: string, file: ProjectFile, manualSave: boolean): ProjectFile {
  switch (file.type) {
    case 'DIRECTORY':
    case 'IMAGE_FILE':
    case 'ASSET_FILE':
      return file
    case 'CODE_FILE':
      const codeLastSavedContents = updateLastSavedContents(
        file.fileContents,
        file.lastSavedContents,
        manualSave,
      )
      return {
        ...file,
        fileContents: contents,
        lastSavedContents: codeLastSavedContents,
      }
    case 'UI_JS_FILE':
      const uiJsLastSavedContents = updateLastSavedContents(
        file.fileContents,
        file.lastSavedContents,
        manualSave,
      )
      return uiJsFile(
        updateParseResultCode(
          file.fileContents,
          contents,
          getHighlightBoundsFromParseResult(file.fileContents), // here we just update the code without updating the highlights!
        ),
        uiJsLastSavedContents,
        RevisionsState.CodeAhead,
        Date.now(),
      )
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
  }
}

function getHighlightBoundsForTemplatePath(path: TemplatePath, store: EditorStore) {
  const selectedFile = getOpenFile(store.editor)
  if (isUIJSFile(selectedFile) && isParseSuccess(selectedFile.fileContents)) {
    const parseSuccess = selectedFile.fileContents.value
    if (TP.isInstancePath(path)) {
      const highlightedUID = Utils.optionalMap(
        TP.toUid,
        MetadataUtils.dynamicPathToStaticPath(store.editor.jsxMetadataKILLME, path),
      )
      if (highlightedUID != null) {
        return parseSuccess.highlightBounds[highlightedUID]
      }
    }
  }
  return null
}

function getTemplatePathsInBounds(
  line: number,
  parsedHighlightBounds: HighlightBoundsForUids | null,
  allTemplatePaths: TemplatePath[],
  jsxMetadataKILLME: ComponentMetadata[],
): TemplatePath[] {
  if (parsedHighlightBounds == null) {
    return []
  }
  const sortedHighlightBounds = Object.values(parsedHighlightBounds).sort(
    (a, b) => b.startLine - a.startLine,
  )
  const targets = sortedHighlightBounds
    .filter((bounds) => {
      // TS line numbers are zero based, monaco is 1-based
      return line >= bounds.startLine + 1 && line <= bounds.endLine + 1
    })
    .map((bound) => bound.uid)
  let paths: TemplatePath[] = []
  if (targets.length > 0) {
    const target = targets[0]
    Utils.fastForEach(allTemplatePaths, (path) => {
      if (TP.isInstancePath(path)) {
        const staticPath = MetadataUtils.dynamicPathToStaticPath(jsxMetadataKILLME, path)
        const uid = staticPath != null ? TP.toUid(staticPath) : null
        if (uid === target) {
          paths.push(path)
        }
      }
    })
  }
  return paths
}

interface ScriptEditorProps {
  relevantPanel: EditorPanel
  runtimeErrors: Array<RuntimeErrorInfo>
  canvasConsoleLogs: Array<ConsoleLog>
}

export const ScriptEditor = betterReactMemo('ScriptEditor', (props: ScriptEditorProps) => {
  const { relevantPanel, runtimeErrors, canvasConsoleLogs } = props
  const {
    dispatch,
    workers,
    filePath,
    openFile,
    cursorPositionFromOpenFile,
    savedCursorPosition,
    packageJsonFile,
    typeDefinitions,
    lintErrors,
    parserPrinterErrors,
    projectContents,
    parsedHighlightBounds,
    allTemplatePaths,
    jsxMetadataKILLME,
    focusedPanel,
    codeEditorTheme,
    selectedViews,
  } = useEditorState((store) => {
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
      packageJsonFile: packageJsonFileFromProjectContents(store.editor.projectContents),
      typeDefinitions: getDependencyTypeDefinitions(store.editor.nodeModules.files),
      lintErrors: getAllLintErrors(store.editor),
      parserPrinterErrors: parseFailureAsErrorMessages(openUIJSFileKey, openUIJSFile),
      projectContents: store.editor.projectContents,
      parsedHighlightBounds:
        openUIJSFile != null && isRight(openUIJSFile.fileContents)
          ? openUIJSFile.fileContents.value.highlightBounds
          : null,
      allTemplatePaths: store.derived.navigatorTargets,
      jsxMetadataKILLME: store.editor.jsxMetadataKILLME,
      focusedPanel: store.editor.focusedPanel,
      codeEditorTheme: store.editor.codeEditorTheme,
      selectedViews: store.editor.selectedViews,
    }
  }, 'ScriptEditor')

  const selectedViewBounds = useKeepReferenceEqualityIfPossible(
    useEditorState((store) => {
      return Utils.stripNulls(
        store.editor.selectedViews.map((selectedView) =>
          getHighlightBoundsForTemplatePath(selectedView, store),
        ),
      )
    }, 'ScriptEditor selectedViewBounds'),
  )

  const highlightBounds = useKeepReferenceEqualityIfPossible(
    useEditorState((store) => {
      return Utils.stripNulls(
        store.editor.highlightedViews.map((highlightedView) =>
          getHighlightBoundsForTemplatePath(highlightedView, store),
        ),
      )
    }, 'ScriptEditor highlightBounds'),
  )

  const onHover = React.useCallback(
    (line: number, column: number) => {
      const targets = getTemplatePathsInBounds(
        line,
        parsedHighlightBounds,
        allTemplatePaths,
        jsxMetadataKILLME,
      )
      if (targets.length > 0) {
        const actions = targets.map((path) => EditorActions.setHighlightedView(path))
        dispatch(actions, 'everyone')
      }
    },
    [parsedHighlightBounds, allTemplatePaths, jsxMetadataKILLME, dispatch],
  )

  const onSelect = React.useCallback(
    (line: number, column: number) => {
      const targets = getTemplatePathsInBounds(
        line,
        parsedHighlightBounds,
        allTemplatePaths,
        jsxMetadataKILLME,
      )
      if (targets.length > 0) {
        dispatch([EditorActions.selectComponents(targets, false)], 'everyone')
      }
    },
    [parsedHighlightBounds, allTemplatePaths, jsxMetadataKILLME, dispatch],
  )

  const onSave = React.useCallback(
    (value: string, manualSave: boolean, toast?: Notice) => {
      if (filePath != null && openFile != null) {
        const updatedFile = updateFileContents(value, openFile, manualSave)

        const updateFileActions: EditorAction[] = [
          EditorActions.updateFile(filePath, updatedFile, false),
          EditorActions.setCodeEditorBuildErrors({}),
        ]
        const withToastAction =
          toast == null
            ? updateFileActions
            : updateFileActions.concat(EditorActions.pushToast(toast))

        const actions = manualSave
          ? withToastAction.concat(EditorActions.saveCurrentFile())
          : withToastAction
        dispatch(actions, 'everyone')
      }
    },
    [openFile, dispatch, filePath],
  )

  const onOpenFile = React.useCallback(
    (path: string, cursorPos: CursorPosition | null) => {
      dispatch([EditorActions.openEditorTab(openFileTab(path), cursorPos)], 'everyone')
    },
    [dispatch],
  )

  const close = React.useCallback(() => {
    dispatch([EditorActions.setCodeEditorVisibility(false)], 'everyone')
  }, [dispatch])

  const sendLinterRequestMessage = React.useCallback(
    (content: string) => {
      if (filePath != null && isJsFile(filePath)) {
        workers.sendLinterRequestMessage(filePath, content)
      }
    },
    [filePath, workers],
  )

  const saveCursorPosition = React.useCallback(
    (position: CursorPosition) => {
      if (filePath != null) {
        dispatch([EditorActions.saveCursorPosition(filePath, position)], 'everyone')
      }
    },
    [filePath, dispatch],
  )

  const onFocus = React.useCallback(() => {
    if (focusedPanel !== relevantPanel) {
      dispatch([setFocus(relevantPanel)], 'everyone')
    }
  }, [focusedPanel, dispatch, relevantPanel])

  const newErrors = [
    ...runtimeErrors.map((e) => runtimeErrorInfoToErrorMessage(projectContents, e)),
    ...lintErrors.filter(messageIsFatalOrError),
    ...lintErrors.filter(messageIsWarning),
  ]
  const newErrorsOrParserPrinterErrors = newErrors.length > 0 ? newErrors : parserPrinterErrors
  const errors = useKeepReferenceEqualityIfPossible(newErrorsOrParserPrinterErrors)
  const newErrorsForFile = errors.filter((error) => error.fileName === filePath)
  const errorsForFile = useKeepReferenceEqualityIfPossible(newErrorsForFile)

  const npmDependencies = usePossiblyResolvedPackageDependencies()

  if (filePath == null || openFile == null) {
    return null
  } else {
    const fileContents = getFileContents(openFile)
    const readOnly = fileIsLocked(openFile)

    return (
      <CodeEditor
        key={'script-code-editor'}
        name={filePath}
        value={fileContents}
        onSave={onSave}
        onChange={sendLinterRequestMessage}
        onChangeFromProps={sendLinterRequestMessage}
        saveCursorPosition={saveCursorPosition}
        onOpenFile={onOpenFile}
        close={close}
        enabled={true}
        autoSave={true}
        npmTypeDefinitions={{
          npmDependencies: npmDependencies,
          typeDefinitions: typeDefinitions,
        }}
        allErrors={errors}
        errorsForFile={errorsForFile}
        readOnly={readOnly}
        projectContents={projectContents}
        workers={workers}
        selectedViews={selectedViews}
        selectedViewsBounds={selectedViewBounds}
        highlightedViewsBounds={highlightBounds}
        onHover={onHover}
        onSelect={onSelect}
        onFocus={onFocus}
        cursorPosition={
          cursorPositionFromOpenFile == null ? savedCursorPosition : cursorPositionFromOpenFile
        }
        canvasConsoleLogs={canvasConsoleLogs}
        codeEditorTheme={codeEditorTheme}
      />
    )
  }
})
ScriptEditor.displayName = 'ScriptEditor'
