/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { betterReactMemo } from '../../utils/react-performance'
import { ComponentMetadata } from '../../core/shared/element-template'
import {
  getHighlightBoundsFromParseResult,
  updateLastSavedContents,
  updateParsedTextFileHighlightBounds,
} from '../../core/model/project-file-utils'
import {
  ErrorMessage,
  messageIsFatalOrError,
  messageIsWarning,
} from '../../core/shared/error-messages'
import {
  HighlightBoundsForUids,
  ParseFailure,
  ParseSuccess,
  ProjectFile,
  RevisionsState,
  TemplatePath,
  isTextFile,
  isParseSuccess,
  textFile,
  textFileContents,
  TextFile,
  ImageFile,
  Directory,
  AssetFile,
  HighlightBounds,
} from '../../core/shared/project-file-types'
import { codeNeedsPrinting } from '../../core/workers/common/project-file-utils'
import { isJsFile } from '../../core/workers/ts/ts-worker'
import { foldEither, isRight } from '../../core/shared/either'
import Utils from '../../utils/utils'
import { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { runtimeErrorInfoToErrorMessage } from './monaco-wrapper'
import { EditorPanel } from '../common/actions'
import { EditorAction, EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/actions'
import {
  dependenciesFromPackageJson,
  usePossiblyResolvedPackageDependencies,
} from '../editor/npm-dependency/npm-dependency'
import { ConsoleLog } from '../editor/store/editor-state'
import * as TP from '../../core/shared/template-path'
import { CodeEditor } from './code-editor'
import { CursorPosition } from './code-editor-utils'
import { Notice } from '../common/notices'
import { getDependencyTypeDefinitions } from '../../core/es-modules/package-manager/package-manager'
import { UtopiaTsWorkers } from '../../core/workers/common/worker-types'
import {
  PossiblyUnversionedNpmDependency,
  TypeDefinitions,
} from '../../core/shared/npm-dependency-types'
import { ProjectContentTreeRoot } from '../assets'
import { EditorTab, openFileTab } from '../editor/store/editor-tabs'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'

function getFileContents(file: ProjectFile): string {
  switch (file.type) {
    case 'DIRECTORY':
    case 'IMAGE_FILE':
    case 'ASSET_FILE':
      return ''
    case 'TEXT_FILE':
      return file.fileContents.code
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
  }
}

function fileIsLocked(file: ProjectFile): boolean {
  return isTextFile(file) && codeNeedsPrinting(file.fileContents.revisionsState)
}

function updateFileContents(contents: string, file: ProjectFile, manualSave: boolean): ProjectFile {
  switch (file.type) {
    case 'DIRECTORY':
    case 'IMAGE_FILE':
    case 'ASSET_FILE':
      return file
    case 'TEXT_FILE':
      const uiJsLastSavedContents = updateLastSavedContents(
        file.fileContents,
        file.lastSavedContents,
        manualSave,
      )

      const newParsed = updateParsedTextFileHighlightBounds(
        file.fileContents.parsed,
        getHighlightBoundsFromParseResult(file.fileContents.parsed), // here we just update the code without updating the highlights!
      )
      const newContents = textFileContents(contents, newParsed, RevisionsState.CodeAhead)
      return textFile(newContents, uiJsLastSavedContents, Date.now())
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
  }
}

function getTemplatePathsInBounds(
  line: number,
  parsedHighlightBounds: HighlightBoundsForUids | null,
  allTemplatePaths: TemplatePath[],
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
        const staticPath = TP.dynamicPathToStaticPath(path)
        const uid = staticPath != null ? TP.toUid(staticPath) : null
        if (uid === target) {
          paths.push(path)
        }
      }
    })
  }
  return paths
}

export interface ScriptEditorProps {
  setHighlightedViews: (targets: TemplatePath[]) => void
  selectComponents: (targets: TemplatePath[]) => void
  onSave: (
    manualSave: boolean,
    toast: Notice | undefined,
    filePath: string,
    updatedFile: ProjectFile,
  ) => void
  openEditorTab: (editorTab: EditorTab, cursorPosition: CursorPosition | null) => void
  setCodeEditorVisibility: (visible: boolean) => void
  setFocus: (panel: EditorPanel | null) => void
  sendLinterRequestMessage: (filePath: string, content: string) => void
  relevantPanel: EditorPanel
  runtimeErrors: Array<RuntimeErrorInfo>
  canvasConsoleLogs: Array<ConsoleLog>
  selectedViews: TemplatePath[]
  filePath: string | null
  openFile: TextFile | ImageFile | Directory | AssetFile | null
  cursorPositionFromOpenFile: CursorPosition | null
  typeDefinitions: TypeDefinitions
  lintErrors: ErrorMessage[]
  parserPrinterErrors: ErrorMessage[]
  projectContents: ProjectContentTreeRoot
  parsedHighlightBounds: HighlightBoundsForUids | null
  allTemplatePaths: TemplatePath[]
  focusedPanel: EditorPanel | null
  codeEditorTheme: string
  selectedViewBounds: HighlightBounds[]
  highlightBounds: HighlightBounds[]
  npmDependencies: PossiblyUnversionedNpmDependency[]
}

export const ScriptEditor = betterReactMemo('ScriptEditor', (props: ScriptEditorProps) => {
  const {
    setHighlightedViews,
    selectComponents,
    onSave,
    openEditorTab,
    setCodeEditorVisibility,
    setFocus,
    sendLinterRequestMessage,
    relevantPanel,
    runtimeErrors,
    canvasConsoleLogs,
    filePath,
    openFile,
    cursorPositionFromOpenFile,
    typeDefinitions,
    lintErrors,
    parserPrinterErrors,
    projectContents,
    parsedHighlightBounds,
    allTemplatePaths,
    focusedPanel,
    codeEditorTheme,
    selectedViews,
    selectedViewBounds,
    highlightBounds,
    npmDependencies,
  } = props

  const [cursorPositions, setCursorPositions] = React.useState<{ [key: string]: CursorPosition }>(
    {},
  )

  const onHover = React.useCallback(
    (line: number, column: number) => {
      const targets = getTemplatePathsInBounds(line, parsedHighlightBounds, allTemplatePaths)
      if (targets.length > 0) {
        setHighlightedViews(targets)
      }
    },
    [setHighlightedViews, parsedHighlightBounds, allTemplatePaths],
  )

  const onSelect = React.useCallback(
    (line: number, column: number) => {
      const targets = getTemplatePathsInBounds(line, parsedHighlightBounds, allTemplatePaths)
      if (targets.length > 0) {
        selectComponents(targets)
      }
    },
    [selectComponents, parsedHighlightBounds, allTemplatePaths],
  )

  const onSaveInner = React.useCallback(
    (value: string, manualSave: boolean, toast?: Notice) => {
      if (filePath != null && openFile != null) {
        const updatedFile = updateFileContents(value, openFile, manualSave)
        onSave(manualSave, toast, filePath, updatedFile)
      }
    },
    [onSave, openFile, filePath],
  )

  const onOpenFile = React.useCallback(
    (path: string, cursorPos: CursorPosition | null) => {
      openEditorTab(openFileTab(path), cursorPos)
    },
    [openEditorTab],
  )

  const close = React.useCallback(() => {
    setCodeEditorVisibility(false)
  }, [setCodeEditorVisibility])

  const sendLinterRequestMessageInner = React.useCallback(
    (content: string) => {
      if (filePath != null && isJsFile(filePath)) {
        sendLinterRequestMessage(filePath, content)
      }
    },
    [sendLinterRequestMessage, filePath],
  )

  const saveCursorPositionInner = React.useCallback(
    (position: CursorPosition) => {
      if (filePath != null) {
        setCursorPositions({ ...cursorPositions, [filePath]: position })
      }
    },
    [cursorPositions, filePath],
  )

  const onFocus = React.useCallback(() => {
    if (focusedPanel !== relevantPanel) {
      setFocus(relevantPanel)
    }
  }, [setFocus, focusedPanel, relevantPanel])

  const newErrors = [
    ...runtimeErrors.map((e) => runtimeErrorInfoToErrorMessage(projectContents, e)),
    ...lintErrors.filter(messageIsFatalOrError),
    ...lintErrors.filter(messageIsWarning),
  ]
  const newErrorsOrParserPrinterErrors = newErrors.length > 0 ? newErrors : parserPrinterErrors
  const errors = useKeepReferenceEqualityIfPossible(newErrorsOrParserPrinterErrors)
  const newErrorsForFile = errors.filter((error) => error.fileName === filePath)
  const errorsForFile = useKeepReferenceEqualityIfPossible(newErrorsForFile)

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
        onSave={onSaveInner}
        onChange={sendLinterRequestMessageInner}
        onChangeFromProps={sendLinterRequestMessageInner}
        saveCursorPosition={saveCursorPositionInner}
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
        selectedViews={selectedViews}
        selectedViewsBounds={selectedViewBounds}
        highlightedViewsBounds={highlightBounds}
        onHover={onHover}
        onSelect={onSelect}
        onFocus={onFocus}
        cursorPosition={
          cursorPositionFromOpenFile == null
            ? cursorPositions[filePath]
            : cursorPositionFromOpenFile
        }
        canvasConsoleLogs={canvasConsoleLogs}
        codeEditorTheme={codeEditorTheme}
      />
    )
  }
})
ScriptEditor.displayName = 'ScriptEditor'
