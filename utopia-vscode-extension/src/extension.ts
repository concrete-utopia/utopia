import * as vscode from 'vscode'
import {
  ensureDirectoryExists,
  RootDir,
  initMailbox,
  isOpenFileMessage,
  VSCodeInbox,
  isUpdateDecorationsMessage,
  DecorationRange,
  DecorationRangeType,
  setErrorHandler,
  FSError,
  toUtopiaPath,
  initializeFS,
  isSelectedElementChanged,
  BoundsInFile,
  Bounds,
  ToVSCodeMessage,
  parseToVSCodeMessage,
  sendMessage,
  editorCursorPositionChanged,
} from 'utopia-vscode-common'
import { UtopiaFSExtension } from './utopia-fs'

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const workspaceRootUri = vscode.workspace.workspaceFolders[0].uri
  const projectID = workspaceRootUri.scheme
  useFileSystemProviderErrors(projectID)
  await initializeFS(projectID)
  await ensureDirectoryExists(RootDir)
  const utopiaFS = new UtopiaFSExtension(projectID)
  context.subscriptions.push(utopiaFS)
  initMessaging(context, workspaceRootUri)
}

const selectionDecorationType = vscode.window.createTextEditorDecorationType({
  borderColor: '#007aff',
  borderStyle: 'solid',
  borderWidth: '0 0 0 3px',
  isWholeLine: true,
})

const highlightDecorationType = vscode.window.createTextEditorDecorationType({
  borderColor: '#007aff11',
  borderStyle: 'solid',
  borderWidth: '0 0 0 3px',
  isWholeLine: true,
})

const allDecorationRangeTypes: Array<DecorationRangeType> = ['highlight', 'selection']

function initMessaging(context: vscode.ExtensionContext, workspaceRootUri: vscode.Uri): void {
  // State that needs to be stored between messages.
  let currentDecorations: Array<DecorationRange> = []

  function handleMessage(message: ToVSCodeMessage): void {
    if (isOpenFileMessage(message)) {
      vscode.commands.executeCommand(
        'vscode.open',
        vscode.Uri.joinPath(workspaceRootUri, message.filePath),
      )
    } else if (isUpdateDecorationsMessage(message)) {
      currentDecorations = message.decorations
      updateDecorations(currentDecorations)
    } else if (isSelectedElementChanged(message)) {
      revealRangeIfPossible(message.boundsInFile)
    } else {
      console.error(`Unhandled message type ${JSON.stringify(message)}`)
    }
  }

  initMailbox(VSCodeInbox, parseToVSCodeMessage, handleMessage)

  context.subscriptions.push(
    vscode.window.onDidChangeOpenEditors(() => {
      updateDecorations(currentDecorations)
    }),
    vscode.window.onDidChangeTextEditorSelection((event) => {
      cursorPositionChanged(event)
    }),
  )
}

function cursorPositionChanged(event: vscode.TextEditorSelectionChangeEvent): void {
  const editor = event.textEditor
  const filename = editor.document.uri.path
  const position = editor.selection.active
  sendMessage(editorCursorPositionChanged(filename, position.line, position.character))
}

function revealRangeIfPossible(boundsInFile: BoundsInFile): void {
  const visibleEditors = vscode.window.visibleTextEditors
  for (const visibleEditor of visibleEditors) {
    const filename = visibleEditor.document.uri.path
    if (boundsInFile.filePath === filename) {
      visibleEditor.revealRange(getVSCodeRange(boundsInFile))
    }
  }
}

type DecorationsByType = { [key: string]: Array<DecorationRange> }

type DecorationsByFilenameAndType = { [key: string]: DecorationsByType }

function getVSCodeDecorationType(rangeType: DecorationRangeType): vscode.TextEditorDecorationType {
  switch (rangeType) {
    case 'highlight':
      return highlightDecorationType
    case 'selection':
      return selectionDecorationType
    default:
      const _exhaustiveCheck: never = rangeType
      throw new Error(`Unhandled type ${JSON.stringify(rangeType)}`)
  }
}

function getDecorationsByFilenameAndType(
  decorations: Array<DecorationRange>,
): DecorationsByFilenameAndType {
  let result: DecorationsByFilenameAndType = {}

  for (const range of decorations) {
    // Add the top level entry by the filename.
    let decorationsForFilename: DecorationsByType
    if (range.filePath in result) {
      decorationsForFilename = result[range.filePath]
    } else {
      decorationsForFilename = {}
      result[range.filePath] = decorationsForFilename
    }

    // Add the entry within by decoration type.
    let decorationsForType: Array<DecorationRange>
    if (range.rangeType in decorationsForFilename) {
      decorationsForType = decorationsForFilename[range.rangeType]
    } else {
      decorationsForType = []
      decorationsForFilename[range.rangeType] = decorationsForType
    }
    decorationsForType.push(range)
  }

  return result
}

function getVSCodeRange(bounds: Bounds): vscode.Range {
  return new vscode.Range(
    new vscode.Position(bounds.startLine, bounds.startCol),
    new vscode.Position(bounds.endLine, bounds.endCol),
  )
}

function updateDecorations(decorations: Array<DecorationRange>): void {
  const visibleEditors = vscode.window.visibleTextEditors
  const decorationsByFilenameAndType = getDecorationsByFilenameAndType(decorations)
  for (const visibleEditor of visibleEditors) {
    const filename = visibleEditor.document.uri.path
    // Default in the possible value we have received for a filename.
    const decorations = decorationsByFilenameAndType[filename] ?? {}
    for (const rangeType of allDecorationRangeTypes) {
      // Default in the possible value we have received for a range type.
      const decorationsForType = decorations[rangeType] ?? []
      // Construct the VS Code values and set those against the editor.
      const vsCodeDecorationType = getVSCodeDecorationType(rangeType)
      const vsCodeRanges = decorationsForType.map(getVSCodeRange)
      visibleEditor.setDecorations(vsCodeDecorationType, vsCodeRanges)
    }
  }
}

function useFileSystemProviderErrors(projectID: string): void {
  setErrorHandler((e) => toFileSystemProviderError(projectID, e))
}

function toFileSystemProviderError(projectID: string, error: FSError): vscode.FileSystemError {
  const { path: unadjustedPath, code } = error
  const path = toUtopiaPath(projectID, unadjustedPath)
  switch (code) {
    case 'ENOENT':
      return vscode.FileSystemError.FileNotFound(path)
    case 'EISDIR':
      return vscode.FileSystemError.FileIsADirectory(path)
    case 'ENOTDIR':
      return vscode.FileSystemError.FileNotADirectory(path)
    case 'EEXIST':
      return vscode.FileSystemError.FileExists(path)
    default:
      const _exhaustiveCheck: never = code
      throw new Error(`Unhandled FS Error ${JSON.stringify(error)}`)
  }
}
