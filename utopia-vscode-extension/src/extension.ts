import * as vscode from 'vscode'
import {
  ensureDirectoryExists,
  RootDir,
  initMailbox,
  VSCodeInbox,
  DecorationRange,
  DecorationRangeType,
  setErrorHandler,
  FSError,
  toUtopiaPath,
  initializeFS,
  BoundsInFile,
  Bounds,
  ToVSCodeMessage,
  parseToVSCodeMessage,
  sendMessage,
  editorCursorPositionChanged,
  readFileAsUTF8,
  pathIsFile,
} from 'utopia-vscode-common'
import { UtopiaFSExtension } from './utopia-fs'
import { fromUtopiaURI } from './path-utils'

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const workspaceRootUri = vscode.workspace.workspaceFolders[0].uri
  const projectID = workspaceRootUri.scheme
  useFileSystemProviderErrors(projectID)
  await initializeFS(projectID)
  await ensureDirectoryExists(RootDir)
  const utopiaFS = new UtopiaFSExtension(projectID)
  context.subscriptions.push(utopiaFS)
  initMessaging(utopiaFS, context, workspaceRootUri)

  utopiaFS.onDidChangeFile((changes) => {
    changes.forEach((change) => {
      if (change.type === vscode.FileChangeType.Changed) {
        updateDirtyFlags(change.uri)
      }
    })
  })
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

function initMessaging(
  utopiaFS: UtopiaFSExtension,
  context: vscode.ExtensionContext,
  workspaceRootUri: vscode.Uri,
): void {
  // State that needs to be stored between messages.
  let currentDecorations: Array<DecorationRange> = []

  function handleMessage(message: ToVSCodeMessage): void {
    switch (message.type) {
      case 'OPEN_FILE':
        openFile(utopiaFS, vscode.Uri.joinPath(workspaceRootUri, message.filePath))
        break
      case 'UPDATE_DECORATIONS':
        currentDecorations = message.decorations
        updateDecorations(currentDecorations)
        break
      case 'SELECTED_ELEMENT_CHANGED':
        revealRangeIfPossible(message.boundsInFile)
        break
      default:
        const _exhaustiveCheck: never = message
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

function entireDocRange() {
  return new vscode.Range(
    new vscode.Position(-1, -1),
    new vscode.Position(Number.MAX_VALUE, Number.MAX_VALUE),
  )
}

async function updateDirtyFlags(resource: vscode.Uri): Promise<void> {
  const filePath = fromUtopiaURI(resource)
  const isFile = await pathIsFile(filePath)
  if (isFile) {
    const { unsavedContent } = await readFileAsUTF8(filePath)
    if (unsavedContent == null) {
      vscode.commands.executeCommand('workbench.action.files.revertResource', resource)
    } else {
      const workspaceEdit = new vscode.WorkspaceEdit()
      workspaceEdit.replace(resource, entireDocRange(), unsavedContent)
      vscode.workspace.applyEdit(workspaceEdit)
    }
  }
}

async function openFile(
  utopiaFS: UtopiaFSExtension,
  fileUri: vscode.Uri,
  retries: number = 5,
): Promise<void> {
  const fileExists = await utopiaFS.exists(fileUri)
  if (fileExists) {
    vscode.commands.executeCommand('vscode.open', fileUri)
  } else {
    if (retries > 0) {
      setTimeout(() => openFile(utopiaFS, fileUri, retries - 1), 100)
    }
  }
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
