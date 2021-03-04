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
  exists,
  writeFileUnsavedContentAsUTF8,
  clearFileUnsavedContent,
} from 'utopia-vscode-common'
import { UtopiaFSExtension } from './utopia-fs'
import { fromUtopiaURI } from './path-utils'

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const workspaceRootUri = vscode.workspace.workspaceFolders[0].uri
  const projectID = workspaceRootUri.scheme
  useFileSystemProviderErrors(projectID)

  await initFS(projectID)
  const utopiaFS = initUtopiaFSProvider(projectID, context)
  initMessaging(context, workspaceRootUri)

  watchForUnsavedContentChangesFromFS(utopiaFS)
  watchForChangesFromVSCode(context, projectID)
}

async function initFS(projectID: string): Promise<void> {
  await initializeFS(projectID, 'VSCODE')
  await ensureDirectoryExists(RootDir)
}

function initUtopiaFSProvider(
  projectID: string,
  context: vscode.ExtensionContext,
): UtopiaFSExtension {
  const utopiaFS = new UtopiaFSExtension(projectID)
  context.subscriptions.push(utopiaFS)
  return utopiaFS
}

function watchForUnsavedContentChangesFromFS(utopiaFS: UtopiaFSExtension) {
  utopiaFS.onUtopiaDidChangeUnsavedContent((uris) => {
    uris.forEach((uri) => {
      updateDirtyContent(uri)
    })
  })
  utopiaFS.onUtopiaDidChangeSavedContent((uris) => {
    uris.forEach((uri) => {
      clearDirtyFlags(uri)
    })
  })
}

let dirtyFiles: Set<string> = new Set()
let incomingFileChanges: Set<string> = new Set()

function watchForChangesFromVSCode(context: vscode.ExtensionContext, projectID: string) {
  function isUtopiaDocument(document: vscode.TextDocument): boolean {
    return document.uri.scheme === projectID
  }

  context.subscriptions.push(
    vscode.workspace.onDidChangeTextDocument((event) => {
      if (isUtopiaDocument(event.document)) {
        const resource = event.document.uri
        if (resource.scheme === projectID) {
          // Don't act on changes to other documents
          const path = fromUtopiaURI(resource)
          if (event.document.isDirty) {
            // New unsaved change
            dirtyFiles.add(path)
            if (incomingFileChanges.has(path)) {
              // This change actually came from Utopia, so we don't want to re-write it to the FS
              incomingFileChanges.delete(path)
            } else {
              const fullText = event.document.getText()
              writeFileUnsavedContentAsUTF8(path, fullText)
            }
          }
        }
      }
    }),
    vscode.workspace.onWillSaveTextDocument((event) => {
      if (isUtopiaDocument(event.document)) {
        const path = fromUtopiaURI(event.document.uri)
        dirtyFiles.delete(path)
      }
    }),
    vscode.workspace.onDidCloseTextDocument((document) => {
      if (isUtopiaDocument(document)) {
        const path = fromUtopiaURI(document.uri)
        if (dirtyFiles.has(path)) {
          // User decided to bin unsaved changes when closing the document
          clearFileUnsavedContent(path)
          dirtyFiles.delete(path)
        }
      }
    }),
  )
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
    switch (message.type) {
      case 'OPEN_FILE':
        openFile(vscode.Uri.joinPath(workspaceRootUri, message.filePath))
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
    new vscode.Position(0, 0),
    new vscode.Position(Number.MAX_VALUE, Number.MAX_VALUE),
  )
}

async function clearDirtyFlags(resource: vscode.Uri): Promise<void> {
  // File saved on Utopia side, so FS has been updated, so we want VS Code to revert
  // to the now saved version
  vscode.commands.executeCommand('workbench.action.files.revertResource', resource)
}

async function updateDirtyContent(resource: vscode.Uri): Promise<void> {
  const filePath = fromUtopiaURI(resource)
  const { unsavedContent } = await readFileAsUTF8(filePath)
  if (unsavedContent != null) {
    incomingFileChanges.add(filePath)
    const workspaceEdit = new vscode.WorkspaceEdit()
    workspaceEdit.replace(resource, entireDocRange(), unsavedContent)
    const editApplied = await vscode.workspace.applyEdit(workspaceEdit)
    if (!editApplied) {
      // Something went wrong applying the edit, so we clear the block on unsaved content fs writes
      incomingFileChanges.delete(filePath)
    }
  }
}

async function openFile(fileUri: vscode.Uri, retries: number = 5): Promise<void> {
  const filePath = fromUtopiaURI(fileUri)
  const fileExists = await exists(filePath)
  if (fileExists) {
    vscode.commands.executeCommand('vscode.open', fileUri)
  } else {
    // Just in case the message is processed before the file has been written to the FS
    if (retries > 0) {
      setTimeout(() => openFile(fileUri, retries - 1), 100)
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
