import * as vscode from 'vscode'
import type {
  DecorationRange,
  DecorationRangeType,
  BoundsInFile,
  Bounds,
  UtopiaVSCodeConfig,
  FromUtopiaToVSCodeMessage,
  FromVSCodeToUtopiaMessage,
} from 'utopia-vscode-common'
import {
  toUtopiaPath,
  editorCursorPositionChanged,
  applyPrettier,
  utopiaVSCodeConfigValues,
  vsCodeReady,
  clearLoadingScreen,
  ProjectIDPlaceholderPrefix,
  vsCodeBridgeReady,
  vsCodeFileChange,
} from 'utopia-vscode-common'
import { UtopiaFSExtension } from './utopia-fs'
import type { TextDocumentChangeEvent, TextDocumentWillSaveEvent, Uri } from 'vscode'
import type { FSError } from './in-mem-fs'
import {
  clearFileUnsavedContent,
  exists,
  readFileAsUTF8,
  setErrorHandler,
  writeFileUnsavedContentAsUTF8,
} from './in-mem-fs'

const FollowSelectionConfigKey = 'utopia.editor.followSelection.enabled'

export function activate(context: vscode.ExtensionContext) {
  const workspaceRootUri = vscode.workspace.workspaceFolders[0].uri
  const projectID = workspaceRootUri.scheme
  if (projectID.startsWith(ProjectIDPlaceholderPrefix)) {
    // We don't want the extension to do anything now. We'll restart it with the actual projectID
    return
  }

  setErrorHandler((e) => toFileSystemProviderError(projectID, e))

  const utopiaFS = new UtopiaFSExtension(projectID)
  context.subscriptions.push(utopiaFS)

  initMessaging(context, workspaceRootUri, utopiaFS)

  watchForChangesFromVSCode(context, projectID)

  // Send a VSCodeReady message on activation as this might be triggered by an iframe reload,
  // meaning no new UtopiaReady message will have been sent
  sendMessageToUtopia(vsCodeReady())

  // watchForFileDeletions()
}

// FIXME Do we still want this? It seems wrong...
// function watchForFileDeletions() {
//   let fileWatcherChain: Promise<void> = Promise.resolve()
//   const fileWatcher = vscode.workspace.createFileSystemWatcher('**/*')
//   fileWatcher.onDidDelete(async (deletedFile) => {
//     for (const textDocument of vscode.workspace.textDocuments) {
//       if (textDocument.uri.fsPath === deletedFile.fsPath) {
//         fileWatcherChain = fileWatcherChain.then(async () => {
//           await vscode.window.showTextDocument(textDocument)
//           return Promise.resolve()
//         })
//         if (textDocument.isDirty) {
//           await clearDirtyFlags(textDocument.uri)
//         }
//         fileWatcherChain = fileWatcherChain.then(async () => {
//           return vscode.commands.executeCommand('workbench.action.closeActiveEditor')
//         })
//       }
//     }
//   })
// }

async function wait(timeoutms: number): Promise<void> {
  return new Promise<void>((resolve) => setTimeout(() => resolve(), timeoutms))
}

let dirtyFiles: Set<string> = new Set()
let incomingFileChanges: Set<string> = new Set()

interface UpdateDirtyContentChange {
  type: 'UPDATE_DIRTY_CONTENT'
  path: string
  uri: Uri
}

function updateDirtyContentChange(path: string, uri: Uri): UpdateDirtyContentChange {
  return {
    type: 'UPDATE_DIRTY_CONTENT',
    path: path,
    uri: uri,
  }
}

interface DidChangeTextChange {
  type: 'DID_CHANGE_TEXT'
  path: string
  event: TextDocumentChangeEvent
}

function didChangeTextChange(path: string, event: TextDocumentChangeEvent): DidChangeTextChange {
  return {
    type: 'DID_CHANGE_TEXT',
    path: path,
    event: event,
  }
}

interface WillSaveText {
  type: 'WILL_SAVE_TEXT'
  path: string
  event: TextDocumentWillSaveEvent
}

function willSaveText(path: string, event: TextDocumentWillSaveEvent): WillSaveText {
  return {
    type: 'WILL_SAVE_TEXT',
    path: path,
    event: event,
  }
}

interface DidClose {
  type: 'DID_CLOSE'
  path: string
}

function didClose(path: string): DidClose {
  return {
    type: 'DID_CLOSE',
    path: path,
  }
}

type SubscriptionWork = UpdateDirtyContentChange | DidChangeTextChange | WillSaveText | DidClose
let pendingWork: Array<SubscriptionWork> = []

function minimisePendingWork(): void {
  let newPendingWork: Array<SubscriptionWork> = []
  for (const workItem of pendingWork) {
    const latestWorkItem = newPendingWork[newPendingWork.length - 1]
    if (
      latestWorkItem != null &&
      latestWorkItem.type === 'DID_CHANGE_TEXT' &&
      workItem.type === 'DID_CHANGE_TEXT'
    ) {
      // In this case `workItem` is for a subsequent change that happened after `latestWorkItem`.
      if (latestWorkItem.path === workItem.path) {
        newPendingWork.splice(newPendingWork.length - 1, 1, workItem)
      } else {
        newPendingWork.push(workItem)
      }
    } else {
      newPendingWork.push(workItem)
    }
  }
  pendingWork = newPendingWork
}

function doSubscriptionWork(work: SubscriptionWork) {
  switch (work.type) {
    case 'DID_CHANGE_TEXT': {
      const { path, event } = work
      if (event.document.isDirty) {
        // New unsaved change
        dirtyFiles.add(path)
      }
      if (dirtyFiles.has(path)) {
        if (incomingFileChanges.has(path)) {
          // This change actually came from Utopia, so we don't want to re-write it to the FS
          incomingFileChanges.delete(path)
        } else {
          const fullText = event.document.getText()
          writeFileUnsavedContentAsUTF8(path, fullText)
          const updatedFile = readFileAsUTF8(path)
          sendMessageToUtopia(vsCodeFileChange(path, updatedFile))
        }
      }
      break
    }
    case 'UPDATE_DIRTY_CONTENT': {
      const { path, uri } = work
      if (!incomingFileChanges.has(path)) {
        updateDirtyContent(uri)
      }
      break
    }
    case 'WILL_SAVE_TEXT': {
      const { path } = work
      dirtyFiles.delete(path)

      break
    }
    case 'DID_CLOSE': {
      const { path } = work
      if (dirtyFiles.has(path)) {
        // User decided to bin unsaved changes when closing the document
        clearFileUnsavedContent(path)
        dirtyFiles.delete(path)

        const updatedFile = readFileAsUTF8(path)
        sendMessageToUtopia(vsCodeFileChange(path, updatedFile))
      }

      break
    }
    default:
      const _exhaustiveCheck: never = work
      console.error(`Unhandled work type ${JSON.stringify(work)}`)
  }
}

const SUBSCRIPTION_POLLING_TIMEOUT = 100

function runPendingSubscriptionChanges() {
  minimisePendingWork()
  for (const work of pendingWork) {
    doSubscriptionWork(work)
  }

  pendingWork = []

  // FIXME should we still do it like this, or instead follow the pattern used by queueEvents?
  setTimeout(runPendingSubscriptionChanges, SUBSCRIPTION_POLLING_TIMEOUT)
}

setTimeout(runPendingSubscriptionChanges, SUBSCRIPTION_POLLING_TIMEOUT)

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
          const path = resource.path
          pendingWork.push(didChangeTextChange(path, event))
        }
      }
    }),
    vscode.workspace.onWillSaveTextDocument((event) => {
      if (isUtopiaDocument(event.document)) {
        const path = event.document.uri.path
        pendingWork.push(willSaveText(path, event))
        if (event.reason === vscode.TextDocumentSaveReason.Manual) {
          const formattedCode = applyPrettier(event.document.getText(), false).formatted
          event.waitUntil(Promise.resolve([new vscode.TextEdit(entireDocRange(), formattedCode)]))
        }
      }
    }),
    vscode.workspace.onDidCloseTextDocument((document) => {
      if (isUtopiaDocument(document)) {
        const path = document.uri.path
        pendingWork.push(didClose(path))
      }
    }),
    vscode.workspace.onDidOpenTextDocument((document) => {
      if (isUtopiaDocument(document)) {
        const path = document.uri.path
        pendingWork.push(updateDirtyContentChange(path, document.uri))
      }
    }),
  )
}

// TODO pass dynamicBlue color from the editor messages instead
const baseDecorationTypeColor = '#007aff'

const selectionDecorationType = vscode.window.createTextEditorDecorationType({
  borderColor: baseDecorationTypeColor,
  borderStyle: 'solid',
  borderWidth: '0 0 0 3px',
  isWholeLine: true,
})

const highlightDecorationType = vscode.window.createTextEditorDecorationType({
  borderColor: `${baseDecorationTypeColor}11`,
  borderStyle: 'solid',
  borderWidth: '0 0 0 3px',
  isWholeLine: true,
})

const allDecorationRangeTypes: Array<DecorationRangeType> = ['highlight', 'selection']

function getFollowSelectionEnabledConfig(): boolean {
  const followSelectionEnabledConfig = vscode.workspace
    .getConfiguration()
    .get(FollowSelectionConfigKey)
  return typeof followSelectionEnabledConfig === 'boolean' && followSelectionEnabledConfig
}

function getFullConfig(): UtopiaVSCodeConfig {
  return {
    followSelection: {
      enabled: getFollowSelectionEnabledConfig(),
    },
  }
}

let currentDecorations: Array<DecorationRange> = []
let currentSelection: BoundsInFile | null = null

function sendMessageToUtopia(message: FromVSCodeToUtopiaMessage): void {
  vscode.commands.executeCommand('utopia.toUtopiaMessage', message)
}

function initMessaging(
  context: vscode.ExtensionContext,
  workspaceRootUri: vscode.Uri,
  utopiaFS: UtopiaFSExtension,
): void {
  context.subscriptions.push(
    vscode.commands.registerCommand(
      'utopia.toVSCodeMessage',
      (message: FromUtopiaToVSCodeMessage) => {
        switch (message.type) {
          case 'INIT_PROJECT':
            const { projectContents, openFilePath } = message
            for (const projectFile of projectContents) {
              utopiaFS.writeProjectFile(projectFile)
              if (projectFile.type === 'PROJECT_TEXT_FILE' && projectFile.unsavedContent != null) {
                updateDirtyContent(vscode.Uri.joinPath(workspaceRootUri, projectFile.filePath))
              }
            }
            if (openFilePath != null) {
              openFile(vscode.Uri.joinPath(workspaceRootUri, openFilePath))
            } else {
              sendMessageToUtopia(clearLoadingScreen())
            }

            sendMessageToUtopia(vsCodeReady()) // FIXME do we need both?
            sendFullConfigToUtopia()
            sendMessageToUtopia(vsCodeBridgeReady())
            break
          case 'WRITE_PROJECT_FILE':
            const { projectFile } = message
            utopiaFS.writeProjectFile(projectFile)
            if (projectFile.type === 'PROJECT_TEXT_FILE') {
              const fileUri = vscode.Uri.joinPath(workspaceRootUri, projectFile.filePath)
              if (projectFile.unsavedContent == null) {
                clearDirtyFlags(fileUri)
              } else {
                updateDirtyContent(fileUri)
              }
            }
            break
          case 'DELETE_PATH':
            // FIXME Maybe we still need the watchForFileDeletions here to close the file
            utopiaFS.silentDelete(message.fullPath, { recursive: message.recursive })
            break
          case 'ENSURE_DIRECTORY_EXISTS':
            utopiaFS.ensureDirectoryExists(message.fullPath)
            break
          case 'OPEN_FILE':
            if (message.bounds != null) {
              revealRangeIfPossible(workspaceRootUri, {
                ...message.bounds,
                filePath: message.filePath,
              })
            } else {
              openFile(vscode.Uri.joinPath(workspaceRootUri, message.filePath))
            }
            break
          case 'UPDATE_DECORATIONS':
            currentDecorations = message.decorations
            updateDecorations(currentDecorations)
            break
          case 'SELECTED_ELEMENT_CHANGED':
            const followSelectionEnabled = getFollowSelectionEnabledConfig()
            const shouldFollowSelection =
              followSelectionEnabled &&
              (shouldFollowSelectionWithActiveFile() ||
                message.forceNavigation === 'force-navigation')
            if (shouldFollowSelection) {
              currentSelection = message.boundsInFile
              revealRangeIfPossible(workspaceRootUri, message.boundsInFile)
            }
            break
          case 'GET_UTOPIA_VSCODE_CONFIG':
            sendFullConfigToUtopia()
            break
          case 'SET_FOLLOW_SELECTION_CONFIG':
            vscode.workspace
              .getConfiguration()
              .update(
                FollowSelectionConfigKey,
                message.enabled,
                vscode.ConfigurationTarget.Workspace,
              )
            break
          case 'SET_VSCODE_THEME':
            vscode.workspace.getConfiguration().update('workbench.colorTheme', message.theme, true)
            break
          case 'UTOPIA_READY':
            sendMessageToUtopia(vsCodeReady())
            break
          default:
            const _exhaustiveCheck: never = message
            console.error(`Unhandled message type ${JSON.stringify(message)}`)
        }
      },
    ),
  )

  context.subscriptions.push(
    vscode.window.onDidChangeVisibleTextEditors(() => {
      updateDecorations(currentDecorations)
    }),
    vscode.window.onDidChangeTextEditorSelection((event) => {
      if (vscode.window.state.focused) {
        cursorPositionChanged(event)
      }
    }),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration(FollowSelectionConfigKey)) {
        sendFullConfigToUtopia()
      }
    }),
  )
}

function sendFullConfigToUtopia() {
  const fullConfig = getFullConfig()
  sendMessageToUtopia(utopiaVSCodeConfigValues(fullConfig))
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
  return vscode.commands.executeCommand('workbench.action.files.revertResource', resource)
}

async function updateDirtyContent(resource: vscode.Uri): Promise<void> {
  const filePath = resource.path
  const { unsavedContent } = readFileAsUTF8(filePath)
  if (unsavedContent != null) {
    incomingFileChanges.add(filePath)
    const workspaceEdit = new vscode.WorkspaceEdit()
    workspaceEdit.replace(resource, entireDocRange(), unsavedContent)
    const editApplied = await vscode.workspace.applyEdit(workspaceEdit)
    if (editApplied) {
      // Reset the highlights and selection
      updateDecorations(currentDecorations)
      if (currentSelection != null) {
        revealRangeIfPossibleInVisibleEditor(currentSelection)
      }
    } else {
      // Something went wrong applying the edit, so we clear the block on unsaved content fs writes
      incomingFileChanges.delete(filePath)
    }
  }
}

async function openFile(fileUri: vscode.Uri, retries: number = 5): Promise<boolean> {
  const filePath = fileUri.path
  const fileExists = exists(filePath)
  if (fileExists) {
    await vscode.commands.executeCommand('vscode.open', fileUri, { preserveFocus: true })
    sendMessageToUtopia(clearLoadingScreen())
    return true
  } else {
    // FIXME We shouldn't need this
    // Just in case the message is processed before the file has been written to the FS
    if (retries > 0) {
      await wait(100)
      return openFile(fileUri, retries - 1)
    } else {
      sendMessageToUtopia(clearLoadingScreen())
      return false
    }
  }
}

function cursorPositionChanged(event: vscode.TextEditorSelectionChangeEvent): void {
  try {
    const editor = event.textEditor
    const filename = editor.document.uri.path
    const position = editor.selection.active
    sendMessageToUtopia(editorCursorPositionChanged(filename, position.line, position.character))
  } catch (error) {
    console.error('cursorPositionChanged failure.', error)
  }
}

function revealRangeIfPossible(workspaceRootUri: vscode.Uri, boundsInFile: BoundsInFile) {
  const visibleEditor = vscode.window.visibleTextEditors.find(
    (editor) => editor.document.uri.path === boundsInFile.filePath,
  )
  if (visibleEditor == null) {
    const opened = openFile(vscode.Uri.joinPath(workspaceRootUri, boundsInFile.filePath))
    if (opened) {
      revealRangeIfPossibleInVisibleEditor(boundsInFile)
    }
  } else {
    revealRangeIfPossibleInVisibleEditor(boundsInFile)
  }
}

function revealRangeIfPossibleInVisibleEditor(boundsInFile: BoundsInFile): void {
  const visibleEditor = vscode.window.visibleTextEditors.find(
    (editor) => editor.document.uri.path === boundsInFile.filePath,
  )
  if (visibleEditor != null) {
    const rangeToReveal = getVSCodeRangeForScrolling(boundsInFile)
    const alreadyVisible = visibleEditor.visibleRanges.some((r) => r.contains(rangeToReveal))

    const selectionRange = getVSCodeRange(boundsInFile)
    const newSelection = new vscode.Selection(selectionRange.start, selectionRange.start) // selectionRange.end?
    if (!visibleEditor.selection.isEqual(newSelection)) {
      visibleEditor.selection = newSelection
    }

    const shouldReveal = !alreadyVisible
    if (shouldReveal) {
      visibleEditor.revealRange(
        rangeToReveal,
        vscode.TextEditorRevealType.InCenterIfOutsideViewport,
      )
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
  const { startLine, endLine, startCol, endCol } = bounds
  return new vscode.Range(
    new vscode.Position(startLine, startCol),
    new vscode.Position(endLine, endCol),
  )
}

function getVSCodeRangeForScrolling(bounds: Bounds): vscode.Range {
  const { startLine, endLine, startCol, endCol } = bounds
  return new vscode.Range(new vscode.Position(startLine, 0), new vscode.Position(endLine, endCol))
}

function updateDecorations(decorations: Array<DecorationRange>): void {
  const visibleEditors = vscode.window.visibleTextEditors
  const decorationsByFilenameAndType = getDecorationsByFilenameAndType(decorations)
  for (const visibleEditor of visibleEditors) {
    const filename = visibleEditor.document.uri.path
    // Default in the possible value we have received for a filename.
    const decorationsForFile = decorationsByFilenameAndType[filename] ?? {}
    for (const rangeType of allDecorationRangeTypes) {
      // Default in the possible value we have received for a range type.
      const decorationsForType = decorationsForFile[rangeType] ?? []
      // Construct the VS Code values and set those against the editor.
      const vsCodeDecorationType = getVSCodeDecorationType(rangeType)
      const vsCodeRanges = decorationsForType.map(getVSCodeRange)
      visibleEditor.setDecorations(vsCodeDecorationType, vsCodeRanges)
    }
  }
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

function shouldFollowSelectionWithActiveFile() {
  const { activeTextEditor } = vscode.window
  if (activeTextEditor == null) {
    return true
  }

  const filePath = activeTextEditor.document.uri.path
  const isJs =
    filePath.endsWith('.js') ||
    filePath.endsWith('.jsx') ||
    filePath.endsWith('.cjs') ||
    filePath.endsWith('.mjs')
  const isComponentDescriptor = filePath.startsWith('/utopia/') && filePath.endsWith('.utopia.js')
  return isJs && !isComponentDescriptor
}
