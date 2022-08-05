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
  applyPrettier,
  UtopiaVSCodeConfig,
  utopiaVSCodeConfigValues,
  vsCodeReady,
  clearLoadingScreen,
} from 'utopia-vscode-common'
import { UtopiaFSExtension } from './utopia-fs'
import { fromUtopiaURI } from './path-utils'
import { TextDocumentChangeEvent, TextDocumentWillSaveEvent, Uri } from 'vscode'

const FollowSelectionConfigKey = 'utopia.editor.followSelection.enabled'

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const workspaceRootUri = vscode.workspace.workspaceFolders[0].uri
  const projectID = workspaceRootUri.scheme
  /* eslint-disable-next-line react-hooks/rules-of-hooks */
  useFileSystemProviderErrors(projectID)

  await initFS(projectID)
  const utopiaFS = initUtopiaFSProvider(projectID, context)
  initMessaging(context, workspaceRootUri)

  watchForUnsavedContentChangesFromFS(utopiaFS)
  watchForChangesFromVSCode(context, projectID)

  // Send a VSCodeReady message on activation as this might be triggered by an iframe reload,
  // meaning no new UtopiaReady message will have been sent
  sendMessage(vsCodeReady())

  watchForFileDeletions()
}

function watchForFileDeletions() {
  let fileWatcherChain: Promise<void> = Promise.resolve()
  const fileWatcher = vscode.workspace.createFileSystemWatcher('**/*')
  fileWatcher.onDidDelete(async (deletedFile) => {
    for (const textDocument of vscode.workspace.textDocuments) {
      if (textDocument.uri.fsPath === deletedFile.fsPath) {
        fileWatcherChain = fileWatcherChain.then(async () => {
          await vscode.window.showTextDocument(textDocument)
          return Promise.resolve()
        })
        if (textDocument.isDirty) {
          await clearDirtyFlags(textDocument.uri)
        }
        fileWatcherChain = fileWatcherChain.then(async () => {
          return vscode.commands.executeCommand('workbench.action.closeActiveEditor')
        })
      }
    }
  })
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

async function doSubscriptionWork(work: SubscriptionWork): Promise<void> {
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
          await writeFileUnsavedContentAsUTF8(path, fullText)
        }
      }
      break
    }
    case 'UPDATE_DIRTY_CONTENT': {
      const { path, uri } = work
      if (!incomingFileChanges.has(path)) {
        await updateDirtyContent(uri)
      }
      break
    }
    case 'WILL_SAVE_TEXT': {
      const { path, event } = work
      dirtyFiles.delete(path)

      break
    }
    case 'DID_CLOSE': {
      const { path } = work
      if (dirtyFiles.has(path)) {
        // User decided to bin unsaved changes when closing the document
        clearFileUnsavedContent(path)
        dirtyFiles.delete(path)
      }

      break
    }
    default:
      const _exhaustiveCheck: never = work
      console.error(`Unhandled work type ${JSON.stringify(work)}`)
  }
}

const SUBSCRIPTION_POLLING_TIMEOUT = 100

async function runPendingSubscriptionChanges(): Promise<void> {
  minimisePendingWork()
  for (const work of pendingWork) {
    await doSubscriptionWork(work)
  }

  pendingWork = []
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
          const path = fromUtopiaURI(resource)
          pendingWork.push(didChangeTextChange(path, event))
        }
      }
    }),
    vscode.workspace.onWillSaveTextDocument((event) => {
      if (isUtopiaDocument(event.document)) {
        const path = fromUtopiaURI(event.document.uri)
        pendingWork.push(willSaveText(path, event))
        if (event.reason === vscode.TextDocumentSaveReason.Manual) {
          const formattedCode = applyPrettier(event.document.getText(), false).formatted
          event.waitUntil(Promise.resolve([new vscode.TextEdit(entireDocRange(), formattedCode)]))
        }
      }
    }),
    vscode.workspace.onDidCloseTextDocument((document) => {
      if (isUtopiaDocument(document)) {
        const path = fromUtopiaURI(document.uri)
        pendingWork.push(didClose(path))
      }
    }),
    vscode.workspace.onDidOpenTextDocument((document) => {
      if (isUtopiaDocument(document)) {
        const path = fromUtopiaURI(document.uri)
        pendingWork.push(updateDirtyContentChange(path, document.uri))
      }
    }),
  )
}

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

function initMessaging(context: vscode.ExtensionContext, workspaceRootUri: vscode.Uri): void {
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
        const followSelectionEnabled = getFollowSelectionEnabledConfig()
        if (followSelectionEnabled) {
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
          .update(FollowSelectionConfigKey, message.enabled, vscode.ConfigurationTarget.Workspace)
        break
      case 'ACCUMULATED_TO_VSCODE_MESSAGE':
        for (const innerMessage of message.messages) {
          handleMessage(innerMessage)
        }
        break
      case 'UTOPIA_READY':
        sendMessage(vsCodeReady())
        break
      default:
        const _exhaustiveCheck: never = message
        console.error(`Unhandled message type ${JSON.stringify(message)}`)
    }
  }

  initMailbox(VSCodeInbox, parseToVSCodeMessage, handleMessage)

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

function sendFullConfigToUtopia(): Promise<void> {
  const fullConfig = getFullConfig()
  return sendMessage(utopiaVSCodeConfigValues(fullConfig))
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
  const filePath = fromUtopiaURI(resource)
  const { unsavedContent } = await readFileAsUTF8(filePath)
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
  const filePath = fromUtopiaURI(fileUri)
  const fileExists = await exists(filePath)
  if (fileExists) {
    await vscode.commands.executeCommand('vscode.open', fileUri, { preserveFocus: true })
    sendMessage(clearLoadingScreen())
    return true
  } else {
    // Just in case the message is processed before the file has been written to the FS
    if (retries > 0) {
      await new Promise<void>((resolve) => setTimeout(() => resolve(), 100))
      return openFile(fileUri, retries - 1)
    } else {
      sendMessage(clearLoadingScreen())
      return false
    }
  }
}

function cursorPositionChanged(event: vscode.TextEditorSelectionChangeEvent): void {
  try {
    const editor = event.textEditor
    const filename = editor.document.uri.path
    const position = editor.selection.active
    sendMessage(editorCursorPositionChanged(filename, position.line, position.character))
  } catch (error) {
    console.error('cursorPositionChanged failure.', error)
  }
}

async function revealRangeIfPossible(
  workspaceRootUri: vscode.Uri,
  boundsInFile: BoundsInFile,
  forceIfFocused: boolean = false,
): Promise<void> {
  const focused = vscode.window.state.focused
  if (forceIfFocused || !focused) {
    const visibleEditor = vscode.window.visibleTextEditors.find(
      (editor) => editor.document.uri.path === boundsInFile.filePath,
    )
    if (visibleEditor == null) {
      const opened = await openFile(vscode.Uri.joinPath(workspaceRootUri, boundsInFile.filePath))
      if (opened) {
        revealRangeIfPossibleInVisibleEditor(boundsInFile)
      }
    } else {
      revealRangeIfPossibleInVisibleEditor(boundsInFile)
    }
  }
}

function revealRangeIfPossibleInVisibleEditor(boundsInFile: BoundsInFile): void {
  const visibleEditor = vscode.window.visibleTextEditors.find(
    (editor) => editor.document.uri.path === boundsInFile.filePath,
  )
  if (visibleEditor != null) {
    const rangeToReveal = getVSCodeRangeForScrolling(boundsInFile)
    const alreadyVisible = visibleEditor.visibleRanges.some((r) =>
      r.contains(visibleEditor.selection),
    )

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
    case 'FS_UNAVAILABLE':
      return vscode.FileSystemError.Unavailable(path)
    default:
      const _exhaustiveCheck: never = code
      throw new Error(`Unhandled FS Error ${JSON.stringify(error)}`)
  }
}
