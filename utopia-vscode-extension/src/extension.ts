import * as vscode from 'vscode'
import {
  ensureDirectoryExists,
  initializeBrowserFS,
  RootDir,
  initMailbox,
  isOpenFileMessage,
  VSCodeInbox,
  UtopiaVSCodeMessage,
} from 'utopia-vscode-common'
import { fromUtopiaURI, toUtopiaURI } from './path-utils'
import { UtopiaFSExtension } from './utopia-fs'
import { useFileSystemProviderErrors } from './browserfs-utils'

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  useFileSystemProviderErrors()
  const workspaceRootUri = vscode.workspace.workspaceFolders[0].uri
  const workspaceRootPath = fromUtopiaURI(workspaceRootUri)
  await initializeBrowserFS()
  await ensureDirectoryExists(RootDir)
  await ensureDirectoryExists(workspaceRootPath)
  const utopiaFS = new UtopiaFSExtension(workspaceRootPath)
  context.subscriptions.push(utopiaFS)
  initMessaging(workspaceRootUri)
}

function initMessaging(workspaceRootUri: vscode.Uri): void {
  function handleMessage(message: UtopiaVSCodeMessage): void {
    if (isOpenFileMessage(message)) {
      vscode.commands.executeCommand(
        'vscode.open',
        vscode.Uri.joinPath(workspaceRootUri, message.filePath),
      )
    } else {
      console.log(`Unhandled message type ${message.type}`)
    }
  }

  initMailbox(VSCodeInbox, handleMessage)
}
