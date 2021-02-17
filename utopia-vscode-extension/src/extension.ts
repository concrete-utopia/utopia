import * as vscode from 'vscode'
import { ensureDirectoryExists, initializeBrowserFS, RootDir } from 'utopia-vscode-common'
import { fromUtopiaURI } from './path-utils'
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
}
