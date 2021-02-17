import * as vscode from 'vscode'
import { ensureDirectoryExists, waitUntilFSReady } from './browserfs-utils'
import { fromUtopiaURI, RootDir } from './path-utils'
import { UtopiaFSExtension } from './utopia-fs'

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const workspaceRootUri = vscode.workspace.workspaceFolders[0].uri
  const workspaceRootPath = fromUtopiaURI(workspaceRootUri)
  await waitUntilFSReady()
  await ensureDirectoryExists(RootDir)
  await ensureDirectoryExists(workspaceRootPath)
  const utopiaFS = new UtopiaFSExtension(workspaceRootPath)
  context.subscriptions.push(utopiaFS)
}
