import * as vscode from 'vscode'
import { ensureDirectoryExists, waitUntilFSReady } from './browserfs-utils'
import { RootDir } from './path-utils'
import { UtopiaFSExtension } from './utopia-fs'

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  await waitUntilFSReady()
  await ensureDirectoryExists(RootDir)
  const utopiaFS = new UtopiaFSExtension()
  context.subscriptions.push(utopiaFS)
}
