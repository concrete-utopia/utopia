import * as vscode from 'vscode'
import { UtopiaFSExtension } from './utopia-fs'

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const utopiaFS = new UtopiaFSExtension()
  await utopiaFS.init()
  context.subscriptions.push(utopiaFS)
}
