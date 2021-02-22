import { Uri } from 'vscode'
import { RootDir, toUtopiaPath } from 'utopia-vscode-common'

export function toUtopiaURI(projectID: string, path: string): Uri {
  return Uri.parse(toUtopiaPath(projectID, path))
}

export function fromUtopiaURI(uri: Uri): string {
  return `${RootDir}${uri.path}`
}
