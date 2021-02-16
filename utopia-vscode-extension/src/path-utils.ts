import { Uri } from 'vscode'

export function toUtopiaURI(path: string): Uri {
  return Uri.parse(toUtopiaPath(path))
}

export function fromUtopiaURI(uri: Uri): string {
  return `${RootDir}${uri.path}`
}

