import { ApiError } from 'browserfs/dist/node/core/api_error'
import { FileSystemError } from 'vscode'
import { toUtopiaPath } from './path-utils'

export function useFileSystemProviderErrors(): void {
  setErrorHandler(toFileSystemProviderError)
}

function toFileSystemProviderError(error: ApiError): FileSystemError {
  const { path: unadjustedPath, code } = error
  const path = toUtopiaPath(unadjustedPath)
  switch (code) {
    case 'ENOENT':
      return FileSystemError.FileNotFound(path)
    case 'EISDIR':
      return FileSystemError.FileIsADirectory(path)
    case 'ENOTDIR':
      return FileSystemError.FileNotADirectory(path)
    case 'EEXIST':
      return FileSystemError.FileExists(path)
    case 'EPERM':
    case 'EACCES':
      return FileSystemError.NoPermissions(path)
    default:
      return new FileSystemError(error.message)
  }
}
