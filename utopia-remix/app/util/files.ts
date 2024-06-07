import * as MimeTypes from 'mime-types'
import path from 'path'
import textExtensions from 'textextensions'

export type FileType = 'ASSET_FILE' | 'TEXT_FILE' | 'IMAGE_FILE'

// Ported from editor/src/core/model/project-file-utils.ts
export function fileTypeFromFileName(filename: string | null): FileType | null {
  if (filename == null) {
    return null
  }
  if (filename.endsWith('.svg')) {
    return 'ASSET_FILE'
  }
  if (isText(filename)) {
    return 'TEXT_FILE'
  }

  const mimeType = mimeTypeLookup(filename)
  if (mimeType === false) {
    return 'TEXT_FILE' // FIXME This is definitely not a safe assumption
  } else if (mimeType.startsWith('image/')) {
    return 'IMAGE_FILE'
  } else {
    return 'ASSET_FILE'
  }
}

// Ported from editor/src/core/model/project-file-utils.ts
export function mimeTypeLookup(filename: string): string | false {
  if (filename.endsWith('.ts')) {
    return 'application/x-typescript'
  } else if (filename.endsWith('.tsx')) {
    return 'application/x-tsx'
  } else if (filename.endsWith('.jsx')) {
    return 'application/x-jsx'
  } else {
    return MimeTypes.lookup(filename)
  }
}

// Slim port of https://github.com/bevry/istextorbinary which has TypeScript import issues.
export function isText(filename: string): boolean {
  if (filename) {
    const parts = path.basename(filename).split('.').reverse()
    for (const extension of parts) {
      if (textExtensions.indexOf(extension) !== -1) {
        return true
      }
    }
  }
  return false
}
