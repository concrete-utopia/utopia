/**
 * The following types are copied from editor/src/core/shared/project-file-types.ts,
 * with some defaults for convenience since they are not used here.
 */

export type ProjectContentTreeRoot = { [key: string]: ProjectContentsTree }

export type ProjectContentsTree = ProjectContentDirectory | ProjectContentFile

export interface ProjectContentDirectory {
  type: 'PROJECT_CONTENT_DIRECTORY'
  fullPath: string
  directory: Directory
  children: ProjectContentTreeRoot
}

export function projectContentDirectory(fullPath: string): ProjectContentDirectory {
  return {
    type: 'PROJECT_CONTENT_DIRECTORY',
    fullPath: fullPath,
    directory: { type: 'DIRECTORY' },
    children: {},
  }
}

export interface Directory {
  type: 'DIRECTORY'
}

export type Content = TextFile | ImageFile | AssetFile

export interface ProjectContentFile {
  type: 'PROJECT_CONTENT_FILE'
  fullPath: string
  content: Content
}

export function projectContentFile(fullPath: string, content: Content): ProjectContentFile {
  return {
    type: 'PROJECT_CONTENT_FILE',
    fullPath: fullPath,
    content: content,
  }
}

export interface TextFile {
  type: 'TEXT_FILE'
  fileContents: TextFileContents
  lastSavedContents: null // see editor/src/core/shared/project-file-types.ts
  lastParseSuccess: null // see editor/src/core/shared/project-file-types.ts
  versionNumber: 0 // see editor/src/core/shared/project-file-types.ts
}

export function textFile(code: string): TextFile {
  return {
    type: 'TEXT_FILE',
    fileContents: { code: code, parsed: { type: 'UNPARSED' }, revisionsState: 'CODE_AHEAD' },
    lastParseSuccess: null,
    lastSavedContents: null,
    versionNumber: 0,
  }
}

export interface TextFileContents {
  code: string
  parsed: { type: 'UNPARSED' } // see editor/src/core/shared/project-file-types.ts
  revisionsState: 'CODE_AHEAD' // see editor/src/core/shared/project-file-types.ts
}

export interface ImageFile {
  type: 'IMAGE_FILE'
  imageType: null // see editor/src/core/shared/project-file-types.ts
  base64: null // see editor/src/core/shared/project-file-types.ts
  hash: number
  gitBlobSha: string
}

export function imageFile(params: { hash: number; gitBlobSha: string }): ImageFile {
  return {
    type: 'IMAGE_FILE',
    imageType: null,
    base64: null,
    hash: params.hash,
    gitBlobSha: params.gitBlobSha,
  }
}

export interface AssetFile {
  type: 'ASSET_FILE'
  base64: null // see editor/src/core/shared/project-file-types.ts
  gitBlobSha: string
}

export function assetFile(params: { gitBlobSha: string }): AssetFile {
  return {
    type: 'ASSET_FILE',
    base64: null,
    gitBlobSha: params.gitBlobSha,
  }
}
