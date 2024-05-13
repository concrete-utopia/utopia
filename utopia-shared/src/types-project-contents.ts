/**
 * TODO: change this to the correct types
 */
type ParseSuccess = null
type ParsedTextFile = { type: 'UNPARSED' } // see editor/src/core/shared/project-file-types.ts

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
type ProjectContentTreeRoot = { [key: string]: ProjectContentsTree }

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
type ProjectContentsTree = ProjectContentDirectory | ProjectContentFile

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs
interface ProjectContentDirectory {
  type: 'PROJECT_CONTENT_DIRECTORY'
  fullPath: string
  directory: Directory
  children: ProjectContentTreeRoot
}

function projectContentDirectory(
  fullPath: string,
  dir: Directory,
  children: ProjectContentTreeRoot,
): ProjectContentDirectory {
  return {
    type: 'PROJECT_CONTENT_DIRECTORY',
    fullPath: fullPath,
    directory: dir,
    children: children,
  }
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
interface ProjectContentFile {
  type: 'PROJECT_CONTENT_FILE'
  fullPath: string
  content: Content
}

function projectContentFile(fullPath: string, content: Content): ProjectContentFile {
  return {
    type: 'PROJECT_CONTENT_FILE',
    fullPath: fullPath,
    content: content,
  }
}

export interface Directory {
  type: 'DIRECTORY'
}

export function directory(): Directory {
  return {
    type: 'DIRECTORY',
  }
}

type Content = ImageFile | AssetFile | TextFile

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ImageFile {
  type: 'IMAGE_FILE'
  imageType?: string
  base64?: string
  width?: number
  height?: number
  hash: number
  gitBlobSha?: string
}

export function imageFile(
  imageType: string | undefined,
  base64: string | undefined,
  width: number | undefined,
  height: number | undefined,
  hash: number,
  gitBlobSha: string | undefined,
): ImageFile {
  return {
    type: 'IMAGE_FILE',
    imageType: imageType,
    base64: base64,
    width: width,
    height: height,
    hash: hash,
    gitBlobSha: gitBlobSha,
  }
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface AssetFile {
  type: 'ASSET_FILE'
  base64?: string
  gitBlobSha?: string
}

export function assetFile(base64: string | undefined, gitBlobSha: string | undefined): AssetFile {
  return {
    type: 'ASSET_FILE',
    base64: base64,
    gitBlobSha: gitBlobSha,
  }
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
interface TextFile {
  type: 'TEXT_FILE'
  fileContents: TextFileContents
  lastSavedContents: TextFileContents | null // it is null when the file is saved
  lastParseSuccess: ParseSuccess | null
  versionNumber: number
}

function textFile(
  fileContents: TextFileContents,
  lastSavedContents: TextFileContents | null,
  lastParseSuccess: ParseSuccess | null,
  versionNumber: number,
): TextFile {
  return {
    type: 'TEXT_FILE',
    fileContents: fileContents,
    lastSavedContents: lastSavedContents,
    lastParseSuccess: lastParseSuccess,
    versionNumber: versionNumber,
  }
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
interface TextFileContents {
  code: string
  parsed: ParsedTextFile
  revisionsState: RevisionsStateType
}

function textFileContents(
  code: string,
  parsed: ParsedTextFile,
  revisionsState: RevisionsStateType,
): TextFileContents {
  return {
    code: code,
    parsed: parsed,
    revisionsState: revisionsState,
  }
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type RevisionsStateType =
  | 'PARSED_AHEAD'
  | 'CODE_AHEAD'
  | 'BOTH_MATCH'
  | 'CODE_AHEAD_BUT_PLEASE_TELL_VSCODE_ABOUT_IT'

export const RevisionsState = {
  ParsedAhead: 'PARSED_AHEAD',
  CodeAhead: 'CODE_AHEAD',
  BothMatch: 'BOTH_MATCH',
  CodeAheadButPleaseTellVSCodeAboutIt: 'CODE_AHEAD_BUT_PLEASE_TELL_VSCODE_ABOUT_IT',
} as const
