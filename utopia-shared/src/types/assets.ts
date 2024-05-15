import type { ImageFile, ProjectFile, Directory, TextFile, AssetFile } from './project-file-types'

export interface AssetFileWithFileName {
  fileName: string
  file: ImageFile | AssetFile
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type ProjectContentTreeRoot = { [key: string]: ProjectContentsTree }

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs
export interface ProjectContentDirectory {
  type: 'PROJECT_CONTENT_DIRECTORY'
  fullPath: string
  directory: Directory
  children: ProjectContentTreeRoot
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export interface ProjectContentFile {
  type: 'PROJECT_CONTENT_FILE'
  fullPath: string
  content: TextFile | ImageFile | AssetFile
}

// Ensure this is kept up to date with clientmodel/lib/src/Utopia/ClientModel.hs.
export type ProjectContentsTree = ProjectContentDirectory | ProjectContentFile

export interface PathAndFileEntry {
  fullPath: string
  file: ProjectFile
}
