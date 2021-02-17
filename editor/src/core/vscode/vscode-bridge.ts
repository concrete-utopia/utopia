import { ProjectContentTreeRoot, walkContentsTree } from 'src/components/assets'
import { EditorDispatch } from 'src/components/editor/action-types'
import { updateFile } from 'src/components/editor/actions/action-creators'
import { isDirectory } from '../model/project-file-utils'
import { isTextFile, ProjectFile } from '../shared/project-file-types'
import { writeFile, ensureDirectoryExists, watch } from 'utopia-vscode-common'

const Scheme = 'utopia'
const RootDir = `/${Scheme}`

export function toFSPath(projectID: string, path: string): string {
  const fsPath = `${RootDir}/${projectID}${path}`
  return fsPath
}

export function fromFSPath(projectID: string, path: string): string {
  const prefix = `${RootDir}/${projectID}`
  const prefixIndex = path.indexOf(prefix)
  if (prefixIndex === 0) {
    const projectPath = path.slice(prefix.length)
    return projectPath
  } else {
    throw new Error(`Invalid FS path: ${path}`)
  }
}

export function writeProjectFile(projectID: string, path: string, file: ProjectFile): void {
  switch (file.type) {
    case 'DIRECTORY':
      {
        ensureDirectoryExists(toFSPath(projectID, path))
      }
      break
    case 'TEXT_FILE':
      {
        const fileContents = Buffer.from(file.fileContents.code, 'utf-8')
        writeFile(toFSPath(projectID, path), fileContents)
      }
      break
    case 'ASSET_FILE':
      throw new Error(`Can't handle asset file at ${path}`)
    case 'IMAGE_FILE':
      throw new Error(`Can't handle image file at ${path}`)
  }
}

export function writeProjectContents(
  projectID: string,
  projectContents: ProjectContentTreeRoot,
): void {
  walkContentsTree(projectContents, (fullPath, file) => {
    if (isTextFile(file) || isDirectory(file)) {
      writeProjectFile(projectID, fullPath, file)
    }
  })
}

export function watchForChanges(projectID: string, dispatch: EditorDispatch): void {
  // eslint-disable-next-line @typescript-eslint/no-empty-function
  function onCreated(path: string): void {}
  // eslint-disable-next-line @typescript-eslint/no-empty-function
  function onModified(path: string): void {}
  // eslint-disable-next-line @typescript-eslint/no-empty-function
  function onDeleted(path: string): void {}
  watch(toFSPath(projectID, '/'), true, onCreated, onModified, onDeleted)
}
