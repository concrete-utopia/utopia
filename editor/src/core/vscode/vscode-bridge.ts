import { ProjectContentTreeRoot, walkContentsTree } from '../../components/assets'
import { EditorDispatch } from '../../components/editor/action-types'
import { deleteFile, updateFromWorker } from '../../components/editor/actions/action-creators'
import { isDirectory } from '../model/project-file-utils'
import { writeFile, ensureDirectoryExists, watch } from 'utopia-vscode-common'
import {
  isTextFile,
  ProjectFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../shared/project-file-types'

const Scheme = 'utopia'
const RootDir = `/${Scheme}`

export function toFSPath(projectID: string, projectPath: string): string {
  const fsPath = `${RootDir}/${projectID}${projectPath}`
  return fsPath
}

export function fromFSPath(projectID: string, fsPath: string): string {
  const prefix = `${RootDir}/${projectID}`
  const prefixIndex = fsPath.indexOf(prefix)
  if (prefixIndex === 0) {
    const projectPath = fsPath.slice(prefix.length)
    return projectPath
  } else {
    throw new Error(`Invalid FS path: ${fsPath}`)
  }
}

export function writeProjectFile(projectID: string, projectPath: string, file: ProjectFile): void {
  switch (file.type) {
    case 'DIRECTORY':
      {
        ensureDirectoryExists(toFSPath(projectID, projectPath))
      }
      break
    case 'TEXT_FILE':
      {
        const fileContents = Buffer.from(file.fileContents.code, 'utf-8')
        writeFile(toFSPath(projectID, projectPath), fileContents)
      }
      break
    case 'ASSET_FILE':
      throw new Error(`Can't handle asset file at ${projectPath}`)
    case 'IMAGE_FILE':
      throw new Error(`Can't handle image file at ${projectPath}`)
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
  function onCreated(fsPath: string): void {
    readFileWithEncoding(fsPath, 'utf-8').then((text) => {
      const file = textFile(
        textFileContents(text, unparsed, RevisionsState.CodeAhead),
        null,
        Date.now(),
      )
      const action = updateFromWorker(fromFSPath(projectID, fsPath), file, 'Code')
      dispatch([action], 'everyone')
    })
  }
  function onModified(fsPath: string): void {
    onCreated(fsPath)
  }
  function onDeleted(fsPath: string): void {
    const projectPath = fromFSPath(projectID, fsPath)
    const action = deleteFile(projectPath)
    dispatch([action], 'everyone')
  }
  watch(toFSPath(projectID, '/'), true, onCreated, onModified, onDeleted)
}
