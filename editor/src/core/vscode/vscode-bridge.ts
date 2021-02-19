import {
  getProjectFileFromTree,
  isProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
  walkContentsTreeAsync,
  zipContentsTreeAsync,
} from '../../components/assets'
import { EditorDispatch } from '../../components/editor/action-types'
import { deleteFile, updateFromCodeEditor } from '../../components/editor/actions/action-creators'
import { isDirectory } from '../model/project-file-utils'
import {
  initializeBrowserFS,
  writeFileWithEncoding,
  ensureDirectoryExists,
  watch,
  readFileWithEncoding,
  initMailbox,
  openFileMessage,
  sendMessage,
  UtopiaInbox,
  UtopiaVSCodeMessage,
  deletePath,
} from 'utopia-vscode-common'
import { isTextFile, ProjectFile } from '../shared/project-file-types'

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

export async function writeProjectFile(
  projectID: string,
  projectPath: string,
  file: ProjectFile,
): Promise<void> {
  switch (file.type) {
    case 'DIRECTORY': {
      return ensureDirectoryExists(toFSPath(projectID, projectPath))
    }
    case 'TEXT_FILE': {
      return writeFileWithEncoding(toFSPath(projectID, projectPath), file.fileContents.code)
    }
    case 'ASSET_FILE':
      return Promise.resolve()
    case 'IMAGE_FILE':
      return Promise.resolve()
  }
}

export async function writeProjectContents(
  projectID: string,
  projectContents: ProjectContentTreeRoot,
): Promise<void> {
  await walkContentsTreeAsync(projectContents, (fullPath, file) => {
    if (isTextFile(file) || isDirectory(file)) {
      return writeProjectFile(projectID, fullPath, file)
    } else {
      return Promise.resolve()
    }
  })
}

export function watchForChanges(projectID: string, dispatch: EditorDispatch): void {
  function onCreated(fsPath: string): void {
    readFileWithEncoding(fsPath).then((text) => {
      const action = updateFromCodeEditor(fromFSPath(projectID, fsPath), text)
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

export async function initVSCodeBridge(
  projectID: string,
  projectContents: ProjectContentTreeRoot,
  dispatch: EditorDispatch,
): Promise<void> {
  await initializeBrowserFS()
  initMailbox(UtopiaInbox, (message: UtopiaVSCodeMessage) => {
    /* Do nothing */
  })
  await writeProjectContents(projectID, projectContents)
  watchForChanges(projectID, dispatch)
}

export async function sendOpenFileMessage(filePath: string): Promise<void> {
  return sendMessage(openFileMessage(filePath))
}

export async function applyProjectContentChanges(
  projectID: string,
  oldContents: ProjectContentTreeRoot,
  newContents: ProjectContentTreeRoot,
): Promise<void> {
  async function applyChanges(
    fullPath: string,
    firstContents: ProjectContentsTree,
    secondContents: ProjectContentsTree,
  ): Promise<boolean> {
    const fsPath = toFSPath(projectID, fullPath)
    if (isProjectContentFile(firstContents)) {
      if (isProjectContentFile(secondContents)) {
        if (firstContents.content === secondContents.content) {
          // Do nothing, no change.
        } else if (isTextFile(firstContents.content) && isTextFile(secondContents.content)) {
          // We need to be careful around only sending this across if the text has been updated.
          const firstTextContent = firstContents.content
          const secondTextContent = secondContents.content
          if (firstTextContent.fileContents.code === secondTextContent.fileContents.code) {
            // Do nothing, no change.
          } else {
            await writeProjectFile(projectID, fullPath, getProjectFileFromTree(secondContents))
          }
        } else {
          await writeProjectFile(projectID, fullPath, getProjectFileFromTree(secondContents))
        }
      } else {
        await deletePath(fsPath, true)
        await ensureDirectoryExists(fsPath)
      }
    } else {
      if (isProjectContentFile(secondContents)) {
        await deletePath(fsPath, true)
        await writeProjectFile(projectID, fullPath, getProjectFileFromTree(secondContents))
      } else {
        // Do nothing, both sides are a directory.
      }
    }

    return Promise.resolve(true)
  }

  async function onElement(
    fullPath: string,
    firstContents: ProjectContentsTree | null,
    secondContents: ProjectContentsTree | null,
  ): Promise<boolean> {
    const fsPath = toFSPath(projectID, fullPath)
    if (firstContents == null) {
      if (secondContents == null) {
        // Do nothing, nothing exists.
        return Promise.resolve(false)
      } else {
        await writeProjectFile(projectID, fullPath, getProjectFileFromTree(secondContents))
        return Promise.resolve(true)
      }
    } else {
      if (secondContents == null) {
        // Value does not exist, delete it.
        await deletePath(fsPath, true)
        return Promise.resolve(false)
      } else {
        if (firstContents === secondContents) {
          // Same value, stop here.
          return Promise.resolve(false)
        } else {
          return applyChanges(fullPath, firstContents, secondContents)
        }
      }
    }
  }
  await zipContentsTreeAsync(oldContents, newContents, onElement)
}
