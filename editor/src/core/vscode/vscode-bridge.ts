import {
  getProjectFileFromTree,
  isProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
  walkContentsTreeAsync,
  zipContentsTreeAsync,
} from '../../components/assets'
import { EditorDispatch } from '../../components/editor/action-types'
import {
  deleteFile,
  selectFromFileAndPosition,
  markVSCodeBridgeReady,
  updateFromCodeEditor,
} from '../../components/editor/actions/action-creators'
import { isDirectory } from '../model/project-file-utils'
import {
  initializeFS,
  writeFileAsUTF8,
  ensureDirectoryExists,
  watch,
  stopWatchingAll,
  stopPollingMailbox,
  readFileAsUTF8,
  clearBothMailboxes,
  initMailbox,
  openFileMessage,
  sendMessage,
  UtopiaInbox,
  FromVSCodeMessage,
  deletePath,
  DecorationRange,
  updateDecorationsMessage,
  appendToPath,
  stat,
  BoundsInFile,
  selectedElementChanged,
  parseFromVSCodeMessage,
} from 'utopia-vscode-common'
import { isTextFile, ProjectFile } from '../shared/project-file-types'
import { isBrowserEnvironment } from '../shared/utils'

const Scheme = 'utopia'
const RootDir = `/${Scheme}`

function toFSPath(projectPath: string): string {
  const fsPath = appendToPath(RootDir, projectPath)
  return fsPath
}

function fromFSPath(fsPath: string): string {
  const prefix = RootDir
  const prefixIndex = fsPath.indexOf(prefix)
  if (prefixIndex === 0) {
    const projectPath = fsPath.slice(prefix.length)
    return projectPath
  } else {
    throw new Error(`Invalid FS path: ${fsPath}`)
  }
}

async function writeProjectFile(
  projectID: string,
  projectPath: string,
  file: ProjectFile,
): Promise<void> {
  switch (file.type) {
    case 'DIRECTORY': {
      return ensureDirectoryExists(toFSPath(projectPath))
    }
    case 'TEXT_FILE': {
      const savedContent = file.lastSavedContents?.code ?? file.fileContents.code
      const unsavedContent = file.lastSavedContents == null ? null : file.fileContents.code
      return writeFileAsUTF8(toFSPath(projectPath), savedContent, unsavedContent)
    }
    case 'ASSET_FILE':
      return Promise.resolve()
    case 'IMAGE_FILE':
      return Promise.resolve()
  }
}

async function writeProjectContents(
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

function watchForChanges(projectID: string, dispatch: EditorDispatch): void {
  function onCreated(fsPath: string): void {
    stat(fsPath).then((fsStat) => {
      if (fsStat.type === 'FILE') {
        readFileAsUTF8(fsPath).then((fileContent) => {
          const action = updateFromCodeEditor(
            fromFSPath(fsPath),
            fileContent.content,
            fileContent.unsavedContent,
          )
          dispatch([action], 'everyone')
        })
      }
    })
  }
  function onModified(fsPath: string, modifiedBySelf: boolean): void {
    if (!modifiedBySelf) {
      onCreated(fsPath)
    }
  }
  function onDeleted(fsPath: string): void {
    const projectPath = fromFSPath(fsPath)
    const action = deleteFile(projectPath)
    dispatch([action], 'everyone')
  }
  watch(toFSPath('/'), true, onCreated, onModified, onDeleted)
}

let currentInit: Promise<void> = Promise.resolve()

export async function initVSCodeBridge(
  projectID: string,
  projectContents: ProjectContentTreeRoot,
  dispatch: EditorDispatch,
): Promise<void> {
  async function innerInit(): Promise<void> {
    dispatch([markVSCodeBridgeReady(false)], 'everyone')
    if (isBrowserEnvironment) {
      stopWatchingAll()
      stopPollingMailbox()
      await initializeFS(projectID, 'UTOPIA')
      await clearBothMailboxes()
      await writeProjectContents(projectID, projectContents)
      await initMailbox(UtopiaInbox, parseFromVSCodeMessage, (message: FromVSCodeMessage) => {
        dispatch(
          [selectFromFileAndPosition(message.filePath, message.line, message.column)],
          'everyone',
        )
      })
      watchForChanges(projectID, dispatch)
    }
    dispatch([markVSCodeBridgeReady(true)], 'everyone')
  }

  // Prevent multiple initialisations from driving over each other.
  currentInit = currentInit.then(innerInit)
}

export async function sendOpenFileMessage(filePath: string): Promise<void> {
  return sendMessage(openFileMessage(filePath))
}

export async function sendUpdateDecorationsMessage(
  decorations: Array<DecorationRange>,
): Promise<void> {
  return sendMessage(updateDecorationsMessage(decorations))
}

export async function sendSelectedElementChangedMessage(boundsInFile: BoundsInFile): Promise<void> {
  return sendMessage(selectedElementChanged(boundsInFile))
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
    const fsPath = toFSPath(fullPath)
    if (isProjectContentFile(firstContents)) {
      if (isProjectContentFile(secondContents)) {
        if (firstContents.content === secondContents.content) {
          // Do nothing, no change.
        } else if (isTextFile(firstContents.content) && isTextFile(secondContents.content)) {
          // We need to be careful around only sending this across if the text has been updated.
          const firstTextContent = firstContents.content
          const secondTextContent = secondContents.content
          if (
            firstTextContent.fileContents.code === secondTextContent.fileContents.code &&
            firstTextContent.lastSavedContents?.code === secondTextContent.lastSavedContents?.code
          ) {
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
    const fsPath = toFSPath(fullPath)
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
  if (isBrowserEnvironment) {
    await zipContentsTreeAsync(oldContents, newContents, onElement)
  }
}
