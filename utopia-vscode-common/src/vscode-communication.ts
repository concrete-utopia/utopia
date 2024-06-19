// This file exists so that the extension can communicate with the Utopia editor

import type { FSUser } from './fs/fs-types'
import {
  deletePath,
  ensureDirectoryExists,
  initializeFS,
  readFileAsUTF8,
  stat,
  stopWatchingAll,
  watch,
  writeFileAsUTF8,
} from './fs/fs-utils'
import {
  clearBothMailboxes,
  initMailbox,
  sendMessage,
  stopPollingMailbox,
  UtopiaInbox,
} from './mailbox'
import type { FromVSCodeMessage } from './messages'
import {
  clearLoadingScreen,
  getUtopiaVSCodeConfig,
  openFileMessage,
  parseFromVSCodeMessage,
  utopiaReady,
} from './messages'
import { appendToPath } from './path-utils'
import type { ProjectFile } from './window-messages'
import {
  fromVSCodeExtensionMessage,
  indexedDBFailure,
  isDeletePathChange,
  isEnsureDirectoryExistsChange,
  isInitProject,
  isToVSCodeExtensionMessage,
  isWriteProjectFileChange,
  messageListenersReady,
  vsCodeBridgeReady,
  vsCodeFileChange,
  vsCodeFileDelete,
} from './window-messages'

const Scheme = 'utopia'
const RootDir = `/${Scheme}`
const UtopiaFSUser: FSUser = 'UTOPIA'

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

async function writeProjectFile(projectFile: ProjectFile): Promise<void> {
  switch (projectFile.type) {
    case 'PROJECT_DIRECTORY': {
      const { filePath: projectPath } = projectFile
      return ensureDirectoryExists(toFSPath(projectPath))
    }
    case 'PROJECT_TEXT_FILE': {
      const { filePath: projectPath, savedContent, unsavedContent } = projectFile
      const filePath = toFSPath(projectPath)
      const alreadyExistingFile = await readFileAsUTF8(filePath).catch((_) => null)
      const fileDiffers =
        alreadyExistingFile == null ||
        alreadyExistingFile.content !== savedContent ||
        alreadyExistingFile.unsavedContent !== unsavedContent
      if (fileDiffers) {
        // Avoid pushing a file to the file system if the content hasn't changed.
        return writeFileAsUTF8(filePath, savedContent, unsavedContent)
      }
      return
    }
    default:
      const _exhaustiveCheck: never = projectFile
      throw new Error(`Invalid file projectFile type ${projectFile}`)
  }
}

function watchForChanges(): void {
  function onCreated(fsPath: string): void {
    void stat(fsPath).then((fsStat) => {
      if (fsStat.type === 'FILE' && fsStat.sourceOfLastChange !== UtopiaFSUser) {
        void readFileAsUTF8(fsPath).then((fileContent) => {
          const filePath = fromFSPath(fsPath)
          window.top?.postMessage(vsCodeFileChange(filePath, fileContent), '*')
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
    const filePath = fromFSPath(fsPath)
    window.top?.postMessage(vsCodeFileDelete(filePath), '*')
  }
  function onIndexedDBFailure(): void {
    window.top?.postMessage(indexedDBFailure(), '*')
  }

  void watch(toFSPath('/'), true, onCreated, onModified, onDeleted, onIndexedDBFailure)
}

let currentInit: Promise<void> = Promise.resolve()

async function initIndexedDBBridge(
  vsCodeSessionID: string,
  projectContents: Array<ProjectFile>,
  openFilePath: string | null,
): Promise<void> {
  async function innerInit(): Promise<void> {
    stopWatchingAll()
    stopPollingMailbox()
    await initializeFS(vsCodeSessionID, UtopiaFSUser)
    await ensureDirectoryExists(RootDir)
    await clearBothMailboxes()
    for (const projectFile of projectContents) {
      await writeProjectFile(projectFile)
    }
    await initMailbox(UtopiaInbox, parseFromVSCodeMessage, (message: FromVSCodeMessage) => {
      window.top?.postMessage(fromVSCodeExtensionMessage(message), '*')
    })
    await sendMessage(utopiaReady())
    await sendMessage(getUtopiaVSCodeConfig())
    watchForChanges()
    if (openFilePath != null) {
      await sendMessage(openFileMessage(openFilePath, null))
    } else {
      window.top?.postMessage(fromVSCodeExtensionMessage(clearLoadingScreen()), '*')
    }

    window.top?.postMessage(vsCodeBridgeReady(), '*')
  }

  // Prevent multiple initialisations from driving over each other.
  currentInit = currentInit.then(innerInit)
}

// Chain off of the previous one to ensure the ordering of changes is maintained.
let applyProjectChangesCoordinator: Promise<void> = Promise.resolve()

export function setupVSCodeEventListenersForProject(vsCodeSessionID: string) {
  let intervalID: number | null = null
  window.addEventListener('message', (messageEvent: MessageEvent) => {
    const { data } = messageEvent
    if (isInitProject(data)) {
      if (intervalID != null) {
        window.clearInterval(intervalID)
      }
      initIndexedDBBridge(vsCodeSessionID, data.projectContents, data.openFilePath)
    } else if (isDeletePathChange(data)) {
      applyProjectChangesCoordinator = applyProjectChangesCoordinator.then(async () => {
        await deletePath(toFSPath(data.fullPath), data.recursive)
      })
    } else if (isWriteProjectFileChange(data)) {
      applyProjectChangesCoordinator = applyProjectChangesCoordinator.then(async () => {
        await writeProjectFile(data.projectFile)
      })
    } else if (isEnsureDirectoryExistsChange(data)) {
      applyProjectChangesCoordinator = applyProjectChangesCoordinator.then(async () => {
        await ensureDirectoryExists(toFSPath(data.fullPath))
      })
    } else if (isToVSCodeExtensionMessage(data)) {
      applyProjectChangesCoordinator = applyProjectChangesCoordinator.then(async () => {
        await sendMessage(data.message)
      })
    }
  })

  intervalID = window.setInterval(() => {
    try {
      window.top?.postMessage(messageListenersReady(), '*')
    } catch (error) {
      console.error('Error posting messageListenersReady', error)
    }
  }, 500)
}
