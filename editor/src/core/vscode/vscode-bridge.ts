import {
  getProjectFileFromTree,
  isProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
  walkContentsTreeAsync,
  zipContentsTree,
  zipContentsTreeAsync,
} from '../../components/assets'
import { EditorDispatch } from '../../components/editor/action-types'
import {
  deleteFile,
  selectFromFileAndPosition,
  markVSCodeBridgeReady,
  updateFromCodeEditor,
  sendCodeEditorInitialisation,
  updateConfigFromVSCode,
} from '../../components/editor/actions/action-creators'
import {
  getSavedCodeFromTextFile,
  getUnsavedCodeFromTextFile,
  isDirectory,
} from '../model/project-file-utils'
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
  FSUser,
  decorationRange,
  DecorationRangeType,
  boundsInFile,
  getUtopiaVSCodeConfig,
  setFollowSelectionConfig,
} from 'utopia-vscode-common'
import { isTextFile, ProjectFile, ElementPath, TextFile } from '../shared/project-file-types'
import { isBrowserEnvironment } from '../shared/utils'
import {
  EditorState,
  getHighlightBoundsForElementPath,
  getOpenTextFileKey,
} from '../../components/editor/store/editor-state'

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

async function writeProjectFile(projectPath: string, file: ProjectFile): Promise<void> {
  switch (file.type) {
    case 'DIRECTORY': {
      return ensureDirectoryExists(toFSPath(projectPath))
    }
    case 'TEXT_FILE': {
      const savedContent = getSavedCodeFromTextFile(file)
      const unsavedContent = getUnsavedCodeFromTextFile(file)
      const filePath = toFSPath(projectPath)
      return writeFileAsUTF8(filePath, savedContent, unsavedContent)
    }
    case 'ASSET_FILE':
      return Promise.resolve()
    case 'IMAGE_FILE':
      return Promise.resolve()
  }
}

async function textFileDiffers(projectPath: string, file: TextFile): Promise<boolean> {
  const savedContent = getSavedCodeFromTextFile(file)
  const unsavedContent = getUnsavedCodeFromTextFile(file)
  const filePath = toFSPath(projectPath)
  const alreadyExistingFile = await readFileAsUTF8(filePath).catch((_) => null)
  return (
    alreadyExistingFile == null ||
    alreadyExistingFile.content !== savedContent ||
    alreadyExistingFile.unsavedContent !== unsavedContent
  )
}

async function writeProjectContents(projectContents: ProjectContentTreeRoot): Promise<void> {
  await walkContentsTreeAsync(projectContents, async (fullPath, file) => {
    // Avoid pushing a file to the file system if the content hasn't changed.
    if ((isTextFile(file) && (await textFileDiffers(fullPath, file))) || isDirectory(file)) {
      return writeProjectFile(fullPath, file)
    } else {
      return Promise.resolve()
    }
  })
}

function watchForChanges(dispatch: EditorDispatch): void {
  function onCreated(fsPath: string): void {
    stat(fsPath).then((fsStat) => {
      if (fsStat.type === 'FILE' && fsStat.sourceOfLastChange !== UtopiaFSUser) {
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
      await initializeFS(projectID, UtopiaFSUser)
      await clearBothMailboxes()
      await writeProjectContents(projectContents)
      await initMailbox(UtopiaInbox, parseFromVSCodeMessage, (message: FromVSCodeMessage) => {
        switch (message.type) {
          case 'SEND_INITIAL_DATA':
            dispatch([sendCodeEditorInitialisation()], 'everyone')
            break
          case 'EDITOR_CURSOR_POSITION_CHANGED':
            dispatch(
              [selectFromFileAndPosition(message.filePath, message.line, message.column)],
              'everyone',
            )
            break
          case 'UTOPIA_VSCODE_CONFIG_VALUES':
            dispatch([updateConfigFromVSCode(message.config)], 'everyone')
            break
          default:
            const _exhaustiveCheck: never = message
            throw new Error(`Unhandled message type${JSON.stringify(message)}`)
        }
      })
      sendGetUtopiaVSCodeConfigMessage()
      watchForChanges(dispatch)
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

export async function sendSelectedElementChangedMessage(
  boundsForFile: BoundsInFile,
): Promise<void> {
  return sendMessage(selectedElementChanged(boundsForFile))
}

export async function sendSetFollowSelectionEnabledMessage(enabled: boolean): Promise<void> {
  return sendMessage(setFollowSelectionConfig(enabled))
}

export async function sendGetUtopiaVSCodeConfigMessage(): Promise<void> {
  return sendMessage(getUtopiaVSCodeConfig())
}

export interface WriteProjectFileChange {
  type: 'WRITE_PROJECT_FILE'
  fullPath: string
  projectFile: ProjectFile
}

export function writeProjectFileChange(
  fullPath: string,
  projectFile: ProjectFile,
): WriteProjectFileChange {
  return {
    type: 'WRITE_PROJECT_FILE',
    fullPath: fullPath,
    projectFile: projectFile,
  }
}

export interface DeletePathChange {
  type: 'DELETE_PATH'
  fullPath: string
  recursive: boolean
}

export function deletePathChange(fullPath: string, recursive: boolean): DeletePathChange {
  return {
    type: 'DELETE_PATH',
    fullPath: fullPath,
    recursive: recursive,
  }
}

export interface EnsureDirectoryExistsChange {
  type: 'ENSURE_DIRECTORY_EXISTS'
  fullPath: string
}

export function ensureDirectoryExistsChange(fullPath: string): EnsureDirectoryExistsChange {
  return {
    type: 'ENSURE_DIRECTORY_EXISTS',
    fullPath: fullPath,
  }
}

export type ProjectChange = WriteProjectFileChange | DeletePathChange | EnsureDirectoryExistsChange

export function collateProjectChanges(
  projectID: string,
  oldContents: ProjectContentTreeRoot,
  newContents: ProjectContentTreeRoot,
): Array<ProjectChange> {
  let changesToProcess: Array<ProjectChange> = []

  function applyChanges(
    fullPath: string,
    firstContents: ProjectContentsTree,
    secondContents: ProjectContentsTree,
  ): boolean {
    if (isProjectContentFile(firstContents)) {
      if (isProjectContentFile(secondContents)) {
        if (firstContents.content === secondContents.content) {
          // Do nothing, no change.
        } else if (isTextFile(firstContents.content) && isTextFile(secondContents.content)) {
          // We need to be careful around only sending this across if the text has been updated.
          const firstSavedContent = getSavedCodeFromTextFile(firstContents.content)
          const firstUnsavedContent = getUnsavedCodeFromTextFile(firstContents.content)
          const secondSavedContent = getSavedCodeFromTextFile(secondContents.content)
          const secondUnsavedContent = getUnsavedCodeFromTextFile(secondContents.content)

          const savedContentChanged = firstSavedContent !== secondSavedContent
          const unsavedContentChanged = firstUnsavedContent !== secondUnsavedContent
          const fileMarkedDirtyButNoCodeChangeYet =
            firstUnsavedContent == null && secondUnsavedContent === firstSavedContent

          // When a parsed model is updated but that change hasn't been reflected in the code yet, we end up with a file
          // that has no code change, so we don't want to write that to the FS for VS Code to act on it until the new code
          // has been generated
          const fileShouldBeWritten =
            savedContentChanged || (unsavedContentChanged && !fileMarkedDirtyButNoCodeChangeYet)

          if (fileShouldBeWritten) {
            changesToProcess.push(writeProjectFileChange(fullPath, secondContents.content))
          }
        } else {
          changesToProcess.push(writeProjectFileChange(fullPath, secondContents.content))
        }
      } else {
        changesToProcess.push(deletePathChange(fullPath, true))
        changesToProcess.push(ensureDirectoryExistsChange(fullPath))
      }
    } else {
      if (isProjectContentFile(secondContents)) {
        changesToProcess.push(deletePathChange(fullPath, true))
        changesToProcess.push(writeProjectFileChange(fullPath, secondContents.content))
      } else {
        // Do nothing, both sides are a directory.
      }
    }

    return true
  }

  function onElement(
    fullPath: string,
    firstContents: ProjectContentsTree | null,
    secondContents: ProjectContentsTree | null,
  ): boolean {
    if (firstContents == null) {
      if (secondContents == null) {
        // Do nothing, nothing exists.
        return false
      } else {
        changesToProcess.push(
          writeProjectFileChange(fullPath, getProjectFileFromTree(secondContents)),
        )
        return true
      }
    } else {
      if (secondContents == null) {
        changesToProcess.push(deletePathChange(fullPath, true))
        return false
      } else {
        if (firstContents === secondContents) {
          // Same value, stop here.
          return false
        } else {
          return applyChanges(fullPath, firstContents, secondContents)
        }
      }
    }
  }
  if (isBrowserEnvironment) {
    if (oldContents != newContents) {
      zipContentsTree(oldContents, newContents, onElement)
    }
  }

  return changesToProcess
}

export async function applyProjectChanges(changes: Array<ProjectChange>): Promise<void> {
  for (const change of changes) {
    switch (change.type) {
      case 'DELETE_PATH':
        // eslint-disable-next-line no-await-in-loop
        await deletePath(toFSPath(change.fullPath), change.recursive)
        break
      case 'WRITE_PROJECT_FILE':
        // eslint-disable-next-line no-await-in-loop
        await writeProjectFile(change.fullPath, change.projectFile)
        break
      case 'ENSURE_DIRECTORY_EXISTS':
        // eslint-disable-next-line no-await-in-loop
        await ensureDirectoryExists(toFSPath(change.fullPath))
        break
      default:
        const _exhaustiveCheck: never = change
        throw new Error(`Unhandled message type: ${JSON.stringify(change)}`)
    }
  }
}

export async function sendCodeEditorDecorations(editorState: EditorState): Promise<void> {
  let decorations: Array<DecorationRange> = []
  function addRange(rangeType: DecorationRangeType, path: ElementPath): void {
    const highlightBounds = getHighlightBoundsForElementPath(path, editorState)
    if (highlightBounds != null) {
      decorations.push(
        decorationRange(
          rangeType,
          highlightBounds.filePath,
          highlightBounds.startLine,
          highlightBounds.startCol,
          highlightBounds.endLine,
          highlightBounds.endCol,
        ),
      )
    }
  }

  editorState.selectedViews.forEach((selectedView) => {
    addRange('selection', selectedView)
  })
  editorState.highlightedViews.forEach((highlightedView) => {
    addRange('highlight', highlightedView)
  })
  await sendUpdateDecorationsMessage(decorations)
}

export async function sendSelectedElement(newEditorState: EditorState): Promise<void> {
  const selectedView = newEditorState.selectedViews[0]
  const highlightBounds = getHighlightBoundsForElementPath(selectedView, newEditorState)
  if (highlightBounds != null) {
    await sendSelectedElementChangedMessage(
      boundsInFile(
        highlightBounds.filePath,
        highlightBounds.startLine,
        highlightBounds.startCol,
        highlightBounds.endLine,
        highlightBounds.endCol,
      ),
    )
  }
}
