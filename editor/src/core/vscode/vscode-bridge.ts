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
  UpdateDecorationsMessage,
  SelectedElementChanged,
} from 'utopia-vscode-common'
import { isTextFile, ProjectFile, ElementPath, TextFile } from '../shared/project-file-types'
import { isBrowserEnvironment } from '../shared/utils'
import {
  EditorState,
  getHighlightBoundsForElementPath,
  getOpenTextFileKey,
} from '../../components/editor/store/editor-state'
import { ProjectChange } from '../../components/editor/store/vscode-changes'

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

export function getCodeEditorDecorations(editorState: EditorState): UpdateDecorationsMessage {
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
  return updateDecorationsMessage(decorations)
}

export async function sendCodeEditorDecorations(editorState: EditorState): Promise<void> {
  const decorationsMessage = getCodeEditorDecorations(editorState)
  await sendMessage(decorationsMessage)
}

export function getSelectedElementChangedMessage(
  newEditorState: EditorState,
): SelectedElementChanged | null {
  const selectedView = newEditorState.selectedViews[0]
  const highlightBounds = getHighlightBoundsForElementPath(selectedView, newEditorState)
  if (highlightBounds == null) {
    return null
  } else {
    return selectedElementChanged(
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

export async function sendSelectedElement(newEditorState: EditorState): Promise<void> {
  const selectedElementChangedMessage = getSelectedElementChangedMessage(newEditorState)
  if (selectedElementChangedMessage == null) {
    return Promise.resolve()
  } else {
    await sendMessage(selectedElementChangedMessage)
  }
}
