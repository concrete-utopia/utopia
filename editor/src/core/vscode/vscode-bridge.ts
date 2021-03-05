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
  sendCodeEditorInitialisation,
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
  FSUser,
  decorationRange,
  DecorationRangeType,
  boundsInFile,
} from 'utopia-vscode-common'
import { isTextFile, ProjectFile, TemplatePath, TextFile } from '../shared/project-file-types'
import { isBrowserEnvironment } from '../shared/utils'
import {
  EditorState,
  getHighlightBoundsForTemplatePath,
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
function getSavedCodeFromTextFile(textFile: TextFile): string {
  return textFile.lastSavedContents?.code ?? textFile.fileContents.code
}

function getUnsavedCodeFromTextFile(textFile: TextFile): string | null {
  return textFile.lastSavedContents == null ? null : textFile.fileContents.code
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
      const savedContent = getSavedCodeFromTextFile(file)
      const unsavedContent = getUnsavedCodeFromTextFile(file)
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
      await writeProjectContents(projectID, projectContents)
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
          default:
            const _exhaustiveCheck: never = message
            throw new Error(`Unhandled message type${JSON.stringify(message)}`)
        }
      })
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

export async function sendCodeEditorDecorations(editorState: EditorState): Promise<void> {
  let decorations: Array<DecorationRange> = []
  function addRange(filename: string, rangeType: DecorationRangeType, path: TemplatePath): void {
    const highlightBounds = getHighlightBoundsForTemplatePath(path, editorState)
    if (highlightBounds != null) {
      decorations.push(
        decorationRange(
          rangeType,
          filename,
          highlightBounds.startLine,
          highlightBounds.startCol,
          highlightBounds.endLine,
          highlightBounds.endCol,
        ),
      )
    }
  }
  const openFilename = getOpenTextFileKey(editorState)
  if (openFilename != null) {
    editorState.selectedViews.forEach((selectedView) => {
      addRange(openFilename, 'selection', selectedView)
    })
    editorState.highlightedViews.forEach((highlightedView) => {
      addRange(openFilename, 'highlight', highlightedView)
    })
  }
  await sendUpdateDecorationsMessage(decorations)
}

export async function sendSelectedElement(newEditorState: EditorState): Promise<void> {
  const openFilename = getOpenTextFileKey(newEditorState)
  if (openFilename != null) {
    const selectedView = newEditorState.selectedViews[0]
    const highlightBounds = getHighlightBoundsForTemplatePath(selectedView, newEditorState)
    if (highlightBounds != null) {
      await sendSelectedElementChangedMessage(
        boundsInFile(
          openFilename,
          highlightBounds.startLine,
          highlightBounds.startCol,
          highlightBounds.endLine,
          highlightBounds.endCol,
        ),
      )
    }
  }
}
