import {
  getProjectFileFromTree,
  isProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
  walkContentsTree,
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
  sendLinterRequestMessage,
  hideVSCodeLoadingScreen,
  setIndexedDBFailed,
} from '../../components/editor/actions/action-creators'
import {
  getSavedCodeFromTextFile,
  getUnsavedCodeFromTextFile,
  isDirectory,
} from '../model/project-file-utils'
import {
  openFileMessage,
  DecorationRange,
  updateDecorationsMessage,
  selectedElementChanged,
  decorationRange,
  DecorationRangeType,
  boundsInFile,
  getUtopiaVSCodeConfig,
  setFollowSelectionConfig,
  UpdateDecorationsMessage,
  SelectedElementChanged,
  utopiaReady,
  setVSCodeTheme,
  isMessageListenersReady,
  ProjectFile as CommonProjectFile,
  projectTextFile,
  projectDirectory,
  initProject,
  isVSCodeBridgeReady,
  isFromVSCodeExtensionMessage,
  isVSCodeFileChange,
  isVSCodeFileDelete,
  isIndexedDBFailure,
  ToVSCodeMessage,
  toVSCodeExtensionMessage,
  deletePathChange,
  ToVSCodeExtensionMessage,
  FromUtopiaToVSCodeMessage,
  writeProjectFileChange,
  ensureDirectoryExistsChange,
} from 'utopia-vscode-common'
import { isTextFile, ProjectFile, ElementPath, TextFile } from '../shared/project-file-types'
import { assertNever, isBrowserEnvironment } from '../shared/utils'
import {
  EditorState,
  getHighlightBoundsForElementPath,
  getOpenTextFileKey,
} from '../../components/editor/store/editor-state'
import { ProjectFileChange } from '../../components/editor/store/vscode-changes'
import { Theme } from '../../uuiui'

export const VSCODE_EDITOR_IFRAME_ID = 'vscode-editor'

function projectFileToCommonProjectFile(
  fullPath: string,
  projectFile: ProjectFile,
): CommonProjectFile | null {
  switch (projectFile.type) {
    case 'TEXT_FILE': {
      const savedContent = getSavedCodeFromTextFile(projectFile)
      const unsavedContent = getUnsavedCodeFromTextFile(projectFile)
      return projectTextFile(fullPath, savedContent, unsavedContent)
    }
    case 'DIRECTORY': {
      return projectDirectory(fullPath)
    }
    case 'ASSET_FILE':
    case 'IMAGE_FILE':
      return null // Don't send these files to VS Code
    default:
      assertNever(projectFile)
  }
}

function convertProjectContents(projectContents: ProjectContentTreeRoot): Array<CommonProjectFile> {
  let projectFiles: Array<CommonProjectFile> = []
  walkContentsTree(projectContents, (fullPath, file) => {
    const commonProjectFile = projectFileToCommonProjectFile(fullPath, file)
    if (commonProjectFile != null) {
      projectFiles.push(commonProjectFile)
    }
  })

  return projectFiles
}

let currentInit: Promise<void> = Promise.resolve()

let vscodeIFrame: MessageEventSource | null = null

export async function initVSCodeBridge(
  projectID: string,
  projectContents: ProjectContentTreeRoot,
  dispatch: EditorDispatch,
  openFilePath: string | null,
): Promise<void> {
  // window.removeEventListener('message', onMessage)
  // window.addEventListener('message', onMessage)

  let loadingScreenHidden = false

  window.addEventListener('message', (messageEvent: MessageEvent) => {
    const { data } = messageEvent
    if (isMessageListenersReady(data) && messageEvent.source != null) {
      // Store the source
      vscodeIFrame = messageEvent.source

      // Send the full project contents
      const projectFiles = convertProjectContents(projectContents)
      sendMessage(initProject(projectFiles, openFilePath))

      if (openFilePath == null) {
        loadingScreenHidden = true
        dispatch([hideVSCodeLoadingScreen()], 'everyone')
      }
    } else if (isVSCodeBridgeReady(data)) {
      dispatch([markVSCodeBridgeReady(true)], 'everyone')
    } else if (isFromVSCodeExtensionMessage(data)) {
      const message = data.message
      switch (message.type) {
        case 'EDITOR_CURSOR_POSITION_CHANGED':
          dispatch(
            [selectFromFileAndPosition(message.filePath, message.line, message.column)],
            'everyone',
          )
          break
        case 'UTOPIA_VSCODE_CONFIG_VALUES':
          dispatch([updateConfigFromVSCode(message.config)], 'everyone')
          break
        case 'VSCODE_READY':
          dispatch([sendCodeEditorInitialisation()], 'everyone')
          break
        case 'CLEAR_LOADING_SCREEN':
          if (!loadingScreenHidden) {
            loadingScreenHidden = true
            dispatch([hideVSCodeLoadingScreen()], 'everyone')
          }
          break
        default:
          const _exhaustiveCheck: never = message
          throw new Error(`Unhandled message type${JSON.stringify(message)}`)
      }
    } else if (isVSCodeFileChange(data)) {
      const { filePath, fileContent } = data
      const updateAction = updateFromCodeEditor(
        filePath,
        fileContent.content,
        fileContent.unsavedContent,
      )
      const requestLintAction = sendLinterRequestMessage(
        filePath,
        fileContent.unsavedContent ?? fileContent.content,
      )
      dispatch([updateAction, requestLintAction], 'everyone')
    } else if (isVSCodeFileDelete(data)) {
      const action = deleteFile(data.filePath)
      dispatch([action], 'everyone')
    } else if (isIndexedDBFailure(data)) {
      dispatch([setIndexedDBFailed(true)], 'everyone')
    }
  })

  // // Prevent multiple initialisations from driving over each other.
  // currentInit = currentInit.then(innerInit)
}

export function sendMessage(message: FromUtopiaToVSCodeMessage) {
  vscodeIFrame?.postMessage(message, { targetOrigin: '*' })
}

// FIXME None of these are async now
export async function sendOpenFileMessage(filePath: string): Promise<void> {
  return sendMessage(toVSCodeExtensionMessage(openFileMessage(filePath)))
}

export async function sendUpdateDecorationsMessage(
  decorations: Array<DecorationRange>,
): Promise<void> {
  return sendMessage(toVSCodeExtensionMessage(updateDecorationsMessage(decorations)))
}

export async function sendSetFollowSelectionEnabledMessage(enabled: boolean): Promise<void> {
  return sendMessage(toVSCodeExtensionMessage(setFollowSelectionConfig(enabled)))
}

export async function sendGetUtopiaVSCodeConfigMessage(): Promise<void> {
  return sendMessage(toVSCodeExtensionMessage(getUtopiaVSCodeConfig()))
}

export async function applyProjectChanges(changes: Array<ProjectFileChange>): Promise<void> {
  for (const change of changes) {
    switch (change.type) {
      case 'DELETE_PATH':
        sendMessage(deletePathChange(change.fullPath, change.recursive))
        break
      case 'WRITE_PROJECT_FILE':
        const commonProjectFile = projectFileToCommonProjectFile(
          change.fullPath,
          change.projectFile,
        )
        if (commonProjectFile != null) {
          sendMessage(writeProjectFileChange(commonProjectFile))
        }
        break
      case 'ENSURE_DIRECTORY_EXISTS':
        sendMessage(ensureDirectoryExistsChange(change.fullPath))
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
  await sendMessage(toVSCodeExtensionMessage(decorationsMessage))
}

export function getSelectedElementChangedMessage(
  newEditorState: EditorState,
): SelectedElementChanged | null {
  const selectedView = newEditorState.selectedViews[0]
  if (selectedView == null) {
    return null
  }
  const highlightBounds = getHighlightBoundsForElementPath(selectedView, newEditorState)
  if (highlightBounds == null) {
    return null
  } else {
    if (document.activeElement?.id === VSCODE_EDITOR_IFRAME_ID) {
      // If the code editor is active, we don't want to inform it of selection changes as that
      // would then update the user's cursor in VS Code
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
}

export async function sendSelectedElement(newEditorState: EditorState): Promise<void> {
  const selectedElementChangedMessage = getSelectedElementChangedMessage(newEditorState)
  if (selectedElementChangedMessage == null) {
    return Promise.resolve()
  } else {
    await sendMessage(toVSCodeExtensionMessage(selectedElementChangedMessage))
  }
}

function vsCodeThemeForTheme(theme: Theme): string {
  switch (theme) {
    case 'dark':
      return 'Default Dark+'
    case 'light':
      return 'Default Light+'
    default:
      const _exhaustiveCheck: never = theme
      throw new Error(`Unhandled theme ${theme}`)
  }
}

export async function sendSetVSCodeTheme(theme: Theme): Promise<void> {
  const vsCodeTheme = vsCodeThemeForTheme(theme)
  await sendMessage(toVSCodeExtensionMessage(setVSCodeTheme(vsCodeTheme)))
}
