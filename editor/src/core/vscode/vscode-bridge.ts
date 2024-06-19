import type {
  DecorationRange,
  DecorationRangeType,
  FromUtopiaToVSCodeMessage,
  ProjectFile as CommonProjectFile,
  SelectedElementChanged,
  UpdateDecorationsMessage,
  ForceNavigation,
  BoundsInFile,
} from 'utopia-vscode-common'
import {
  boundsInFile,
  decorationRange,
  deletePathChange,
  ensureDirectoryExistsChange,
  initProject,
  isFromVSCodeExtensionMessage,
  isIndexedDBFailure,
  isMessageListenersReady,
  isVSCodeBridgeReady,
  isVSCodeFileChange,
  isVSCodeFileDelete,
  openFileMessage,
  projectDirectory,
  projectTextFile,
  selectedElementChanged,
  setFollowSelectionConfig,
  setVSCodeTheme,
  toVSCodeExtensionMessage,
  updateDecorationsMessage,
  writeProjectFileChange,
} from 'utopia-vscode-common'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { walkContentsTree } from '../../components/assets'
import { EditorDispatch } from '../../components/editor/action-types'
import type { EditorState } from '../../components/editor/store/editor-state'
import { getHighlightBoundsForElementPath } from '../../components/editor/store/editor-state'
import type { ProjectFileChange } from '../../components/editor/store/vscode-changes'
import type { Theme } from '../../uuiui'
import { getSavedCodeFromTextFile, getUnsavedCodeFromTextFile } from '../model/project-file-utils'
import type {
  ElementPath,
  HighlightBoundsWithFile,
  ProjectFile,
} from '../shared/project-file-types'
import { assertNever, NO_OP } from '../shared/utils'
import type { FromVSCodeAction } from '../../components/editor/actions/actions-from-vscode'
import {
  deleteFileFromVSCode,
  hideVSCodeLoadingScreen,
  markVSCodeBridgeReady,
  selectFromFileAndPosition,
  sendCodeEditorInitialisation,
  sendLinterRequestMessage,
  setIndexedDBFailed,
  updateConfigFromVSCode,
  updateFromCodeEditor,
} from '../../components/editor/actions/actions-from-vscode'

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

let vscodeIFrame: MessageEventSource | null = null
let registeredHandlers: (messageEvent: MessageEvent) => void = NO_OP

export function initVSCodeBridge(
  projectContents: ProjectContentTreeRoot,
  dispatch: (actions: Array<FromVSCodeAction>) => void,
  openFilePath: string | null,
) {
  let loadingScreenHidden = false
  let seenMessageListenersReadyMessage = false

  // Remove any existing message handlers to prevent us accidentally duplicating them
  window.removeEventListener('message', registeredHandlers)
  registeredHandlers = (messageEvent: MessageEvent) => {
    const { data } = messageEvent
    if (
      isMessageListenersReady(data) &&
      messageEvent.source != null &&
      !seenMessageListenersReadyMessage
    ) {
      seenMessageListenersReadyMessage = true
      // Don't store the source yet, because we don't want to send any messages
      // until the bridge is ready

      // Send the full project contents
      const projectFiles = convertProjectContents(projectContents)
      messageEvent.source.postMessage(initProject(projectFiles, openFilePath), {
        targetOrigin: '*',
      })

      if (openFilePath == null) {
        loadingScreenHidden = true
        dispatch([hideVSCodeLoadingScreen()])
      }
    } else if (isVSCodeBridgeReady(data) && messageEvent.source != null) {
      // Store the source
      vscodeIFrame = messageEvent.source
      dispatch([markVSCodeBridgeReady(true)])
    } else if (isFromVSCodeExtensionMessage(data)) {
      const message = data.message
      switch (message.type) {
        case 'EDITOR_CURSOR_POSITION_CHANGED':
          dispatch([selectFromFileAndPosition(message.filePath, message.line, message.column)])
          break
        case 'UTOPIA_VSCODE_CONFIG_VALUES':
          dispatch([updateConfigFromVSCode(message.config)])
          break
        case 'VSCODE_READY':
          dispatch([sendCodeEditorInitialisation()])
          break
        case 'CLEAR_LOADING_SCREEN':
          if (!loadingScreenHidden) {
            loadingScreenHidden = true
            dispatch([hideVSCodeLoadingScreen()])
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
      dispatch([updateAction, requestLintAction])
    } else if (isVSCodeFileDelete(data)) {
      dispatch([deleteFileFromVSCode(data.filePath)])
    } else if (isIndexedDBFailure(data)) {
      dispatch([setIndexedDBFailed(true)])
    }
  }

  window.addEventListener('message', registeredHandlers)
}

export function sendMessage(message: FromUtopiaToVSCodeMessage) {
  vscodeIFrame?.postMessage(message, { targetOrigin: '*' })
}

export function sendOpenFileMessage(filePath: string, bounds?: BoundsInFile) {
  sendMessage(toVSCodeExtensionMessage(openFileMessage(filePath, bounds)))
}

export function sendSetFollowSelectionEnabledMessage(enabled: boolean) {
  sendMessage(toVSCodeExtensionMessage(setFollowSelectionConfig(enabled)))
}

export function applyProjectChanges(changes: Array<ProjectFileChange>) {
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

export function sendCodeEditorDecorations(editorState: EditorState) {
  const decorationsMessage = getCodeEditorDecorations(editorState)
  sendMessage(toVSCodeExtensionMessage(decorationsMessage))
}

export function getSelectedElementChangedMessage(
  newEditorState: EditorState,
  forceNavigation: ForceNavigation,
): SelectedElementChanged | null {
  const selectedView = newEditorState.selectedViews[0]
  if (selectedView == null) {
    return null
  }
  const highlightBounds = getHighlightBoundsForElementPath(selectedView, newEditorState)
  if (highlightBounds == null) {
    return null
  } else {
    return selectedElementChangedMessageFromHighlightBounds(highlightBounds, forceNavigation)
  }
}

export function selectedElementChangedMessageFromHighlightBounds(
  highlightBounds: HighlightBoundsWithFile,
  forceNavigation: ForceNavigation,
): SelectedElementChanged {
  return selectedElementChanged(
    boundsInFile(
      highlightBounds.filePath,
      highlightBounds.startLine,
      highlightBounds.startCol,
      highlightBounds.endLine,
      highlightBounds.endCol,
    ),
    forceNavigation,
  )
}

export function sendSelectedElement(newEditorState: EditorState) {
  const selectedElementChangedMessage = getSelectedElementChangedMessage(
    newEditorState,
    'do-not-force-navigation',
  )
  if (selectedElementChangedMessage != null) {
    sendMessage(toVSCodeExtensionMessage(selectedElementChangedMessage))
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

export function sendSetVSCodeTheme(theme: Theme) {
  const vsCodeTheme = vsCodeThemeForTheme(theme)
  sendMessage(toVSCodeExtensionMessage(setVSCodeTheme(vsCodeTheme)))
}
