import type { ProjectFile, TemplatePath } from '../../../core/shared/project-file-types'
import type { CursorPosition } from '../../code-editor/code-editor-utils'
import type { Notice } from '../../common/notices'
import type {
  OpenEditorTab,
  PopToast,
  PushToast,
  SaveCurrentFile,
  SelectComponents,
  SendLinterRequestMessage,
  SetCodeEditorBuildErrors,
  SetCodeEditorVisibility,
  SetHighlightedView,
  UpdateFile,
} from '../action-types'
import type { EditorTab, ErrorMessages } from '../store/editor-state'

export function selectComponents(
  target: Array<TemplatePath>,
  addToSelection: boolean,
): SelectComponents {
  return {
    action: 'SELECT_COMPONENTS',
    target: target,
    addToSelection: addToSelection,
  }
}

export function setHighlightedView(target: TemplatePath): SetHighlightedView {
  return {
    action: 'SET_HIGHLIGHTED_VIEW',
    target: target,
  }
}

export function updateFile(
  filePath: string,
  file: ProjectFile,
  addIfNotInFiles: boolean,
): UpdateFile {
  return {
    action: 'UPDATE_FILE',
    filePath: filePath,
    file: file,
    addIfNotInFiles: addIfNotInFiles,
  }
}

export function setCodeEditorBuildErrors(buildErrors: ErrorMessages): SetCodeEditorBuildErrors {
  return {
    action: 'SET_CODE_EDITOR_BUILD_ERRORS',
    buildErrors: buildErrors,
  }
}

export function showToast(toastContent: Notice): PushToast {
  return pushToast(toastContent)
}

export function pushToast(toastContent: Notice): PushToast {
  return {
    action: 'PUSH_TOAST',
    toast: toastContent,
  }
}

export function popToast(): PopToast {
  return {
    action: 'POP_TOAST',
  }
}

export function saveCurrentFile(): SaveCurrentFile {
  return {
    action: 'SAVE_CURRENT_FILE',
  }
}

export function setCodeEditorVisibility(value: boolean): SetCodeEditorVisibility {
  return {
    action: 'SET_CODE_EDITOR_VISIBILITY',
    value: value,
  }
}

export function openEditorTab(
  editorTab: EditorTab,
  cursorPosition: CursorPosition | null,
): OpenEditorTab {
  return {
    action: 'OPEN_FILE',
    editorTab: editorTab,
    cursorPosition: cursorPosition,
  }
}

export function sendLinterRequestMessage(
  filePath: string,
  content: string,
): SendLinterRequestMessage {
  return {
    action: 'SEND_LINTER_REQUEST_MESSAGE',
    filePath: filePath,
    content: content,
  }
}
