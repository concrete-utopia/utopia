import { TopLevelElement } from '../../../core/shared/element-template'
import { isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import {
  DerivedState,
  EditorState,
  getFileForName,
  getOpenUIJSFileKey,
} from '../../editor/store/editor-state'

export function getTopLevelElements(
  filePath: string,
  editor: EditorState,
  derived: DerivedState,
): TopLevelElement[] {
  const projectFile = getFileForName(filePath, editor)
  const uiFilePath = getOpenUIJSFileKey(editor)
  if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
    if (uiFilePath === filePath && derived.canvas.transientState.fileState != null) {
      return derived.canvas.transientState.fileState.topLevelElementsIncludingScenes
    }
    return projectFile.fileContents.parsed.topLevelElements
  } else {
    return []
  }
}
