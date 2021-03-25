import { useContextSelector } from 'use-context-selector'
import { TopLevelElement } from '../../../core/shared/element-template'
import { Imports, isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../assets'
import {
  DerivedState,
  EditorState,
  getFileForName,
  getOpenUIJSFileKey,
  TransientFileState,
} from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { UtopiaProjectContext } from './ui-jsx-canvas-contexts'

export function getTopLevelElementsFromEditorState(
  filePath: string,
  editor: EditorState,
  derived: DerivedState,
): {
  topLevelElements: TopLevelElement[]
  imports: Imports
} {
  return getTopLevelElements(
    filePath,
    editor.projectContents,
    getOpenUIJSFileKey(editor),
    derived.canvas.transientState.fileState,
  )
}

const EmptyProjectContents: TopLevelElement[] = []
export function getTopLevelElements(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
  openStoryboardFileNameKILLME: string | null,
  transientFileState: TransientFileState | null,
): {
  topLevelElements: TopLevelElement[]
  imports: Imports
} {
  const projectFile = getContentsTreeFileFromString(projectContents, filePath)
  if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
    if (openStoryboardFileNameKILLME === filePath && transientFileState != null) {
      return {
        topLevelElements: transientFileState.topLevelElementsIncludingScenes,
        imports: transientFileState.imports,
      }
    }
    return {
      topLevelElements: projectFile.fileContents.parsed.topLevelElements,
      imports: projectFile.fileContents.parsed.imports,
    }
  } else {
    return {
      topLevelElements: EmptyProjectContents,
      imports: {},
    }
  }
}

export function useGetTopLevelElements(filePath: string): TopLevelElement[] {
  return useContextSelector(UtopiaProjectContext, (c) => {
    return getTopLevelElements(
      filePath,
      c.projectContents,
      c.openStoryboardFilePathKILLME,
      c.transientFileState,
    )
  }).topLevelElements
}
