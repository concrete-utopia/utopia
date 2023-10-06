import type { Spec } from 'immutability-helper'
import { drop } from '../../../core/shared/array-utils'
import type { ProjectContentsTree, ProjectContentTreeRoot } from '../../../components/assets'
import {
  getProjectFileByFilePath,
  getProjectContentKeyPathElements,
} from '../../../components/assets'
import type { EditorState } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import type {
  ElementPath,
  ParsedTextFile,
  ParseSuccess,
  StaticElementPath,
} from '../../../core/shared/project-file-types'
import { isDirectory } from '../../../core/shared/project-file-types'
import { isParseSuccess, isTextFile, RevisionsState } from '../../../core/shared/project-file-types'
import type { JSXElementChild } from '../../../core/shared/element-template'

export function patchProjectContentsWithParsedFile(
  filePath: string,
  filePatch: Spec<ParsedTextFile>,
): Spec<EditorState> {
  const pathElements = getProjectContentKeyPathElements(filePath)
  if (pathElements.length === 0) {
    return {}
  } else {
    const remainderPath = drop(1, pathElements)
    const contentsTreePatch: Spec<ProjectContentsTree> = {
      content: {
        fileContents: {
          revisionsState: {
            $set: RevisionsState.ParsedAhead,
          },
          parsed: filePatch,
        },
        versionNumber: {
          $apply: (v) => v + 1,
        },
      },
    }

    const projectContentsTreePatch: Spec<ProjectContentsTree> = remainderPath.reduceRight(
      (working: Spec<ProjectContentsTree>, pathPart: string) => {
        return {
          children: {
            [pathPart]: working,
          },
        }
      },
      contentsTreePatch,
    )

    // Finally patch the last part of the path in.
    const projectContentTreeRootPatch: Spec<ProjectContentTreeRoot> = {
      [pathElements[0]]: projectContentsTreePatch,
    }
    return {
      projectContents: projectContentTreeRootPatch,
    }
  }
}

export function patchParseSuccessAtElementPath(
  target: ElementPath,
  editorState: EditorState,
  patchParseSuccess: (
    success: ParseSuccess,
    element: JSXElementChild,
    underlyingTarget: StaticElementPath,
    underlyingFilePath: string,
  ) => Spec<ParsedTextFile> | null,
): Spec<EditorState> {
  return withUnderlyingTargetFromEditorState(
    target,
    editorState,
    {},
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      const filePatch = patchParseSuccess(
        success,
        underlyingElement,
        underlyingTarget,
        underlyingFilePath,
      )
      if (filePatch == null) {
        return {}
      } else {
        return patchProjectContentsWithParsedFile(underlyingFilePath, filePatch)
      }
    },
  )
}

export function patchParseSuccessAtFilePath(
  filePath: string,
  editorState: EditorState,
  patchParseSuccess: (success: ParseSuccess) => Spec<ParsedTextFile> | null,
): Spec<EditorState> {
  const projectFile = getProjectFileByFilePath(editorState.projectContents, filePath)
  if (projectFile == null) {
    return {}
  } else {
    if (isDirectory(projectFile)) {
      return {}
    } else {
      if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
        const filePatch = patchParseSuccess(projectFile.fileContents.parsed)
        if (filePatch == null) {
          return {}
        } else {
          return patchProjectContentsWithParsedFile(filePath, filePatch)
        }
      } else {
        return {}
      }
    }
  }
}
