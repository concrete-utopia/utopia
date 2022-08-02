import { Spec } from 'immutability-helper'
import { drop } from '../../../core/shared/array-utils'
import {
  getContentsTreeFileFromString,
  getProjectContentKeyPathElements,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../../components/assets'
import { isDirectory } from '../../../core/model/project-file-utils'

export function createProjectContentsPatch(
  target: string,
  projectContents: ProjectContentTreeRoot,
  createFilePatch: (
    file: ProjectContentFile['content'],
  ) => Spec<ProjectContentFile['content']> | null,
): Spec<ProjectContentTreeRoot> {
  const currentFile = getContentsTreeFileFromString(projectContents, target)
  if (currentFile == null) {
    return {}
  } else {
    if (isDirectory(currentFile)) {
      return {}
    } else {
      const filePatch = createFilePatch(currentFile)
      if (filePatch == null) {
        return {}
      } else {
        const pathElements = getProjectContentKeyPathElements(target)
        if (pathElements.length === 0) {
          return {}
        } else {
          const remainderPath = drop(1, pathElements)
          const contentsTreePatch: Spec<ProjectContentsTree> = {
            content: filePatch,
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
          return projectContentTreeRootPatch
        }
      }
    }
  }
  return {}
}
