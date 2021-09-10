import {
  addFileToProjectContents,
  projectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
  transformContentsTree,
} from '../components/assets'
import { PersistentModel, StoryboardFilePath } from '../components/editor/store/editor-state'
import {
  isParseSuccess,
  ParsedTextFile,
  RevisionsState,
  textFile,
  textFileContents,
} from '../core/shared/project-file-types'
import { emptySet } from '../core/shared/set-utils'
import { lintAndParse } from '../core/workers/parser-printer/parser-printer'
import { complexDefaultProject, defaultProject, simpleDefaultProject } from './sample-project-utils'

export function simpleDefaultProjectPreParsed(): PersistentModel {
  const project = simpleDefaultProject()
  const updatedProjectContents = parseProjectContents(project.projectContents)

  return {
    ...project,
    projectContents: updatedProjectContents,
  }
}

export function complexDefaultProjectPreParsed(): PersistentModel {
  const project = complexDefaultProject()
  const updatedProjectContents = parseProjectContents(project.projectContents)

  return {
    ...project,
    projectContents: updatedProjectContents,
  }
}

export function parseProjectContents(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  return transformContentsTree(projectContents, (tree: ProjectContentsTree) => {
    if (tree.type === 'PROJECT_CONTENT_FILE') {
      const file = tree.content
      if (file.type === 'TEXT_FILE') {
        const parsed = lintAndParse(tree.fullPath, file.fileContents.code, null, emptySet())
        const updatedFile = textFile(
          textFileContents(file.fileContents.code, parsed, RevisionsState.BothMatch),
          null,
          isParseSuccess(parsed) ? parsed : null,
          Date.now(),
        )
        return projectContentFile(tree.fullPath, updatedFile)
      } else {
        return tree
      }
    } else {
      return tree
    }
  })
}

export function createTestProjectWithCode(appUiJsFile: string): PersistentModel {
  const baseModel = defaultProject()
  const parsedFile = lintAndParse(
    StoryboardFilePath,
    appUiJsFile,
    null,
    emptySet(),
  ) as ParsedTextFile

  if (!isParseSuccess(parsedFile)) {
    fail('The test file parse failed')
  }

  return {
    ...baseModel,
    projectContents: addFileToProjectContents(
      baseModel.projectContents,
      StoryboardFilePath,
      textFile(
        textFileContents(appUiJsFile, parsedFile, RevisionsState.BothMatch),
        null,
        parsedFile,
        Date.now(),
      ),
    ),
  }
}
