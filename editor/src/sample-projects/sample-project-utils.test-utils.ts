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
  ParseFailure,
  isParseFailure,
  isUnparsed,
  ParseSuccess,
} from '../core/shared/project-file-types'
import { emptySet } from '../core/shared/set-utils'
import { lintAndParse } from '../core/workers/parser-printer/parser-printer'
import { complexDefaultProject, simpleDefaultProject } from './sample-project-utils'

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
        const parsed = lintAndParse(
          tree.fullPath,
          file.fileContents.code,
          null,
          emptySet(),
          'trim-bounds',
        )
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

export function getParseSuccessForStoryboardCode(appUiJsFile: string): ParseSuccess {
  const parsedFile = lintAndParse(StoryboardFilePath, appUiJsFile, null, emptySet(), 'trim-bounds')

  if (isParseFailure(parsedFile)) {
    const failure =
      parsedFile.errorMessage ?? parsedFile.errorMessages.map((m) => m.message).join(`,\n`)
    throw new Error(`createTestProjectWithCode file parsing failed: ${failure}`)
  } else if (isUnparsed(parsedFile)) {
    throw new Error(`createTestProjectWithCode: Unexpected unparsed file.`)
  }
  return parsedFile
}

export function createTestProjectWithCode(appUiJsFile: string): PersistentModel {
  const baseModel = complexDefaultProject()
  const parsedFile: ParseSuccess = getParseSuccessForStoryboardCode(appUiJsFile)

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

export function createTestProjectWithMultipleFiles(files: {
  [filename: string]: string
}): PersistentModel {
  const baseModel = complexDefaultProject()
  return Object.entries(files).reduce((model, [filename, contents]) => {
    const parsedFile: ParseSuccess = getParseSuccessForStoryboardCode(contents)

    return {
      ...model,
      projectContents: addFileToProjectContents(
        model.projectContents,
        filename,
        textFile(
          textFileContents(contents, parsedFile, RevisionsState.BothMatch),
          null,
          parsedFile,
          Date.now(),
        ),
      ),
    }
  }, baseModel)
}

export function createModifiedProject(modifiedFiles: { [filename: string]: string }) {
  const baseModel = complexDefaultProject()

  const updatedProject = Object.keys(modifiedFiles).reduce((workingProject, modifiedFilename) => {
    const parsedFile = lintAndParse(
      modifiedFilename,
      modifiedFiles[modifiedFilename],
      null,
      emptySet(),
      'trim-bounds',
    ) as ParsedTextFile
    if (!isParseSuccess(parsedFile)) {
      const failedParse = parsedFile as ParseFailure
      const failure =
        failedParse.errorMessage ?? failedParse.errorMessages.map((m) => m.message).join(`,\n`)
      throw new Error(`The test file parse failed ${modifiedFilename}, ${failure}`)
    }

    const updatedProjectContents = addFileToProjectContents(
      workingProject.projectContents,
      modifiedFilename,
      textFile(
        textFileContents(modifiedFiles[modifiedFilename], parsedFile, RevisionsState.BothMatch),
        null,
        parsedFile,
        Date.now(),
      ),
    )

    return {
      ...baseModel,
      projectContents: updatedProjectContents,
    }
  }, baseModel)

  return updatedProject
}
