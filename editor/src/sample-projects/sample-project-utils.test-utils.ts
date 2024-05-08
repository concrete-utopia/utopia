import type { ProjectContentsTree, ProjectContentTreeRoot } from '../components/assets'
import {
  addFileToProjectContents,
  projectContentFile,
  transformContentsTree,
} from '../components/assets'
import type { PersistentModel } from '../components/editor/store/editor-state'
import { StoryboardFilePath } from '../components/editor/store/editor-state'
import type { ParsedTextFile, ParseFailure, ParseSuccess } from '../core/shared/project-file-types'
import {
  isParseSuccess,
  RevisionsState,
  textFile,
  textFileContents,
  isParseFailure,
  isUnparsed,
  unparsed,
} from '../core/shared/project-file-types'
import { emptySet } from '../core/shared/set-utils'
import type { SteganographyMode } from '../core/workers/parser-printer/parser-printer'
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

export function complexDefaultProjectPreParsed(dummyComponent: string = 'Spring'): PersistentModel {
  const project = complexDefaultProject(dummyComponent)
  const updatedProjectContents = parseProjectContents(project.projectContents)

  return {
    ...project,
    projectContents: updatedProjectContents,
  }
}

export function parseProjectContents(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  let alreadyExistingUIDs: Set<string> = emptySet()
  return transformContentsTree(projectContents, (tree: ProjectContentsTree) => {
    if (tree.type === 'PROJECT_CONTENT_FILE') {
      const file = tree.content
      if (file.type === 'TEXT_FILE') {
        const parsed = lintAndParse(
          tree.fullPath,
          file.fileContents.code,
          null,
          alreadyExistingUIDs,
          'trim-bounds',
          'do-not-apply-steganography',
        )
        const updatedFile = textFile(
          textFileContents(file.fileContents.code, parsed, RevisionsState.BothMatch),
          null,
          isParseSuccess(parsed) ? parsed : null,
          file.versionNumber + 1,
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

export function getParseSuccessForStoryboardCode(
  appUiJsFile: string,
  applySteganography: SteganographyMode = 'do-not-apply-steganography',
): ParseSuccess {
  const parsedFile = lintAndParse(
    StoryboardFilePath,
    appUiJsFile,
    null,
    emptySet(),
    'trim-bounds',
    applySteganography,
  )

  if (isParseFailure(parsedFile)) {
    const failure =
      parsedFile.errorMessage ?? parsedFile.errorMessages.map((m) => m.message).join(`,\n`)
    throw new Error(`createTestProjectWithCode file parsing failed: ${failure}`)
  } else if (isUnparsed(parsedFile)) {
    throw new Error(`createTestProjectWithCode: Unexpected unparsed file.`)
  }
  return parsedFile
}

export function createTestProjectWithCode(
  appUiJsFile: string,
  applySteganography: SteganographyMode = 'do-not-apply-steganography',
): PersistentModel {
  const baseModel = complexDefaultProject()
  const parsedFile: ParseSuccess = getParseSuccessForStoryboardCode(appUiJsFile, applySteganography)

  return {
    ...baseModel,
    projectContents: addFileToProjectContents(
      baseModel.projectContents,
      StoryboardFilePath,
      textFile(
        textFileContents(appUiJsFile, parsedFile, RevisionsState.BothMatch),
        null,
        parsedFile,
        0,
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
          0,
        ),
      ),
    }
  }, baseModel)
}

export function createModifiedProject(
  modifiedFiles: { [filename: string]: string },
  baseModel = complexDefaultProject(),
) {
  const updatedProject = Object.keys(modifiedFiles).reduce((workingProject, modifiedFilename) => {
    const fileParseResult = modifiedFilename.endsWith('.json')
      ? unparsed
      : (lintAndParse(
          modifiedFilename,
          modifiedFiles[modifiedFilename],
          null,
          emptySet(),
          'trim-bounds',
          'do-not-apply-steganography',
        ) as ParsedTextFile)
    if (isParseFailure(fileParseResult)) {
      const failure =
        fileParseResult.errorMessage ??
        fileParseResult.errorMessages.map((m) => m.message).join(`,\n`)
      throw new Error(`The test file parse failed ${modifiedFilename}, ${failure}`)
    }

    const updatedProjectContents = addFileToProjectContents(
      workingProject.projectContents,
      modifiedFilename,
      textFile(
        textFileContents(
          modifiedFiles[modifiedFilename],
          fileParseResult,
          RevisionsState.BothMatch,
        ),
        null,
        fileParseResult.type === 'PARSE_SUCCESS' ? fileParseResult : null,
        0,
      ),
    )

    return {
      ...baseModel,
      projectContents: updatedProjectContents,
    }
  }, baseModel)

  return updatedProject
}
