import JSZip from 'jszip'
import { JSZipObject } from 'jszip'
import { isText } from 'istextorbinary'
import { RevisionsState, textFile, textFileContents, unparsed } from '../shared/project-file-types'
import { assetFile, directory, fileTypeFromFileName, imageFile } from './project-file-utils'
import { assetResultForBase64, getFileExtension, imageResultForBase64 } from '../shared/file-utils'
import { addFileToProjectContents, ProjectContentTreeRoot } from '../../components/assets'

async function attemptedTextFileLoad(fileName: string, file: JSZipObject): Promise<string | null> {
  const fileBuffer = await file.async('nodebuffer')
  if (isText(fileName, fileBuffer)) {
    return file.async('string')
  } else {
    return null
  }
}

export interface ProjectImportSuccess {
  type: 'SUCCESS'
  projectName: string
  contents: ProjectContentTreeRoot
}

export interface ProjectImportFailure {
  type: 'FAILURE'
  errorMessage: string
}

function projectImportSuccess(
  projectName: string,
  contents: ProjectContentTreeRoot,
): ProjectImportSuccess {
  return {
    type: 'SUCCESS',
    projectName: projectName,
    contents: contents,
  }
}

function projectImportFailure(errorMessage: string): ProjectImportFailure {
  return {
    type: 'FAILURE',
    errorMessage: errorMessage,
  }
}

export type ProjectImportResult = ProjectImportSuccess | ProjectImportFailure

export function isProjectImportSuccess(
  result: ProjectImportResult,
): result is ProjectImportSuccess {
  return result.type === 'SUCCESS'
}

export function isProjectImportFailure(
  result: ProjectImportResult,
): result is ProjectImportFailure {
  return result.type === 'FAILURE'
}

export async function importZippedGitProject(
  projectName: string,
  zipped: JSZip,
): Promise<ProjectImportResult> {
  let loadedProject: ProjectContentTreeRoot = {}
  let promises: Array<Promise<void>> = []
  let errors: Array<string> = []

  const loadFile = async (fileName: string, file: JSZip.JSZipObject) => {
    const shiftedFileName = fileName.replace(/[^\/]*\//, '/') // Github uses the project name and a commit hash as the root directory
    if (shiftedFileName.trim() === '/') {
      // We don't need to create a root directory
      return
    }
    if (file.dir) {
      loadedProject = addFileToProjectContents(loadedProject, shiftedFileName, directory())
    } else {
      const expectedFileType = fileTypeFromFileName(shiftedFileName)
      switch (expectedFileType) {
        case 'ASSET_FILE': {
          const base64 = await file.async('base64')
          const assetResult = assetResultForBase64(shiftedFileName, base64)
          loadedProject = addFileToProjectContents(
            loadedProject,
            shiftedFileName,
            assetFile(assetResult.base64Bytes),
          )
          break
        }
        case 'IMAGE_FILE': {
          const fileType = getFileExtension(shiftedFileName)
          const base64 = await file.async('base64')
          const imageResult = await imageResultForBase64(shiftedFileName, fileType, base64)
          loadedProject = addFileToProjectContents(
            loadedProject,
            shiftedFileName,
            imageFile(
              fileType,
              imageResult.base64Bytes,
              imageResult.size.width,
              imageResult.size.height,
              imageResult.hash,
            ),
          )
          break
        }
        case 'TEXT_FILE':
          const loadedFile = await attemptedTextFileLoad(shiftedFileName, file)
          if (loadedFile == null) {
            errors.push(`Unable to parse file ${shiftedFileName} as a text file`)
          } else {
            loadedProject = addFileToProjectContents(
              loadedProject,
              shiftedFileName,
              textFile(
                textFileContents(loadedFile, unparsed, RevisionsState.CodeAhead),
                null,
                null,
                Date.now(),
              ),
            )
          }
          break
        default:
          const _exhaustiveCheck: never = expectedFileType
          throw new Error(`Unknown file type ${expectedFileType}`)
      }
    }
  }

  zipped.forEach((fileName, file) => {
    promises.push(loadFile(fileName, file))
  })

  await Promise.all(promises)

  if (errors.length > 0) {
    return projectImportFailure(errors.join(`\n`))
  } else {
    return projectImportSuccess(projectName, loadedProject)
  }
}
