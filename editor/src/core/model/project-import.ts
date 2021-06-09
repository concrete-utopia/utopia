import * as JSZip from 'jszip'
import { JSZipObject } from 'jszip'
import { isText } from 'istextorbinary'
import { RevisionsState, textFile, textFileContents, unparsed } from '../shared/project-file-types'
import { assetFile, directory, fileTypeFromFileName, imageFile } from './project-file-utils'
import { assetResultForBase64, getFileExtension, imageResultForBase64 } from '../shared/file-utils'
import { Size } from '../shared/math-utils'
import { addFileToProjectContents, ProjectContentTreeRoot } from '../../components/assets'

async function attemptedTextFileLoad(fileName: string, file: JSZipObject): Promise<string | null> {
  const fileBuffer = await file.async('nodebuffer')
  if (isText(fileName, fileBuffer)) {
    return file.async('string')
  } else {
    return null
  }
}

export interface UnsavedAsset {
  fileName: string
  fileType: string
  base64: string
  hash: number
  size: Size | null
}

export interface ProjectImportSuccess {
  type: 'SUCCESS'
  projectName: string
  contents: ProjectContentTreeRoot
  assetsToUpload: Array<UnsavedAsset>
}

export interface ProjectImportFailure {
  type: 'FAILURE'
  errorMessage: string
}

function projectImportSuccess(
  projectName: string,
  contents: ProjectContentTreeRoot,
  assetsToUpload: Array<UnsavedAsset>,
): ProjectImportSuccess {
  return {
    type: 'SUCCESS',
    projectName: projectName,
    contents: contents,
    assetsToUpload: assetsToUpload,
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
  let assetsToUpload: Array<UnsavedAsset> = []
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
          const fileType = getFileExtension(shiftedFileName)
          const base64 = await file.async('base64')
          const assetResult = assetResultForBase64(shiftedFileName, base64)
          assetsToUpload.push({
            fileName: assetResult.filename,
            fileType: fileType,
            base64: assetResult.base64Bytes,
            hash: assetResult.hash,
            size: null,
          })
          loadedProject = addFileToProjectContents(loadedProject, shiftedFileName, assetFile())
          break
        }
        case 'IMAGE_FILE': {
          const fileType = getFileExtension(shiftedFileName)
          const base64 = await file.async('base64')
          const imageResult = await imageResultForBase64(shiftedFileName, fileType, base64)
          assetsToUpload.push({
            fileName: imageResult.filename,
            fileType: fileType,
            base64: imageResult.base64Bytes,
            hash: imageResult.hash,
            size: imageResult.size,
          })
          loadedProject = addFileToProjectContents(
            loadedProject,
            shiftedFileName,
            imageFile(undefined, undefined, undefined, undefined, imageResult.hash),
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
    return projectImportSuccess(projectName, loadedProject, assetsToUpload)
  }
}
