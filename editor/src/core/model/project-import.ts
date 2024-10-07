import type JSZip from 'jszip'
import type { JSZipObject } from 'jszip'
import { isText } from 'istextorbinary'
import {
  assetFile,
  directory,
  imageFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../shared/project-file-types'
import { fileTypeFromFileName } from './project-file-utils'
import { assetResultForBase64, getFileExtension, imageResultForBase64 } from '../shared/file-utils'
import type { ProjectContentTreeRoot } from '../../components/assets'
import { gitBlobChecksumFromBuffer } from '../shared/file-utils'
import { addFileToProjectContents, walkContentsTreeAsync } from '../../components/assets'
import type { Either } from '../shared/either'
import { isRight, left, right } from '../shared/either'
import type { EditorState, PersistentModel } from '../../components/editor/store/editor-state'
import { contentsJSONURLFromProjectURL } from '../shared/utils'
import { EditorDispatch } from '../../components/editor/action-types'
import { saveAsset } from '../../components/editor/server'
import { forceNotNull } from '../shared/optional-utils'

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
          const buffer = await file.async('nodebuffer')
          const gitBlobChecksum = gitBlobChecksumFromBuffer(buffer)
          const assetResult = assetResultForBase64(shiftedFileName, base64, gitBlobChecksum)
          loadedProject = addFileToProjectContents(
            loadedProject,
            shiftedFileName,
            assetFile(assetResult.base64Bytes, gitBlobChecksum),
          )
          break
        }
        case 'IMAGE_FILE': {
          const fileType = getFileExtension(shiftedFileName)
          const base64 = await file.async('base64')
          const buffer = await file.async('nodebuffer')
          const imageResult = await imageResultForBase64(shiftedFileName, fileType, base64, buffer)
          loadedProject = addFileToProjectContents(
            loadedProject,
            shiftedFileName,
            imageFile(
              fileType,
              imageResult.base64Bytes,
              imageResult.size.width,
              imageResult.size.height,
              imageResult.hash,
              gitBlobChecksumFromBuffer(buffer),
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
                0,
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

export interface ImportFromProjectURLSuccess {
  model: PersistentModel
  originalProjectRootURL: string
}

export async function getURLImportDetails(
  projectURL: string,
): Promise<Either<string, ImportFromProjectURLSuccess>> {
  const possibleContentsURL = contentsJSONURLFromProjectURL(projectURL)
  if (isRight(possibleContentsURL)) {
    const contentsURL = possibleContentsURL.value.contentsURL
    const response = await fetch(contentsURL, {
      method: 'GET',
      credentials: 'include',
      mode: 'cors',
    })

    if (response.ok) {
      const responseJSON = await response.json()
      return right({
        model: responseJSON,
        originalProjectRootURL: possibleContentsURL.value.projectRootURL,
      })
    } else if (response.status === 404) {
      return left(`Unable to find the content.`)
    } else {
      return left(`Server responded with ${response.status} ${response.statusText}`)
    }
  } else {
    return possibleContentsURL
  }
}

export async function reuploadAsset(
  originalProjectRootURL: string,
  editorState: EditorState,
  assetPath: string,
): Promise<void> {
  const response = await fetch(`${originalProjectRootURL}${assetPath}`, {
    method: 'GET',
    credentials: 'include',
    mode: 'cors',
  })
  const blobContent = await response.blob()
  const reader = new window.FileReader()
  reader.readAsDataURL(blobContent)
  const base64 = await new Promise((resolve) => {
    reader.onloadend = () => {
      resolve(reader.result)
    }
  })
  if (typeof base64 === 'string') {
    const fileType = getFileExtension(assetPath)
    await saveAsset(
      forceNotNull('Should have a project ID.', editorState.id),
      fileType,
      base64,
      assetPath,
    )
  } else {
    throw new Error(`Not able to construct base64 of asset as a string.`)
  }
}

export async function reuploadAssets(
  originalProjectRootURL: string,
  editorState: EditorState,
): Promise<void> {
  return walkContentsTreeAsync(editorState.projectContents, async (fullPath, file) => {
    if (file.type === 'ASSET_FILE' || file.type === 'IMAGE_FILE') {
      await reuploadAsset(originalProjectRootURL, editorState, fullPath)
    }
  })
}
