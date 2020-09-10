import * as JSZip from 'jszip'
import { JSZipObject } from 'jszip'
import { isText } from 'istextorbinary'
import { ProjectContents, RevisionsState } from '../shared/project-file-types'
import {
  assetFile,
  codeFile,
  directory,
  fileTypeFromFileName,
  imageFile,
  uiJsFile,
} from './project-file-utils'
import { lintAndParse } from '../workers/parser-printer/parser-printer'
import { assetResultForBase64, getFileExtension, imageResultForBase64 } from '../shared/file-utils'

async function attemptedTextFileLoad(fileName: string, file: JSZipObject): Promise<string | null> {
  const fileBuffer = await file.async('nodebuffer')
  if (isText(fileName, fileBuffer)) {
    return file.async('string')
  } else {
    return null
  }
}

export async function importZippedGitProject(zipped: JSZip): Promise<ProjectContents> {
  let loadedProject: ProjectContents = {}
  let promises: Array<Promise<void>> = []

  const loadFile = async (fileName: string, file: JSZip.JSZipObject) => {
    const shiftedFileName = fileName.replace(/[^\/]*\//, '/') // Github uses the project name and a commit hash as the root directory
    if (file.dir) {
      loadedProject[shiftedFileName] = directory()
    } else {
      const expectedFileType = fileTypeFromFileName(shiftedFileName)
      switch (expectedFileType) {
        case 'ASSET_FILE': {
          const fileType = getFileExtension(shiftedFileName)
          const base64 = await file.async('base64')
          const assetResult = assetResultForBase64(shiftedFileName, base64)
          loadedProject[shiftedFileName] = assetFile()
          // TODO trigger upload
          break
        }
        case 'IMAGE_FILE': {
          const fileType = getFileExtension(shiftedFileName)
          const base64 = await file.async('base64')
          const imageResult = await imageResultForBase64(shiftedFileName, fileType, base64)
          loadedProject[shiftedFileName] = imageFile(
            fileType,
            imageResult.dataUrl,
            imageResult.size.width,
            imageResult.size.height,
            imageResult.hash,
          )
          // TODO trigger upload
          break
        }
        case 'UI_JS_FILE':
        case 'CODE_FILE':
          const loadedFile = await attemptedTextFileLoad(shiftedFileName, file)
          if (loadedFile == null) {
            // FIXME Client should show an error if loading a text file fails
            console.error(`Failed to parse file ${shiftedFileName} as a text file`)
          } else {
            if (expectedFileType === 'UI_JS_FILE') {
              const parsedUIFile = lintAndParse(shiftedFileName, loadedFile)
              loadedProject[shiftedFileName] = uiJsFile(
                parsedUIFile,
                null,
                RevisionsState.BothMatch,
                Date.now(),
              )
            } else {
              loadedProject[shiftedFileName] = codeFile(loadedFile, null)
            }
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
  return loadedProject
}
