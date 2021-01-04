import { fileTypeFromFileName } from '../../core/model/project-file-utils'
import { extractFile, extractImage, FileResult } from '../../core/shared/file-utils'
import { codeFile } from '../../core/shared/project-file-types'
import { notice } from '../common/notices'
import { EditorAction, EditorDispatch } from './action-types'
import * as EditorActions from './actions/action-creators'

async function fileUploadAction(file: File, targetPath: string): Promise<EditorAction> {
  const fileResult = await extractFile(file)
  return fileResultUploadAction(fileResult, targetPath)
}

export function fileResultUploadAction(fileResult: FileResult, targetPath: string): EditorAction {
  switch (fileResult.type) {
    case 'IMAGE_RESULT': {
      const afterSave = EditorActions.saveImageReplace()
      return EditorActions.saveAsset(
        targetPath,
        fileResult.fileType,
        fileResult.base64Bytes,
        fileResult.hash,
        EditorActions.saveImageDetails(fileResult.size, afterSave),
      )
    }
    case 'ASSET_RESULT': {
      let fileType: string = ''
      const splitPath = targetPath.split('.')
      if (splitPath.length > 1) {
        fileType = splitPath[splitPath.length - 1]
      }
      return EditorActions.saveAsset(
        targetPath,
        fileType,
        fileResult.base64Bytes,
        fileResult.hash,
        null,
      )
    }
    case 'TEXT_RESULT': {
      return EditorActions.updateFile(targetPath, codeFile(fileResult.content, null), true)
    }

    default:
      const _exhaustiveCheck: never = fileResult
      throw new Error(`Unhandled type ${JSON.stringify(fileResult)}`)
  }
}

function handleImageSelected(
  files: FileList | null,
  dispatch: EditorDispatch,
  createImageElement: boolean,
) {
  if (files != null && files.length === 1) {
    const file = files[0]
    const imageFilePath = `assets/${file.name}`
    if (fileTypeFromFileName(file.name) === 'IMAGE_FILE') {
      extractImage(file)
        .then((result) => {
          const afterSave = createImageElement
            ? EditorActions.saveImageSwitchMode()
            : EditorActions.saveImageDoNothing()
          const saveImageAction = EditorActions.saveAsset(
            imageFilePath,
            file.type,
            result.base64Bytes,
            result.hash,
            EditorActions.saveImageDetails(result.size, afterSave),
          )
          dispatch([saveImageAction], 'everyone')
        })
        .catch((failure) => {
          console.error(failure)
        })
        .finally(() => {
          removeFileDialogTrigger()
        })
    } else {
      // FIXME Support inserting SVGs by adding an import statement
      fileUploadAction(file, imageFilePath).then((saveFileAction) => {
        const warningMessage = EditorActions.showToast(notice(`File saved to ${imageFilePath}`))
        dispatch([saveFileAction, warningMessage], 'everyone')
      })
    }
  }
}

const fileDialogInputId: string = 'filedialoginput'

let inputElement: HTMLInputElement | null = null

function createFileDialogTrigger(
  dispatch: EditorDispatch,
  createImageElement: boolean,
): HTMLInputElement {
  removeFileDialogTrigger()
  inputElement = document.createElement('input')
  inputElement.id = fileDialogInputId
  inputElement.type = 'file'
  inputElement.accept = 'image/*'
  inputElement.style.display = 'none'
  inputElement.onchange = () => {
    if (inputElement != null) {
      handleImageSelected(inputElement.files, dispatch, createImageElement)
    }
  }
  document.body.appendChild(inputElement)
  return inputElement
}

function removeFileDialogTrigger() {
  if (inputElement != null) {
    document.body.removeChild(inputElement)
  }
  inputElement = null
}

export function insertImage(dispatch: EditorDispatch): void {
  const element = createFileDialogTrigger(dispatch, true)
  element.click()
}
