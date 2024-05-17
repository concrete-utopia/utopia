import CanvasActions from '../components/canvas/canvas-actions'
import * as EditorActions from '../components/editor/actions/action-creators'
import * as PP from '../core/shared/property-path'
import type { EditorAction, EditorDispatch, LoginState } from '../components/editor/action-types'
import { isLoggedIn } from '../components/editor/action-types'
import type { InsertionSubject } from '../components/editor/editor-modes'
import { EditorModes, insertionSubject } from '../components/editor/editor-modes'
import type { ImageResult } from '../core/shared/file-utils'
import type { CanvasPoint, Size } from '../core/shared/math-utils'
import type { ElementPath } from '../core/shared/project-file-types'
import { imageFile } from '../core/shared/project-file-types'
import { fastForEach } from '../core/shared/utils'
import { createDirectInsertImageActions, Clipboard } from '../utils/clipboard'
import { imagePathURL } from '../common/server'
import type { ProjectContentTreeRoot } from '../components/assets'
import { createJsxImage, getFrameAndMultiplier } from '../components/images'
import type React from 'react'
import type { CanvasPositions } from '../components/canvas/canvas-types'
import type { AllElementProps, EditorState } from '../components/editor/store/editor-state'
import { notDragging } from '../components/editor/store/editor-state'
import { uniqueProjectContentID } from '../core/model/project-file-utils'
import type { AssetToSave } from '../components/editor/server'
import { notice } from '../components/common/notice'
import { arrayToObject, mapDropNulls, stripNulls } from '../core/shared/array-utils'
import { optionalMap } from '../core/shared/optional-utils'
import { emptyComments, jsExpressionValue } from '../core/shared/element-template'
import { fromString } from '../core/shared/element-path'
import { childInsertionPath } from '../components/editor/store/insertion-path'
import { generateUID } from '../core/shared/uid-utils'

export async function getPastedImages(dataTransfer: DataTransfer): Promise<ImageResult[]> {
  const result = await Clipboard.parseClipboardData(dataTransfer)
  // Snip out the images only from the result.
  let pastedImages: Array<ImageResult> = []
  fastForEach(result.files, (pastedFile) => {
    if (pastedFile.type === 'IMAGE_RESULT') {
      pastedImages.push({
        ...pastedFile,
        filename: `/assets/${pastedFile.filename}`,
      })
    }
  })
  return pastedImages
}

interface InsertImageContext {
  scale: number
  mousePosition: CanvasPoint
  dispatch: EditorDispatch
  elementPath: ElementPath
}

export async function insertImageFromClipboard(
  dataTransfer: DataTransfer,
  context: InsertImageContext,
): Promise<void> {
  context.dispatch(
    [
      CanvasActions.clearInteractionSession(false),
      EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
    ],
    'everyone',
  )
  const pastedImages = await getPastedImages(dataTransfer)

  const actions = createDirectInsertImageActions(
    pastedImages,
    context.mousePosition,
    context.scale,
    childInsertionPath(context.elementPath),
  )

  context.dispatch(actions, 'everyone')
}

export interface DropContext {
  saveAssets: (projectId: string, assets: AssetToSave[]) => Promise<Array<string | null>>
  mousePosition: CanvasPositions
  editor: () => EditorState
  dispatch: EditorDispatch
  loginState: LoginState
  scale: number
}

async function onDrop(
  event: React.DragEvent,
  cont: () => void,
  context: DropContext,
): Promise<void> {
  if (context.editor().mode.type === 'select' && event.dataTransfer != null) {
    const insertionTarget = context.editor().highlightedViews[0]
    void insertImageFromClipboard(event.dataTransfer, {
      scale: context.scale,
      dispatch: context.dispatch,
      mousePosition: context.mousePosition.canvasPositionRounded,
      elementPath: insertionTarget,
    })
  } else if (context.editor().mode.type === 'insert' && event.dataTransfer != null) {
    const images = await getPastedImages(event.dataTransfer)
    if (images.length === 0) {
      return context.dispatch([
        CanvasActions.clearInteractionSession(false),
        EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
        EditorActions.showToast({
          id: 'image-drag-drop',
          level: 'WARNING',
          message: "Didn't find any images to insert",
          persistent: false,
        }),
      ])
    }

    const projectId = context.editor().id
    if (projectId == null) {
      return
    }

    const { actions, subjects, assetInfo } = actionsForDroppedImages(
      images,
      {
        scale: context.scale,
        projectContents: context.editor().projectContents,
        loginState: context.loginState,
        mousePosition: context.mousePosition.canvasPositionRounded,
      },
      'autoincrement',
    )

    context.dispatch(
      [
        ...actions,
        EditorActions.switchEditorMode(EditorModes.insertMode(subjects)),
        EditorActions.setImageDragSessionState(notDragging()),
      ],
      'everyone',
    )
    cont()

    await context
      .saveAssets(projectId, assetInfo)
      .then((checksums) => {
        const substitutionPaths = stripNulls(
          assetInfo.flatMap((i) =>
            i.projectPath == null ? [] : [{ uid: i.uid, path: i.projectPath }],
          ),
        )

        const srcUpdateActions = updateImageSrcsActions(
          context.editor().allElementProps,
          substitutionPaths,
        )

        const openFileName = context.editor().canvas.openFile?.filename
        const openFileActions =
          openFileName == null ? [] : [EditorActions.openCodeEditorFile(openFileName, false)]

        context.dispatch([
          ...srcUpdateActions,
          EditorActions.showToast(notice('Succesfully uploaded assets')),
          ...openFileActions,
        ])
      })
      .catch(() => {
        const deleteFileActions = stripNulls(
          assetInfo.map((info) => optionalMap(EditorActions.deleteFile, info.projectPath)),
        )
        context.dispatch([
          ...deleteFileActions,
          EditorActions.showToast(notice('Error uploading assets', 'ERROR')),
        ])
      })
  }
}

interface ActionsForDroppedImageContext {
  generateUid: () => string
  scale: number
  isUserLoggedIn: boolean
  mousePosition: CanvasPoint
}

interface ActionForDroppedImageResult {
  actions: EditorAction[]
  singleSubject: InsertionSubject
  imageAssetInfo: ImageAssetSaveInfo
}

interface ImageAssetSaveInfo {
  uid: string
  fileType: string
  base64: string
  fileName: string
  projectPath: string | null
}

function actionsForDroppedImage(
  image: ImageResult,
  context: ActionsForDroppedImageContext,
): ActionForDroppedImageResult {
  const { frame } = getFrameAndMultiplier(context.mousePosition, image.filename, image.size, null)

  const projectFile = imageFile(
    image.fileType,
    undefined,
    image.size.width,
    image.size.height,
    image.hash,
    image.gitBlobSha,
  )

  const { saveImageActions, src } = context.isUserLoggedIn
    ? {
        saveImageActions: [EditorActions.updateFile(image.filename, projectFile, true)],
        src: image.filename,
      }
    : { saveImageActions: [], src: null }

  const newUID = context.generateUid()

  const defaultSize: Size = {
    width: 200,
    height: 200,
  }

  const elementSize: Size = {
    width: frame.width ?? defaultSize.width,
    height: frame.height ?? defaultSize.height,
  }

  const newElement = createJsxImage(newUID, {
    width: elementSize.width,
    height: elementSize.height,
    top: context.mousePosition.y,
    left: context.mousePosition.x,
    src: image.base64Bytes,
  })
  return {
    actions: saveImageActions,
    singleSubject: insertionSubject(newUID, newElement, elementSize, {}, null, false, null),
    imageAssetInfo: {
      uid: newUID,
      fileType: image.fileType,
      base64: image.base64Bytes,
      fileName: imagePathURL(image.filename),
      projectPath: optionalMap(imagePathURL, src),
    },
  }
}

interface ActionsForDroppedImagesResult {
  subjects: Array<InsertionSubject>
  actions: Array<EditorAction>
  assetInfo: Array<ImageAssetSaveInfo>
}

interface ActionsForDroppedImagesContext {
  projectContents: ProjectContentTreeRoot
  mousePosition: CanvasPoint
  scale: number
  loginState: LoginState
}

function actionsForDroppedImages(
  images: Array<ImageResult>,
  context: ActionsForDroppedImagesContext,
  overwriteExistingFile: 'overwrite' | 'autoincrement',
): ActionsForDroppedImagesResult {
  let actions: Array<EditorAction> = []
  let uidsSoFar: Array<string> = []
  let subjects: Array<InsertionSubject> = []
  let assetInfo: Array<ImageAssetSaveInfo> = []

  for (const image of images) {
    const filename =
      overwriteExistingFile === 'autoincrement'
        ? uniqueProjectContentID(image.filename, context.projectContents)
        : image.filename
    const {
      actions: actionsForImage,
      singleSubject,
      imageAssetInfo,
    } = actionsForDroppedImage(
      {
        ...image,
        filename: filename,
      },
      {
        generateUid: () => generateUID(),
        scale: context.scale,
        mousePosition: context.mousePosition,
        isUserLoggedIn: isLoggedIn(context.loginState),
      },
    )

    actions = [...actions, ...actionsForImage]
    uidsSoFar = [...uidsSoFar, singleSubject.uid]
    subjects = [...subjects, singleSubject]
    assetInfo = [...assetInfo, imageAssetInfo]
  }

  return { actions, subjects, assetInfo }
}

interface SrcSubstitutionData {
  path: string
  uid: string
}

function updateImageSrcsActions(
  allElementProps: AllElementProps,
  srcs: Array<SrcSubstitutionData>,
): Array<EditorAction> {
  const srcsIndex = arrayToObject(srcs, (s) => s.uid)

  return mapDropNulls(([path, props]) => {
    const maybeImageUpdateData = srcsIndex[props['data-uid']]
    return maybeImageUpdateData == null
      ? null
      : EditorActions.setProp_UNSAFE(
          fromString(path),
          PP.create('src'),
          jsExpressionValue(maybeImageUpdateData.path, emptyComments),
        )
  }, Object.entries(allElementProps))
}

export const DropHandlers = {
  onDrop,
}
