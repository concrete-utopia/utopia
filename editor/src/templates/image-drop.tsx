import CanvasActions from '../components/canvas/canvas-actions'
import * as EditorActions from '../components/editor/actions/action-creators'
import {
  EditorAction,
  EditorDispatch,
  isLoggedIn,
  LoginState,
} from '../components/editor/action-types'
import { EditorModes, InsertionSubject, insertionSubject } from '../components/editor/editor-modes'
import { ImageResult } from '../core/shared/file-utils'
import { CanvasPoint, resize, Size, size } from '../core/shared/math-utils'
import { ElementPath } from '../core/shared/project-file-types'
import { fastForEach } from '../core/shared/utils'
import { createDirectInsertImageActions, parseClipboardData } from '../utils/clipboard'
import { imagePathURL } from '../common/server'
import { ProjectContentTreeRoot } from '../components/assets'
import { getFrameAndMultiplierWithResize, createJsxImage } from '../components/images'
import { generateUidWithExistingComponentsAndExtraUids } from '../core/model/element-template-utils'
import React from 'react'
import { CanvasPositions } from '../components/canvas/canvas-types'
import { EditorState } from '../components/editor/store/editor-state'

export async function getPastedImages(dataTransfer: DataTransfer): Promise<ImageResult[]> {
  const result = await parseClipboardData(dataTransfer)
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
      EditorActions.switchEditorMode(EditorModes.selectMode()),
    ],
    'everyone',
  )
  const pastedImages = await getPastedImages(dataTransfer)

  const actions = createDirectInsertImageActions(
    pastedImages,
    context.mousePosition,
    context.scale,
    context.elementPath,
  )

  context.dispatch(actions, 'everyone')
}

interface DropContext {
  mousePosition: CanvasPositions
  editor: EditorState
  dispatch: EditorDispatch
  loginState: LoginState
  scale: number
}

export function onDrop(event: React.DragEvent, cont: () => void, context: DropContext): void {
  if (context.editor.mode.type === 'select' && event.dataTransfer != null) {
    const insertionTarget = context.editor.highlightedViews[0]
    void insertImageFromClipboard(event.dataTransfer, {
      scale: context.scale,
      dispatch: context.dispatch,
      mousePosition: context.mousePosition.canvasPositionRounded,
      elementPath: insertionTarget,
    })
  } else if (context.editor.mode.type === 'insert' && event.dataTransfer != null) {
    void getPastedImages(event.dataTransfer).then((images) => {
      if (images.length === 0) {
        return context.dispatch([
          CanvasActions.clearInteractionSession(false),
          EditorActions.switchEditorMode(EditorModes.selectMode()),
          EditorActions.showToast({
            id: 'image-drag-drop',
            level: 'WARNING',
            message: "Didn't find any images to insert",
            persistent: false,
          }),
        ])
      }

      const { actions, subjects } = actionsForDroppedImages(images, {
        scale: context.scale,
        projectContents: context.editor.projectContents,
        loginState: context.loginState,
        mousePosition: context.mousePosition.canvasPositionRounded,
      })

      context.dispatch(
        [...actions, EditorActions.switchEditorMode(EditorModes.insertMode(subjects))],
        'everyone',
      )
      cont()
    })
  }
  return
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
}

function actionsForDroppedImage(
  image: ImageResult,
  context: ActionsForDroppedImageContext,
): ActionForDroppedImageResult {
  const { frame } = getFrameAndMultiplierWithResize(
    context.mousePosition,
    image.filename,
    image.size,
    context.scale,
  )

  const { saveImageActions, src } = context.isUserLoggedIn
    ? {
        saveImageActions: [
          EditorActions.saveAsset(
            image.filename,
            image.fileType,
            image.base64Bytes,
            image.hash,
            EditorActions.saveImageDetails(image.size, EditorActions.saveImageReplace()),
          ),
        ],
        src: imagePathURL(image.filename),
      }
    : { saveImageActions: [], src: image.base64Bytes }

  const newUID = context.generateUid()
  const elementSize: Size = resize(
    size(frame.width ?? 100, frame.height ?? 100),
    size(200, 200),
    'keep-aspect-ratio',
  )
  const newElement = createJsxImage(newUID, {
    width: elementSize.width,
    height: elementSize.height,
    top: context.mousePosition.y,
    left: context.mousePosition.x,
    src: src,
  })
  return {
    actions: saveImageActions,
    singleSubject: insertionSubject(newUID, newElement, elementSize, {}, null),
  }
}

interface ActionsForDroppedImagesResult {
  subjects: Array<InsertionSubject>
  actions: Array<EditorAction>
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
): ActionsForDroppedImagesResult {
  let actions: Array<EditorAction> = []
  let uidsSoFar: Array<string> = []
  let subjects: Array<InsertionSubject> = []
  for (const image of images) {
    const { actions: actionsForImage, singleSubject } = actionsForDroppedImage(image, {
      generateUid: () =>
        generateUidWithExistingComponentsAndExtraUids(context.projectContents, uidsSoFar),
      scale: context.scale,
      mousePosition: context.mousePosition,
      isUserLoggedIn: isLoggedIn(context.loginState),
    })
    actions = [...actions, ...actionsForImage]
    uidsSoFar = [...uidsSoFar, singleSubject.uid]
    subjects = [...subjects, singleSubject]
  }

  return { actions, subjects }
}
