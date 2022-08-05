import { EditorAction } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import { EditorModes } from '../components/editor/editor-modes'
import {
  DerivedState,
  EditorState,
  getOpenUIJSFileKey,
  withUnderlyingTarget,
} from '../components/editor/store/editor-state'
import { getFrameAndMultiplier } from '../components/images'
import * as EP from '../core/shared/element-path'
import { findElementAtPath, MetadataUtils } from '../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../core/shared/element-template'
import { getUtopiaJSXComponentsFromSuccess } from '../core/model/project-file-utils'
import {
  isParseSuccess,
  NodeModules,
  ElementPath,
  isTextFile,
} from '../core/shared/project-file-types'
import { encodeUtopiaDataToHtml, parsePasteEvent, PasteResult } from './clipboard-utils'
import { setLocalClipboardData } from './local-clipboard'
import Utils from './utils'
import { FileResult, ImageResult } from '../core/shared/file-utils'
import { CanvasPoint } from '../core/shared/math-utils'
import * as json5 from 'json5'
import { fastForEach } from '../core/shared/utils'
import urljoin from 'url-join'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../components/assets'
import {
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../components/custom-code/code-file'
import { mapDropNulls } from '../core/shared/array-utils'
import ClipboardPolyfill from 'clipboard-polyfill'
import { mapValues, omit, pick } from '../core/shared/object-utils'
import { metaProperty } from '@babel/types'

interface JSXElementCopyData {
  type: 'ELEMENT_COPY'
  elements: JSXElementsJson
  originalElementPaths: ElementPath[]
  targetOriginalContextMetadata: ElementInstanceMetadataMap
}

type JSXElementsJson = string

export type CopyData = JSXElementCopyData

export function parseClipboardData(clipboardData: DataTransfer | null): Promise<PasteResult> {
  return parsePasteEvent(clipboardData)
}

export function setClipboardData(
  copyData: {
    data: Array<CopyData>
    plaintext: string
    imageFilenames: Array<string>
  } | null,
): void {
  // we also set the local clipboard here, used for style copy paste
  setLocalClipboardData(copyData)

  if (copyData != null) {
    const utopiaDataHtml = encodeUtopiaDataToHtml(copyData.data)
    const dt = new ClipboardPolyfill.DT()
    dt.setData('text/plain', copyData.plaintext)
    dt.setData('text/html', utopiaDataHtml)
    ClipboardPolyfill.write(dt)
  }
}

export function getActionsForClipboardItems(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  clipboardData: Array<CopyData>,
  pastedFiles: Array<FileResult>,
  selectedViews: Array<ElementPath>,
  pasteTargetsToIgnore: ElementPath[],
  componentMetadata: ElementInstanceMetadataMap,
): Array<EditorAction> {
  try {
    const utopiaActions = Utils.flatMapArray((data: CopyData, i: number) => {
      const elements = json5.parse(data.elements)
      const metadata = data.targetOriginalContextMetadata
      return [EditorActions.pasteJSXElements(elements, data.originalElementPaths, metadata)]
    }, clipboardData)
    let insertImageActions: EditorAction[] = []
    if (pastedFiles.length > 0 && componentMetadata != null) {
      const target = getTargetParentForPaste(
        projectContents,
        openFile,
        selectedViews,
        componentMetadata,
        pasteTargetsToIgnore,
      )
      const parentFrame =
        target != null ? MetadataUtils.getFrameInCanvasCoords(target, componentMetadata) : null
      const parentCenter =
        parentFrame != null
          ? Utils.getRectCenter(parentFrame)
          : (Utils.point(100, 100) as CanvasPoint)
      const imageSizeMultiplier = 2
      let pastedImages: Array<ImageResult> = []
      fastForEach(pastedFiles, (pastedFile) => {
        if (pastedFile.type === 'IMAGE_RESULT') {
          pastedImages.push({
            ...pastedFile,
            filename: urljoin('/assets/clipboard', pastedFile.filename),
          })
        }
      })
      insertImageActions = createDirectInsertImageActions(
        pastedImages,
        parentCenter,
        target,
        imageSizeMultiplier,
      )
    }
    return [...utopiaActions, ...insertImageActions]
  } catch (e) {
    console.warn('No valid momentum data found on clipboard:', e)
    return []
  }
}

export function createDirectInsertImageActions(
  images: Array<ImageResult>,
  centerPoint: CanvasPoint,
  parentPath: ElementPath | null,
  overrideDefaultMultiplier: number | null,
): Array<EditorAction> {
  if (images.length === 0) {
    return []
  } else {
    return [
      EditorActions.switchEditorMode(EditorModes.selectMode()),
      ...Utils.flatMapArray((image) => {
        const { frame, multiplier } = getFrameAndMultiplier(
          centerPoint,
          image.filename,
          image.size,
          overrideDefaultMultiplier,
        )
        const insertWith = EditorActions.saveImageInsertWith(parentPath, frame, multiplier)
        const saveImageAction = EditorActions.saveAsset(
          image.filename,
          image.fileType,
          image.base64Bytes,
          image.hash,
          EditorActions.saveImageDetails(image.size, insertWith),
        )
        return [saveImageAction]
      }, images),
    ]
  }
}

export function createClipboardDataFromSelection(editor: EditorState): {
  data: Array<JSXElementCopyData>
  imageFilenames: Array<string>
  plaintext: string
} | null {
  const openUIJSFileKey = getOpenUIJSFileKey(editor)
  if (openUIJSFileKey == null || editor.selectedViews.length === 0) {
    return null
  }
  const filteredSelectedViews = editor.selectedViews.filter((view) => {
    return editor.selectedViews.every((otherView) => !EP.isDescendantOf(view, otherView))
  })
  const jsxElements = mapDropNulls((target) => {
    const underlyingTarget = normalisePathToUnderlyingTarget(
      editor.projectContents,
      editor.nodeModules.files,
      openUIJSFileKey,
      target,
    )
    const targetPathSuccess = normalisePathSuccessOrThrowError(underlyingTarget)
    const projectFile = getContentsTreeFileFromString(
      editor.projectContents,
      targetPathSuccess.filePath,
    )
    if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
      const components = getUtopiaJSXComponentsFromSuccess(projectFile.fileContents.parsed)
      return findElementAtPath(target, components)
    } else {
      return null
    }
  }, filteredSelectedViews)

  return {
    data: [
      {
        type: 'ELEMENT_COPY',
        elements: json5.stringify(jsxElements),
        originalElementPaths: editor.selectedViews,
        targetOriginalContextMetadata: filterMetadataForCopy(
          editor.selectedViews,
          editor.jsxMetadata,
        ),
      },
    ],
    imageFilenames: [],
    plaintext: '',
  }
}

function filterMetadataForCopy(
  selectedViews: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
): ElementInstanceMetadataMap {
  const allPaths = Object.keys(jsxMetadata)
  const necessaryPaths = allPaths.filter((p) => {
    const elementPath = EP.fromString(p)
    // only those element paths are relevant which are descendants or ascentors of at least one selected view
    return selectedViews.some(
      (selected) =>
        EP.isDescendantOf(selected, elementPath) || EP.isDescendantOf(elementPath, selected),
    )
  })
  const filteredMetadata = pick(necessaryPaths, jsxMetadata)
  // The static props in metadata are not necessary for copy paste, and they are huge, deep objects
  // Embedding the props can cause two different kinds of exceptions when json stringified:
  // 1. props can contain circular references
  // 2. props can contain the Window object, which throws a DOMException when stringified
  const filteredMetadataWithoutProps = mapValues(
    (meta) => ({
      ...meta,
      props: {},
    }),
    filteredMetadata,
  )
  return filteredMetadataWithoutProps
}

export function getTargetParentForPaste(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  selectedViews: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  pasteTargetsToIgnore: ElementPath[],
): ElementPath | null {
  if (selectedViews.length > 0) {
    const parentTarget = EP.getCommonParent(selectedViews, true)
    if (parentTarget == null) {
      return null
    } else {
      // we should not paste the source into itself
      const insertingSourceIntoItself = EP.containsPath(parentTarget, pasteTargetsToIgnore)

      if (openFile == null) {
        return null
      } else {
        if (
          MetadataUtils.targetSupportsChildren(projectContents, openFile, metadata, parentTarget) &&
          !insertingSourceIntoItself
        ) {
          return parentTarget
        } else {
          const parentOfSelected = EP.parentPath(parentTarget)
          if (
            MetadataUtils.targetSupportsChildren(
              projectContents,
              openFile,
              metadata,
              parentOfSelected,
            )
          ) {
            return parentOfSelected
          } else {
            return null
          }
        }
      }
    }
  } else {
    return null
  }
}
