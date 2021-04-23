import * as R from 'ramda'
import { EditorAction } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import { EditorModes } from '../components/editor/editor-modes'
import { DerivedState, EditorState, getOpenUIJSFile } from '../components/editor/store/editor-state'
import { scaleImageDimensions, getFrameAndMultiplier } from '../components/images'
import * as TP from '../core/shared/template-path'
import { findElementAtPath, MetadataUtils } from '../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../core/shared/element-template'
import { getUtopiaJSXComponentsFromSuccess } from '../core/model/project-file-utils'
import {
  Imports,
  InstancePath,
  isParseSuccess,
  TemplatePath,
} from '../core/shared/project-file-types'
import { encodeUtopiaDataToHtml, parsePasteEvent, PasteResult } from './clipboard-utils'
import { isLeft } from '../core/shared/either'
import { setLocalClipboardData } from './local-clipboard'
import Utils from './utils'
import { FileResult, ImageResult } from '../core/shared/file-utils'
import { CanvasPoint, CanvasRectangle } from '../core/shared/math-utils'
import json5 = require('json5')
import { fastForEach } from '../core/shared/utils'
import urljoin = require('url-join')
// tslint:disable-next-line:no-var-requires
const ClipboardPolyfill = require('clipboard-polyfill') // stupid .d.ts is malformatted

interface JSXElementCopyData {
  type: 'ELEMENT_COPY'
  elements: JSXElementsJson
  originalTemplatePaths: TemplatePath[]
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
  imports: Imports,
  clipboardData: Array<CopyData>,
  pastedFiles: Array<FileResult>,
  selectedViews: Array<TemplatePath>,
  pasteTargetsToIgnore: TemplatePath[],
  componentMetadata: ElementInstanceMetadataMap,
): Array<EditorAction> {
  try {
    const utopiaActions = Utils.flatMapArray((data: CopyData, i: number) => {
      const elements = json5.parse(data.elements)
      const metadata = data.targetOriginalContextMetadata
      return [EditorActions.pasteJSXElements(elements, data.originalTemplatePaths, metadata)]
    }, clipboardData)
    let insertImageActions: EditorAction[] = []
    if (pastedFiles.length > 0 && componentMetadata != null) {
      const target = MetadataUtils.getTargetParentForPaste(
        imports,
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
  parentPath: TemplatePath | null,
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

export function createClipboardDataFromSelectionNewWorld(
  editor: EditorState,
  derived: DerivedState,
): {
  data: Array<JSXElementCopyData>
  imageFilenames: Array<string>
  plaintext: string
} | null {
  const openUIJSFile = getOpenUIJSFile(editor)
  if (
    openUIJSFile == null ||
    !isParseSuccess(openUIJSFile.fileContents.parsed) ||
    editor.selectedViews.length === 0
  ) {
    return null
  }
  const parseSuccess = openUIJSFile.fileContents.parsed
  const filteredSelectedViews = editor.selectedViews.filter((view) => {
    return R.none((otherView) => TP.isAncestorOf(view, otherView, false), editor.selectedViews)
  })
  const utopiaComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
  const jsxElements = filteredSelectedViews.map((view) => {
    return findElementAtPath(view, utopiaComponents)
  })
  return {
    data: [
      {
        type: 'ELEMENT_COPY',
        elements: json5.stringify(jsxElements),
        originalTemplatePaths: editor.selectedViews,
        targetOriginalContextMetadata: editor.jsxMetadata,
      },
    ],
    imageFilenames: [],
    plaintext: '',
  }
}
