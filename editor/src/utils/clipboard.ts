import type { EditorAction, ElementPaste } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import { EditorModes } from '../components/editor/editor-modes'
import type {
  AllElementProps,
  EditorState,
  PastePostActionMenuData,
} from '../components/editor/store/editor-state'
import {
  getElementFromProjectContents,
  getOpenUIJSFileKey,
} from '../components/editor/store/editor-state'
import { getFrameAndMultiplier } from '../components/images'
import * as EP from '../core/shared/element-path'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import { type ElementInstanceMetadataMap } from '../core/shared/element-template'
import type { ElementPath } from '../core/shared/project-file-types'
import { isParseSuccess, isTextFile } from '../core/shared/project-file-types'
import type { PasteResult } from './clipboard-utils'
import { extractFiles, extractUtopiaDataFromClipboardData } from './clipboard-utils'
import Utils from './utils'
import type { FileResult, ImageResult } from '../core/shared/file-utils'
import type { CanvasPoint } from '../core/shared/math-utils'
import { isInfinityRectangle } from '../core/shared/math-utils'
import { fastForEach } from '../core/shared/utils'
import urljoin from 'url-join'
import type { ProjectContentTreeRoot } from '../components/assets'
import { getProjectFileByFilePath } from '../components/assets'
import type { PropertyControlsInfo } from '../components/custom-code/code-file'
import {
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../components/custom-code/code-file'
import { mapDropNulls } from '../core/shared/array-utils'
import ClipboardPolyfill from 'clipboard-polyfill'
import { mapValues, pick } from '../core/shared/object-utils'
import { getStoryboardElementPath } from '../core/model/scene-utils'
import { getRequiredImportsForElement } from '../components/editor/import-utils'
import type { BuiltInDependencies } from '../core/es-modules/package-manager/built-in-dependencies-list'
import type { InsertionPath } from '../components/editor/store/insertion-path'
import type { ElementPathTrees } from '../core/shared/element-path-tree'
import { replaceJSXElementCopyData } from '../components/canvas/canvas-strategies/strategies/reparent-helpers/reparent-helpers'
import {
  PropsPreservedPastePostActionChoice,
  PropsReplacedPastePostActionChoice,
} from '../components/canvas/canvas-strategies/post-action-options/post-action-paste'
import { isLeft } from '../core/shared/either'
import { notice } from '../components/common/notice'
import { getTargetParentForPaste } from '../components/canvas/canvas-strategies/strategies/reparent-utils'

export interface ElementPasteWithMetadata {
  elements: ElementPaste[]
  targetOriginalContextMetadata: ElementInstanceMetadataMap
}

export interface CopyData {
  copyDataWithPropsReplaced: ElementPasteWithMetadata | null
  copyDataWithPropsPreserved: ElementPasteWithMetadata
  targetOriginalContextElementPathTrees: ElementPathTrees
  originalAllElementProps: AllElementProps
}

export interface ParsedCopyData {
  elementPaste: ElementPaste[]
  originalContextMetadata: ElementInstanceMetadataMap
  originalContextElementPathTrees: ElementPathTrees
}

async function parseClipboardData(clipboardData: DataTransfer | null): Promise<PasteResult> {
  if (clipboardData == null) {
    return {
      files: [],
      utopiaData: [],
    }
  }
  const utopiaData = extractUtopiaDataFromClipboardData(clipboardData)
  if (utopiaData.length > 0) {
    return {
      files: [],
      utopiaData: utopiaData,
    }
  } else {
    const items = clipboardData.items
    const imageArray = await extractFiles(items)
    return {
      files: imageArray,
      utopiaData: [],
    }
  }
}

export interface ClipboardDataPayload {
  html: string
  plainText: string
}

// This is required so we can mock the function in a test. Don't hate me, I already hate myself
export const Clipboard = {
  parseClipboardData,
  setClipboardData,
}

export function setClipboardData(copyData: ClipboardDataPayload): void {
  const dt = new ClipboardPolyfill.DT()
  dt.setData('text/plain', copyData.plainText)
  dt.setData('text/html', copyData.html)
  void ClipboardPolyfill.write(dt)
}

function getJSXElementPasteActions(
  editor: EditorState,
  clipboardData: Array<CopyData>,
  canvasViewportCenter: CanvasPoint,
): Array<EditorAction> {
  const clipboardFirstEntry = clipboardData.at(0)
  if (clipboardFirstEntry == null) {
    return []
  }

  const copyDataToUse =
    clipboardFirstEntry.copyDataWithPropsReplaced != null
      ? clipboardFirstEntry.copyDataWithPropsReplaced
      : clipboardFirstEntry.copyDataWithPropsPreserved

  const openFile = editor.canvas.openFile?.filename ?? null

  const selectedViews = editor.selectedViews

  const storyboardPath = getStoryboardElementPath(editor.projectContents, openFile)
  if (storyboardPath == null) {
    // if there's no storyboard, there's not much you can do
    return []
  }

  const target = getTargetParentForPaste(
    storyboardPath,
    editor.projectContents,
    selectedViews,
    editor.jsxMetadata,
    {
      elementPaste: copyDataToUse.elements,
      originalContextMetadata: copyDataToUse.targetOriginalContextMetadata,
      originalContextElementPathTrees: clipboardFirstEntry.targetOriginalContextElementPathTrees,
    },
    editor.elementPathTree,
    editor.propertyControlsInfo,
  )

  if (isLeft(target)) {
    return [
      EditorActions.addToast(
        notice(target.value, 'ERROR', false, 'get-jsx-element-paste-actions-no-parent'),
      ),
    ]
  }

  const pastePostActionData: PastePostActionMenuData = {
    type: 'PASTE',
    target: target.value,
    dataWithPropsPreserved: clipboardFirstEntry.copyDataWithPropsPreserved,
    dataWithPropsReplaced: clipboardFirstEntry.copyDataWithPropsReplaced,
    targetOriginalPathTrees: clipboardFirstEntry.targetOriginalContextElementPathTrees,
    originalAllElementProps: clipboardFirstEntry.originalAllElementProps,
    pasteTargetsToIgnore: editor.pasteTargetsToIgnore,
    canvasViewportCenter: canvasViewportCenter,
  }

  const defaultChoice =
    PropsReplacedPastePostActionChoice(pastePostActionData) ??
    PropsPreservedPastePostActionChoice(pastePostActionData)

  if (defaultChoice == null) {
    return [
      EditorActions.addToast(
        notice(
          'What you copied cannot be pasted',
          'ERROR',
          false,
          'get-jsx-element-paste-actions-no-parent',
        ),
      ),
    ]
  }

  return [
    EditorActions.startPostActionSession(pastePostActionData),
    EditorActions.executePostActionMenuChoice(defaultChoice),
  ]
}

function getFilePasteActions(
  projectContents: ProjectContentTreeRoot,
  openFile: string | null,
  canvasViewportCenter: CanvasPoint,
  pastedFiles: Array<FileResult>,
  selectedViews: Array<ElementPath>,
  componentMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
  elementPathTree: ElementPathTrees,
  propertyControlsInfo: PropertyControlsInfo,
): Array<EditorAction> {
  if (pastedFiles.length == 0) {
    return []
  }

  const storyboardPath = getStoryboardElementPath(projectContents, openFile)
  if (storyboardPath == null) {
    // if there's no storyboard, there's not much you can do
    return []
  }

  const target = getTargetParentForPaste(
    storyboardPath,
    projectContents,
    selectedViews,
    componentMetadata,
    { elementPaste: [], originalContextMetadata: {}, originalContextElementPathTrees: {} }, // TODO: get rid of this when refactoring pasting images
    elementPathTree,
    propertyControlsInfo,
  )

  if (isLeft(target)) {
    return [
      EditorActions.showToast(
        notice(target.value, 'ERROR', false, 'get-target-parent-failure-toast'),
      ),
    ]
  }

  const targetPath = MetadataUtils.resolveReparentTargetParentToPath(
    componentMetadata,
    target.value.parentPath,
  )

  const parentFrame =
    target != null ? MetadataUtils.getFrameInCanvasCoords(targetPath, componentMetadata) : null

  const parentCenter =
    parentFrame == null || isInfinityRectangle(parentFrame)
      ? canvasViewportCenter
      : Utils.getRectCenter(parentFrame)
  let pastedImages: Array<ImageResult> = []
  fastForEach(pastedFiles, (pastedFile) => {
    if (pastedFile.type === 'IMAGE_RESULT') {
      pastedImages.push({
        ...pastedFile,
        filename: urljoin('/assets/clipboard', pastedFile.filename),
      })
    }
  })

  return createDirectInsertImageActions(
    pastedImages,
    parentCenter,
    canvasScale,
    target.value.parentPath,
  )
}

export function getActionsForClipboardItems(
  editor: EditorState,
  canvasViewportCenter: CanvasPoint,
  clipboardData: Array<CopyData>,
  pastedFiles: Array<FileResult>,
  canvasScale: number,
): Array<EditorAction> {
  return [
    ...getJSXElementPasteActions(editor, clipboardData, canvasViewportCenter),
    ...getFilePasteActions(
      editor.projectContents,
      editor.canvas.openFile?.filename ?? null,
      canvasViewportCenter,
      pastedFiles,
      editor.selectedViews,
      editor.jsxMetadata,
      canvasScale,
      editor.elementPathTree,
      editor.propertyControlsInfo,
    ),
  ]
}

export function createDirectInsertImageActions(
  images: Array<ImageResult>,
  centerPoint: CanvasPoint,
  scale: number,
  parentPath: InsertionPath | null,
): Array<EditorAction> {
  if (images.length === 0) {
    return []
  } else {
    return [
      EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
      ...Utils.flatMapArray((image) => {
        const { frame, multiplier } = getFrameAndMultiplier(
          centerPoint,
          image.filename,
          image.size,
          null,
        )
        const insertWith = EditorActions.saveImageInsertWith(parentPath, frame, multiplier)
        const saveImageAction = EditorActions.saveAsset(
          image.filename,
          image.fileType,
          image.base64Bytes,
          image.hash,
          EditorActions.saveImageDetails(image.size, insertWith),
          image.gitBlobSha,
        )
        return [saveImageAction]
      }, images),
    ]
  }
}

export function createClipboardDataFromSelection(
  editor: EditorState,
  builtInDependencies: BuiltInDependencies,
): {
  data: Array<CopyData>
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
  const jsxElements: Array<ElementPaste> = mapDropNulls((target) => {
    const underlyingTarget = normalisePathToUnderlyingTarget(editor.projectContents, target)
    const targetPathSuccess = normalisePathSuccessOrThrowError(underlyingTarget)
    const projectFile = getProjectFileByFilePath(editor.projectContents, targetPathSuccess.filePath)

    if (
      projectFile == null ||
      !isTextFile(projectFile) ||
      !isParseSuccess(projectFile.fileContents.parsed)
    ) {
      return null
    }

    const elementToPaste = getElementFromProjectContents(target, editor.projectContents)
    if (elementToPaste == null || targetPathSuccess.normalisedPath == null) {
      return null
    }

    const requiredImports = getRequiredImportsForElement(
      target,
      editor.projectContents,
      editor.nodeModules.files,
      targetPathSuccess.filePath,
      builtInDependencies,
    )

    return EditorActions.elementPaste(
      elementToPaste,
      requiredImports.imports,
      target,
      requiredImports.duplicateNameMapping,
    )
  }, filteredSelectedViews)

  const copyDataWithPropsPreserved: ElementPasteWithMetadata = {
    elements: jsxElements,
    targetOriginalContextMetadata: filterMetadataForCopy(editor.selectedViews, editor.jsxMetadata),
  }

  const copyDataWithPropsReplaced =
    replaceJSXElementCopyData(copyDataWithPropsPreserved, editor.allElementProps)
      ?.copyDataReplaced ?? null

  const strippedAllElementProps: AllElementProps = Object.entries(editor.allElementProps).reduce(
    (acc: AllElementProps, [key, value]) => ({ ...acc, [key]: { style: value['style'] } }),
    {},
  )

  return {
    data: [
      {
        copyDataWithPropsPreserved: copyDataWithPropsPreserved,
        copyDataWithPropsReplaced: copyDataWithPropsReplaced,
        targetOriginalContextElementPathTrees: editor.elementPathTree,
        originalAllElementProps: strippedAllElementProps,
      },
    ],
    imageFilenames: [],
    plaintext: '',
  }
}

export function filterMetadataForCopy(
  selectedViews: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
): ElementInstanceMetadataMap {
  const allPaths = Object.keys(jsxMetadata)
  const necessaryPaths = allPaths.filter((p) => {
    const elementPath = EP.fromString(p)
    // only those element paths are relevant which are descendants or ascentors of at least one selected view
    return selectedViews.some(
      (selected) =>
        EP.isDescendantOf(selected, elementPath) ||
        EP.isDescendantOf(elementPath, selected) ||
        EP.pathsEqual(selected, elementPath),
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
