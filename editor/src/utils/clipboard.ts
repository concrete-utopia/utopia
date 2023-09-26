import type { EditorAction, ElementPaste } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import { EditorModes } from '../components/editor/editor-modes'
import type {
  AllElementProps,
  DerivedState,
  EditorState,
  PastePostActionMenuData,
} from '../components/editor/store/editor-state'
import { getOpenUIJSFileKey, withUnderlyingTarget } from '../components/editor/store/editor-state'
import { getFrameAndMultiplier } from '../components/images'
import * as EP from '../core/shared/element-path'
import { findElementAtPath, MetadataUtils } from '../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../core/shared/element-template'
import {
  isJSXConditionalExpression,
  isNullJSXAttributeValue,
} from '../core/shared/element-template'
import { getUtopiaJSXComponentsFromSuccess } from '../core/model/project-file-utils'
import type { ElementPath, NodeModules } from '../core/shared/project-file-types'
import { isParseSuccess, isTextFile } from '../core/shared/project-file-types'
import type { PasteResult } from './clipboard-utils'
import {
  encodeUtopiaDataToHtml,
  extractFiles,
  extractUtopiaDataFromClipboardData,
} from './clipboard-utils'
import Utils from './utils'
import type { FileResult, ImageResult } from '../core/shared/file-utils'
import type { CanvasPoint, MaybeInfinityCanvasRectangle } from '../core/shared/math-utils'
import { isInfinityRectangle, rectanglesEqual } from '../core/shared/math-utils'
import * as json5 from 'json5'
import { fastForEach } from '../core/shared/utils'
import urljoin from 'url-join'
import type { ProjectContentTreeRoot } from '../components/assets'
import { getProjectFileByFilePath } from '../components/assets'
import {
  normalisePathSuccessOrThrowError,
  normalisePathToUnderlyingTarget,
} from '../components/custom-code/code-file'
import { mapDropNulls, stripNulls } from '../core/shared/array-utils'
import ClipboardPolyfill from 'clipboard-polyfill'
import { mapValues, pick } from '../core/shared/object-utils'
import { getStoryboardElementPath } from '../core/model/scene-utils'
import { getRequiredImportsForElement } from '../components/editor/import-utils'
import type { BuiltInDependencies } from '../core/es-modules/package-manager/built-in-dependencies-list'
import type { InsertionPath } from '../components/editor/store/insertion-path'
import { childInsertionPath, getInsertionPath } from '../components/editor/store/insertion-path'
import { maybeBranchConditionalCase } from '../core/model/conditionals'
import { optionalMap } from '../core/shared/optional-utils'
import { isFeatureEnabled } from './feature-switches'
import type { ElementPathTrees } from '../core/shared/element-path-tree'
import {
  isElementRenderedBySameComponent,
  replaceJSXElementCopyData,
} from '../components/canvas/canvas-strategies/strategies/reparent-helpers/reparent-helpers'
import CanvasActions from '../components/canvas/canvas-actions'
import {
  PropsPreservedPastePostActionChoice,
  PropsReplacedPastePostActionChoice,
} from '../components/canvas/canvas-strategies/post-action-options/post-action-paste'
import type { Either } from '../core/shared/either'
import { isLeft, left, right } from '../core/shared/either'
import { notice } from '../components/common/notice'
import { generateUidWithExistingComponents } from '../core/model/element-template-utils'
import type { RemixRoutingTable } from '../components/editor/store/remix-derived-data'

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

interface ParsedCopyData {
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

  const target = getTargetParentForPaste(
    editor.projectContents,
    editor.selectedViews,
    editor.canvas.openFile?.filename ?? null,
    editor.jsxMetadata,
    editor.pasteTargetsToIgnore,
    {
      elementPaste: copyDataToUse.elements,
      originalContextMetadata: copyDataToUse.targetOriginalContextMetadata,
      originalContextElementPathTrees: clipboardFirstEntry.targetOriginalContextElementPathTrees,
    },
    editor.elementPathTree,
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
  pasteTargetsToIgnore: ElementPath[],
  componentMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
  elementPathTree: ElementPathTrees,
): Array<EditorAction> {
  if (pastedFiles.length == 0) {
    return []
  }
  const target = getTargetParentForPaste(
    projectContents,
    selectedViews,
    openFile,
    componentMetadata,
    pasteTargetsToIgnore,
    { elementPaste: [], originalContextMetadata: {}, originalContextElementPathTrees: {} }, // TODO: get rid of this when refactoring pasting images
    elementPathTree,
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
      editor.pasteTargetsToIgnore,
      editor.jsxMetadata,
      canvasScale,
      editor.elementPathTree,
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
      projectFile != null &&
      isTextFile(projectFile) &&
      isParseSuccess(projectFile.fileContents.parsed)
    ) {
      const components = getUtopiaJSXComponentsFromSuccess(projectFile.fileContents.parsed)
      const elementToPaste = findElementAtPath(target, components)
      if (elementToPaste == null || targetPathSuccess.normalisedPath == null) {
        return null
      } else {
        const requiredImports = getRequiredImportsForElement(
          target,
          editor.projectContents,
          editor.nodeModules.files,
          targetPathSuccess.filePath,
          builtInDependencies,
        )

        return EditorActions.elementPaste(elementToPaste, requiredImports, target)
      }
    } else {
      return null
    }
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

function rectangleSizesEqual(
  a: MaybeInfinityCanvasRectangle | null,
  b: MaybeInfinityCanvasRectangle | null,
): boolean {
  if (a == null || b == null || isInfinityRectangle(a) || isInfinityRectangle(b)) {
    return false
  }

  return a.height === b.height && a.width === b.width
}

export type ReparentTargetForPaste =
  | {
      type: 'sibling'
      siblingPath: ElementPath
      parentPath: InsertionPath
    }
  | { type: 'parent'; parentPath: InsertionPath }

type PasteParentNotFoundError =
  | 'Cannot find a suitable parent'
  | 'Cannot find storyboard path'
  | 'Cannot insert component instance into component definition'

export function getTargetParentForPaste(
  projectContents: ProjectContentTreeRoot,
  selectedViews: Array<ElementPath>,
  openFile: string | null | undefined,
  metadata: ElementInstanceMetadataMap,
  pasteTargetsToIgnore: ElementPath[],
  copyData: ParsedCopyData,
  elementPathTree: ElementPathTrees,
): Either<PasteParentNotFoundError, ReparentTargetForPaste> {
  const pastedElementNames = mapDropNulls(
    (element) => MetadataUtils.getJSXElementName(element.element),
    copyData.elementPaste,
  )

  if (selectedViews.length === 0) {
    const storyboardPath = getStoryboardElementPath(projectContents, openFile)
    if (storyboardPath == null) {
      return left('Cannot find storyboard path')
    }
    return right({ type: 'parent', parentPath: childInsertionPath(storyboardPath) })
  }

  // Regular handling which attempts to find a common parent.
  const parentTarget = EP.getCommonParent(selectedViews, true)
  if (parentTarget == null) {
    return left('Cannot find a suitable parent')
  }

  if (
    copyData.elementPaste.some((pastedElement) =>
      isElementRenderedBySameComponent(
        metadata,
        parentTarget,
        MetadataUtils.findElementByElementPath(
          copyData.originalContextMetadata,
          pastedElement.originalElementPath,
        ),
      ),
    )
  ) {
    return left('Cannot insert component instance into component definition')
  }

  // Handle "slot" like case of conditional clauses by inserting into them directly rather than their parent.
  if (selectedViews.length === 1) {
    // This should exist because the check above proves there should be a value.
    const targetPath = selectedViews[0]!
    const parentPath = EP.parentPath(targetPath)
    const parentElement = withUnderlyingTarget(parentPath, projectContents, null, (_, element) => {
      return element
    })

    if (parentElement != null && isJSXConditionalExpression(parentElement)) {
      // Check if the target parent is an attribute,
      // if so replace the target parent instead of trying to insert into it.
      const wrapperFragmentUID = generateUidWithExistingComponents(projectContents)
      const conditionalCase = maybeBranchConditionalCase(parentPath, parentElement, targetPath)
      if (conditionalCase != null) {
        const parentInsertionPath = getInsertionPath(
          targetPath,
          projectContents,
          metadata,
          elementPathTree,
          wrapperFragmentUID,
          copyData.elementPaste.length,
        )

        if (parentInsertionPath == null) {
          return left('Cannot find a suitable parent')
        }
        return right({ type: 'parent', parentPath: parentInsertionPath })
      }
    }
  }

  // if only a single item is selected
  if (selectedViews.length === 1 && copyData.elementPaste.length === 1) {
    // These should exist because the check above proves there should be a values there.
    const targetPath = selectedViews[0]!
    const elementPasteEntry = copyData.elementPaste[0]!
    const selectedViewAABB = MetadataUtils.getFrameInCanvasCoords(targetPath, metadata)
    // if the pasted item's BB is the same size as the selected item's BB
    const pastedElementAABB = MetadataUtils.getFrameInCanvasCoords(
      elementPasteEntry.originalElementPath,
      copyData.originalContextMetadata,
    )
    // if the selected item's parent is autolayouted
    const parentInstance = MetadataUtils.findElementByElementPath(
      metadata,
      EP.parentPath(targetPath),
    )

    const isSelectedViewParentAutolayouted = MetadataUtils.isFlexLayoutedContainer(parentInstance)

    const pastingAbsoluteToAbsolute =
      MetadataUtils.isPositionAbsolute(
        MetadataUtils.findElementByElementPath(metadata, targetPath),
      ) &&
      MetadataUtils.isPositionAbsolute(
        MetadataUtils.findElementByElementPath(
          copyData.originalContextMetadata,
          elementPasteEntry.originalElementPath,
        ),
      )

    const parentPath = EP.parentPath(targetPath)
    const targetElementSupportsInsertedElement = MetadataUtils.canInsertElementsToTargetText(
      parentPath,
      metadata,
      pastedElementNames,
    )

    if (
      rectangleSizesEqual(selectedViewAABB, pastedElementAABB) &&
      (isSelectedViewParentAutolayouted || pastingAbsoluteToAbsolute) &&
      targetElementSupportsInsertedElement
    ) {
      return right({
        type: 'sibling',
        siblingPath: targetPath,
        parentPath: childInsertionPath(EP.parentPath(targetPath)),
      })
    }
  }

  // we should not paste the source into itself
  const insertingSourceIntoItself = EP.containsPath(parentTarget, pasteTargetsToIgnore)
  const targetElementSupportsInsertedElement = MetadataUtils.canInsertElementsToTargetText(
    parentTarget,
    metadata,
    pastedElementNames,
  )
  if (
    MetadataUtils.targetSupportsChildren(
      projectContents,
      metadata,
      parentTarget,
      elementPathTree,
    ) &&
    targetElementSupportsInsertedElement &&
    !insertingSourceIntoItself
  ) {
    return right({ type: 'parent', parentPath: childInsertionPath(parentTarget) })
  }

  const parentOfSelected = EP.parentPath(parentTarget)
  if (
    MetadataUtils.targetSupportsChildren(
      projectContents,
      metadata,
      parentOfSelected,
      elementPathTree,
    )
  ) {
    return right({ type: 'parent', parentPath: childInsertionPath(parentOfSelected) })
  }

  return left('Cannot find a suitable parent')
}
