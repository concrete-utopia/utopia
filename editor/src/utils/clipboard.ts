import { EditorAction, ElementPaste } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import { EditorModes } from '../components/editor/editor-modes'
import {
  EditorState,
  getOpenUIJSFileKey,
  withUnderlyingTarget,
} from '../components/editor/store/editor-state'
import { getFrameAndMultiplier } from '../components/images'
import * as EP from '../core/shared/element-path'
import { findElementAtPath, MetadataUtils } from '../core/model/element-metadata-utils'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
  isNullJSXAttributeValue,
} from '../core/shared/element-template'
import { getUtopiaJSXComponentsFromSuccess } from '../core/model/project-file-utils'
import {
  isParseSuccess,
  ElementPath,
  isTextFile,
  NodeModules,
} from '../core/shared/project-file-types'
import {
  encodeUtopiaDataToHtml,
  extractFiles,
  extractUtopiaDataFromClipboardData,
  PasteResult,
} from './clipboard-utils'
import Utils from './utils'
import { FileResult, ImageResult } from '../core/shared/file-utils'
import {
  CanvasPoint,
  MaybeInfinityCanvasRectangle,
  isInfinityRectangle,
  rectanglesEqual,
} from '../core/shared/math-utils'
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
import { mapValues, pick } from '../core/shared/object-utils'
import { getStoryboardElementPath } from '../core/model/scene-utils'
import { getRequiredImportsForElement } from '../components/editor/import-utils'
import { BuiltInDependencies } from '../core/es-modules/package-manager/built-in-dependencies-list'
import {
  childInsertionPath,
  getInsertionPathWithSlotBehavior,
  getInsertionPathWithWrapWithFragmentBehavior,
  InsertionPath,
} from '../components/editor/store/insertion-path'
import { maybeBranchConditionalCase } from '../core/model/conditionals'
import { optionalMap } from '../core/shared/optional-utils'
import { isFeatureEnabled } from './feature-switches'
import { replacePropsWithRuntimeValues } from '../components/canvas/canvas-strategies/strategies/reparent-helpers/reparent-helpers'
import {
  createInteractionViaKeyboard,
  createInteractionViaPaste,
} from '../components/canvas/canvas-strategies/interaction-state'
import CanvasActions from '../components/canvas/canvas-actions'

interface JSXElementCopyData {
  type: 'ELEMENT_COPY'
  elementsWithPropsPreserved: JSXElementsJson
  elementsWithPropsReplaced: JSXElementsJson
  targetOriginalContextMetadata: ElementInstanceMetadataMap
}

type JSXElementsJson = string

export type CopyData = JSXElementCopyData

interface ParsedCopyData {
  elementsWithPropsPreserved: ElementPaste[]
  elementsWithPropsReplaced: ElementPaste[]
  originalContextMetadata: ElementInstanceMetadataMap
}

function parseCopyData(data: CopyData): ParsedCopyData {
  const elementsWithPropsPreserved = json5.parse(data.elementsWithPropsPreserved)
  const elementsWithPropReplaced = json5.parse(data.elementsWithPropsReplaced)
  const metadata = data.targetOriginalContextMetadata

  return {
    elementsWithPropsPreserved: elementsWithPropsPreserved,
    elementsWithPropsReplaced: elementsWithPropReplaced,
    originalContextMetadata: metadata,
  }
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
  clipboardData: Array<CopyData>,
  canvasViewportCenter: CanvasPoint,
): Array<EditorAction> {
  const parsedCopyData = clipboardData.map(parseCopyData)
  if (parsedCopyData.length === 0) {
    return []
  }

  const actions = parsedCopyData.map((data) =>
    EditorActions.pasteJSXElements(
      data.elementsWithPropsPreserved,
      data.originalContextMetadata,
      canvasViewportCenter,
    ),
  )

  if (actions.length === 0) {
    return []
  }

  return [CanvasActions.createInteractionSession(createInteractionViaPaste()), ...actions]
}

function getFilePasteActions(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null,
  canvasViewportCenter: CanvasPoint,
  pastedFiles: Array<FileResult>,
  selectedViews: Array<ElementPath>,
  pasteTargetsToIgnore: ElementPath[],
  componentMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
): Array<EditorAction> {
  if (pastedFiles.length == 0) {
    return []
  }
  const target = getTargetParentForPaste(
    projectContents,
    selectedViews,
    nodeModules,
    openFile,
    componentMetadata,
    pasteTargetsToIgnore,
    { elements: [], originalContextMetadata: {} }, // TODO: get rid of this when refactoring pasting images
  )

  if (target == null) {
    console.warn(`Unable to find the storyboard path.`)
    return []
  }

  const targetPath = MetadataUtils.resolveReparentTargetParentToPath(
    componentMetadata,
    target.parentPath,
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

  return createDirectInsertImageActions(pastedImages, parentCenter, canvasScale, target.parentPath)
}

export function getActionsForClipboardItems(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null,
  canvasViewportCenter: CanvasPoint,
  clipboardData: Array<CopyData>,
  pastedFiles: Array<FileResult>,
  selectedViews: Array<ElementPath>,
  pasteTargetsToIgnore: ElementPath[],
  componentMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
): Array<EditorAction> {
  return [
    ...getJSXElementPasteActions(clipboardData, canvasViewportCenter),
    ...getFilePasteActions(
      projectContents,
      nodeModules,
      openFile,
      canvasViewportCenter,
      pastedFiles,
      selectedViews,
      pasteTargetsToIgnore,
      componentMetadata,
      canvasScale,
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
      EditorActions.switchEditorMode(EditorModes.selectMode()),
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

  const elementsWithPropsPreserved: Array<ElementPaste> = []
  const elementsWithPropsReplaced: Array<ElementPaste> = []

  filteredSelectedViews.forEach((target) => {
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

    if (
      projectFile == null ||
      !isTextFile(projectFile) ||
      !isParseSuccess(projectFile.fileContents.parsed) ||
      targetPathSuccess.normalisedPath == null
    ) {
      return
    }

    const components = getUtopiaJSXComponentsFromSuccess(projectFile.fileContents.parsed)
    const elementProps = editor.allElementProps[EP.toString(target)] ?? {}

    const elementToPaste = findElementAtPath(target, components)

    if (elementToPaste == null) {
      return
    }

    const requiredImports = getRequiredImportsForElement(
      target,
      editor.projectContents,
      editor.nodeModules.files,
      openUIJSFileKey,
      targetPathSuccess.filePath,
      builtInDependencies,
    )

    elementsWithPropsPreserved.push(
      EditorActions.elementPaste(elementToPaste, requiredImports, target),
    )
    elementsWithPropsReplaced.push(
      EditorActions.elementPaste(
        replacePropsWithRuntimeValues(elementProps, elementToPaste),
        requiredImports,
        target,
      ),
    )
  })

  return {
    data: [
      {
        type: 'ELEMENT_COPY',
        elementsWithPropsPreserved: json5.stringify(elementsWithPropsPreserved),
        elementsWithPropsReplaced: json5.stringify(elementsWithPropsReplaced),
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
  left: MaybeInfinityCanvasRectangle | null,
  right: MaybeInfinityCanvasRectangle | null,
): boolean {
  if (left == null || right == null || isInfinityRectangle(left) || isInfinityRectangle(right)) {
    return false
  }

  return left.height === right.height && left.width === right.width
}

export type ReparentTargetForPaste =
  | {
      type: 'sibling'
      siblingPath: ElementPath
      parentPath: InsertionPath
    }
  | { type: 'parent'; parentPath: InsertionPath }

export function getTargetParentForPaste(
  projectContents: ProjectContentTreeRoot,
  selectedViews: Array<ElementPath>,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  metadata: ElementInstanceMetadataMap,
  pasteTargetsToIgnore: ElementPath[],
  copyData: { elements: ElementPaste[]; originalContextMetadata: ElementInstanceMetadataMap },
): ReparentTargetForPaste | null {
  const pastedElementNames = mapDropNulls(
    (element) => MetadataUtils.getJSXElementName(element.element),
    copyData.elements,
  )

  if (selectedViews.length === 0) {
    return optionalMap(
      (s) => ({ type: 'parent', parentPath: childInsertionPath(s) }),
      getStoryboardElementPath(projectContents, openFile),
    )
  }

  // Handle "slot" like case of conditional clauses by inserting into them directly rather than their parent.
  if (selectedViews.length === 1) {
    const targetPath = selectedViews[0]
    const parentPath = EP.parentPath(targetPath)
    const parentElement = withUnderlyingTarget(
      parentPath,
      projectContents,
      nodeModules,
      openFile,
      null,
      (_, element) => {
        return element
      },
    )

    if (parentElement != null && isJSXConditionalExpression(parentElement)) {
      // Check if the target parent is an attribute,
      // if so replace the target parent instead of trying to insert into it.
      const conditionalCase = maybeBranchConditionalCase(parentPath, parentElement, targetPath)
      if (conditionalCase != null) {
        const parentInsertionPath = isFeatureEnabled('Paste wraps into fragment')
          ? getInsertionPathWithWrapWithFragmentBehavior(
              targetPath,
              projectContents,
              nodeModules,
              openFile,
              metadata,
            )
          : getInsertionPathWithSlotBehavior(
              targetPath,
              projectContents,
              nodeModules,
              openFile,
              metadata,
            )

        return optionalMap((path) => ({ type: 'parent', parentPath: path }), parentInsertionPath)
      }
    }
  }

  // if only a single item is selected
  if (selectedViews.length === 1 && copyData.elements.length === 1) {
    const selectedViewAABB = MetadataUtils.getFrameInCanvasCoords(selectedViews[0], metadata)
    // if the pasted item's BB is the same size as the selected item's BB
    const pastedElementAABB = MetadataUtils.getFrameInCanvasCoords(
      copyData.elements[0].originalElementPath,
      copyData.originalContextMetadata,
    )
    // if the selected item's parent is autolayouted
    const parentInstance = MetadataUtils.findElementByElementPath(
      metadata,
      EP.parentPath(selectedViews[0]),
    )

    const isSelectedViewParentAutolayouted = MetadataUtils.isFlexLayoutedContainer(parentInstance)

    const pastingAbsoluteToAbsolute =
      MetadataUtils.isPositionAbsolute(
        MetadataUtils.findElementByElementPath(metadata, selectedViews[0]),
      ) &&
      MetadataUtils.isPositionAbsolute(
        MetadataUtils.findElementByElementPath(
          copyData.originalContextMetadata,
          copyData.elements[0].originalElementPath,
        ),
      )

    const parentTarget = EP.parentPath(selectedViews[0])
    const targetElementSupportsInsertedElement = MetadataUtils.canInsertElementsToTargetText(
      parentTarget,
      metadata,
      pastedElementNames,
    )

    if (
      rectangleSizesEqual(selectedViewAABB, pastedElementAABB) &&
      (isSelectedViewParentAutolayouted || pastingAbsoluteToAbsolute) &&
      targetElementSupportsInsertedElement
    ) {
      return {
        type: 'sibling',
        siblingPath: selectedViews[0],
        parentPath: childInsertionPath(EP.parentPath(selectedViews[0])),
      }
    }
  }

  // paste into the target's parent

  // Regular handling which attempts to find a common parent.
  const parentTarget = EP.getCommonParent(selectedViews, true)
  if (parentTarget == null) {
    return null
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
      nodeModules,
      openFile,
      parentTarget,
    ) &&
    targetElementSupportsInsertedElement &&
    !insertingSourceIntoItself
  ) {
    return { type: 'parent', parentPath: childInsertionPath(parentTarget) }
  }

  const parentOfSelected = EP.parentPath(parentTarget)
  if (
    MetadataUtils.targetSupportsChildren(
      projectContents,
      metadata,
      nodeModules,
      openFile,
      parentOfSelected,
    )
  ) {
    return { type: 'parent', parentPath: childInsertionPath(parentOfSelected) }
  }

  return null
}
