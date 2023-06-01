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

interface JSXElementCopyData {
  type: 'ELEMENT_COPY'
  elements: JSXElementsJson
  targetOriginalContextMetadata: ElementInstanceMetadataMap
}

type JSXElementsJson = string

export type CopyData = JSXElementCopyData

interface ParsedCopyData {
  elementPaste: ElementPaste[]
  originalContextMetadata: ElementInstanceMetadataMap
}

function parseCopyData(data: CopyData): ParsedCopyData {
  const elements = json5.parse(data.elements)
  const metadata = data.targetOriginalContextMetadata

  return { elementPaste: elements, originalContextMetadata: metadata }
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
  try {
    const parsedCopyData = clipboardData.map(parseCopyData)

    const possibleTarget = getTargetParentForPaste(
      projectContents,
      selectedViews,
      nodeModules,
      openFile,
      componentMetadata,
      pasteTargetsToIgnore,
      parsedCopyData,
    )
    const target: InsertionPath | null =
      possibleTarget == null
        ? optionalMap(childInsertionPath, getStoryboardElementPath(projectContents, openFile))
        : possibleTarget
    if (target == null) {
      console.warn(`Unable to find the storyboard path.`)
      return []
    }
    const targetPath = MetadataUtils.resolveReparentTargetParentToPath(componentMetadata, target)

    // Create the actions for inserting JSX elements into the hierarchy.
    const utopiaActions = parsedCopyData.map((data) =>
      EditorActions.pasteJSXElements(
        target,
        data.elementPaste,
        data.originalContextMetadata,
        canvasViewportCenter,
      ),
    )

    // Handle adding files into the project like pasted images.
    let insertImageActions: EditorAction[] = []
    if (pastedFiles.length > 0 && componentMetadata != null) {
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
      insertImageActions = createDirectInsertImageActions(
        pastedImages,
        parentCenter,
        canvasScale,
        target,
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
  const jsxElements: Array<ElementPaste> = mapDropNulls((target) => {
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
          openUIJSFileKey,
          targetPathSuccess.filePath,
          builtInDependencies,
        )

        return EditorActions.elementPaste(elementToPaste, requiredImports, target)
      }
    } else {
      return null
    }
  }, filteredSelectedViews)

  return {
    data: [
      {
        type: 'ELEMENT_COPY',
        elements: json5.stringify(jsxElements),
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

export function getTargetParentForPaste(
  projectContents: ProjectContentTreeRoot,
  selectedViews: Array<ElementPath>,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  metadata: ElementInstanceMetadataMap,
  pasteTargetsToIgnore: ElementPath[],
  copyData: ParsedCopyData[],
): InsertionPath | null {
  const pastedElementNames = copyData.flatMap((data) => {
    return mapDropNulls(
      (element) => MetadataUtils.getJSXElementName(element.element),
      data.elementPaste,
    )
  })

  if (selectedViews.length === 0) {
    return null
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
        return isFeatureEnabled('Paste wraps into fragment')
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
      }
    }
  }

  // if only a single item is pasted
  // if only a single item is selected
  if (
    selectedViews.length === 1 &&
    copyData.length === 1 &&
    copyData[0].elementPaste.length === 1
  ) {
    const selectedViewAABB = MetadataUtils.getFrameInCanvasCoords(selectedViews[0], metadata)
    // if the pasted item's BB is the same size as the selected item's BB
    const pastedElementAABB = MetadataUtils.getFrameInCanvasCoords(
      copyData[0].elementPaste[0].originalElementPath,
      copyData[0].originalContextMetadata,
    )
    // if the selected item's parent is autolayouted
    const parentInstance = MetadataUtils.findElementByElementPath(
      metadata,
      EP.parentPath(selectedViews[0]),
    )

    const isSelectedViewParentAutolayouted = MetadataUtils.isFlexLayoutedContainer(parentInstance)

    const isPastedElementStatic = MetadataUtils.isPositionStatic(
      MetadataUtils.findElementByElementPath(
        copyData[0].originalContextMetadata,
        copyData[0].elementPaste[0].originalElementPath,
      ),
    )

    const parentTarget = EP.parentPath(selectedViews[0])
    const pastingFlowIntoFlow =
      isPastedElementStatic &&
      parentInstance?.specialSizeMeasurements.layoutSystemForChildren === 'flow'

    const targetElementSupportsInsertedElement = MetadataUtils.canInsertElementsToTargetText(
      parentTarget,
      metadata,
      pastedElementNames,
    )

    if (
      rectangleSizesEqual(selectedViewAABB, pastedElementAABB) &&
      (isSelectedViewParentAutolayouted || pastingFlowIntoFlow) &&
      targetElementSupportsInsertedElement
    ) {
      return childInsertionPath(parentTarget)
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
    return childInsertionPath(parentTarget)
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
    return childInsertionPath(parentOfSelected)
  }

  return null
}
