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
import { encodeUtopiaDataToHtml, parsePasteEvent, PasteResult } from './clipboard-utils'
import { setLocalClipboardData } from './local-clipboard'
import Utils from './utils'
import { FileResult, ImageResult } from '../core/shared/file-utils'
import { CanvasPoint, isInfinityRectangle } from '../core/shared/math-utils'
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
  arrayInsertionPath,
  conditionalClauseInsertionPath,
  InsertionPath,
} from '../components/editor/store/reparent-target'
import { maybeBranchConditionalCase } from '../core/model/conditionals'

interface JSXElementCopyData {
  type: 'ELEMENT_COPY'
  elements: JSXElementsJson
  targetOriginalContextMetadata: ElementInstanceMetadataMap
}

type JSXElementsJson = string

export type CopyData = JSXElementCopyData

function parseClipboardData(clipboardData: DataTransfer | null): Promise<PasteResult> {
  return parsePasteEvent(clipboardData)
}

// This is required so we can mock the function in a test. Don't hate me, I already hate myself
export const Clipboard = {
  parseClipboardData,
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
    void ClipboardPolyfill.write(dt)
  }
}

export function getActionsForClipboardItems(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null,
  clipboardData: Array<CopyData>,
  pastedFiles: Array<FileResult>,
  selectedViews: Array<ElementPath>,
  pasteTargetsToIgnore: ElementPath[],
  componentMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
): Array<EditorAction> {
  try {
    const possibleTarget = getTargetParentForPaste(
      projectContents,
      selectedViews,
      nodeModules,
      openFile,
      componentMetadata,
      pasteTargetsToIgnore,
    )
    const target =
      possibleTarget == null
        ? getStoryboardElementPath(projectContents, openFile)
        : MetadataUtils.resolveReparentTargetParentToPath(componentMetadata, possibleTarget)
    if (target == null) {
      console.warn(`Unable to find the storyboard path.`)
      return []
    }

    // Create the actions for inserting JSX elements into the hierarchy.
    const utopiaActions = Utils.flatMapArray((data: CopyData) => {
      const elements = json5.parse(data.elements)
      const metadata = data.targetOriginalContextMetadata
      return [EditorActions.pasteJSXElements(target, elements, metadata)]
    }, clipboardData)

    // Handle adding files into the project like pasted images.
    let insertImageActions: EditorAction[] = []
    if (pastedFiles.length > 0 && componentMetadata != null) {
      const parentFrame =
        target != null ? MetadataUtils.getFrameInCanvasCoords(target, componentMetadata) : null
      const parentCenter =
        parentFrame == null || isInfinityRectangle(parentFrame)
          ? (Utils.point(100, 100) as CanvasPoint) // We should instead paste the top left at 0,0
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
        arrayInsertionPath(target, 'children', null),
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

export function getTargetParentForPaste(
  projectContents: ProjectContentTreeRoot,
  selectedViews: Array<ElementPath>,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  metadata: ElementInstanceMetadataMap,
  pasteTargetsToIgnore: ElementPath[],
): InsertionPath | null {
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
        const clause =
          conditionalCase === 'true-case' ? parentElement.whenTrue : parentElement.whenFalse
        if (!isNullJSXAttributeValue(clause)) {
          return null
        }
        return conditionalClauseInsertionPath(
          EP.dynamicPathToStaticPath(parentPath),
          conditionalCase,
        )
      }
    }
  }

  // Regular handling which attempts to find a common parent.
  if (selectedViews.length > 0) {
    const parentTarget = EP.getCommonParent(selectedViews, true)
    if (parentTarget == null) {
      return null
    } else {
      // we should not paste the source into itself
      const insertingSourceIntoItself = EP.containsPath(parentTarget, pasteTargetsToIgnore)
      if (
        MetadataUtils.targetSupportsChildren(
          projectContents,
          metadata,
          nodeModules,
          openFile,
          parentTarget,
        ) &&
        !insertingSourceIntoItself
      ) {
        return arrayInsertionPath(parentTarget, 'children', null)
      } else {
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
          return arrayInsertionPath(parentOfSelected, 'children', null)
        } else {
          return null
        }
      }
    }
  } else {
    return null
  }
}
