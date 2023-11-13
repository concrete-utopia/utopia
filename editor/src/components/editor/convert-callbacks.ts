import * as React from 'react'
import type { NonEmptyArray } from '../..//core/shared/array-utils'
import { isNonEmptyArray, mapDropNulls } from '../..//core/shared/array-utils'
import * as EP from '../..//core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  JSXConditionalExpressionWithoutUID,
  JSXElementChild,
  JSXFragmentWithoutUID,
} from '../..//core/shared/element-template'
import type { ElementPath, Imports } from '../..//core/shared/project-file-types'
import { assertNever } from '../..//core/shared/utils'
import { emptyImports } from '../..//core/workers/common/project-file-utils'
import { getElementsToTarget } from '../inspector/common/inspector-utils'
import type { EditorAction } from './action-types'
import {
  updateJSXElementName,
  applyCommandsAction,
  selectComponents,
  mergeWithPrevUndo,
} from './actions/action-creators'
import { useDispatch } from './store/dispatch-context'
import { useRefEditorState } from './store/store-hook'
import type { ConditionalClauseInsertBehavior } from './store/insertion-path'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  getElementPathFromInsertionPath,
  replaceWithElementsWrappedInFragmentBehaviour,
  replaceWithSingleElement,
} from './store/insertion-path'
import type { InsertMenuItem, InsertMenuItemValue } from '../canvas/ui/floating-insert-menu'
import { elementFromInsertItem, insertWithStrategies } from './insert-callbacks'
import type { ElementToReparent } from '../canvas/canvas-strategies/strategies/reparent-utils'
import {
  elementToReparent,
  getTargetParentForOneShotInsertion,
  pathToReparent,
} from '../canvas/canvas-strategies/strategies/reparent-utils'
import type { CanvasPoint, LocalRectangle } from '../../core/shared/math-utils'
import {
  canvasPoint,
  nonEmptyboundingRectangleArray,
  nullIfInfinity,
  zeroRectangle,
} from '../../core/shared/math-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import * as PP from '../../core/shared/property-path'
import { sizeToDimensionsFromFrame } from '../inspector/inspector-common'
import { deleteProperties } from '../canvas/commands/delete-properties-command'
import { getStoryboardElementPath } from '../../core/model/scene-utils'
import { isLeft } from '../../core/shared/either'
import { absolute } from '../../utils/utils'
import type { CanvasCommand } from '../canvas/commands/commands'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import type { ProjectContentTreeRoot } from '../assets'
import { addElement } from '../canvas/commands/add-element-command'
import { reparentElement } from '../canvas/commands/reparent-element-command'
import { getAllUniqueUids } from '../../core/model/get-unique-ids'
import { generateConsistentUID } from '../../core/shared/uid-utils'
import { deleteElement } from '../canvas/commands/delete-element-command'
import { addElements } from '../canvas/commands/add-elements-command'
import { updateSelectedViews } from '../canvas/commands/update-selected-views-command'
import { addImportsToFile } from '../canvas/commands/add-imports-to-file-command'
import { withUnderlyingTarget } from './store/editor-state'
import { showToastCommand } from '../canvas/commands/show-toast-command'
import {
  isEmptyGroup,
  isMaybeGroupForWrapping,
} from '../canvas/canvas-strategies/strategies/group-helpers'
import { elementCanBeAGroupChild } from '../canvas/canvas-strategies/strategies/group-conversion-helpers'

export function convertToConditionalOrFragment(
  selectedViews: Array<ElementPath>,
  element: JSXConditionalExpressionWithoutUID | JSXFragmentWithoutUID,
): Array<EditorAction> {
  let actionsToDispatch: Array<EditorAction> = []
  const importsToAdd: Imports =
    element.type === 'JSX_FRAGMENT' && element.longForm
      ? {
          react: {
            importedAs: 'React',
            importedFromWithin: [],
            importedWithName: null,
          },
        }
      : emptyImports()

  if (element.type === 'JSX_FRAGMENT') {
    const targetsForUpdates = getElementsToTarget(selectedViews)
    actionsToDispatch = targetsForUpdates.flatMap((path) => {
      return updateJSXElementName(path, { type: 'JSX_FRAGMENT' }, importsToAdd)
    })
  }
  return actionsToDispatch
}

export function convertToElement(
  selectedViews: Array<ElementPath>,
  pickedInsertableComponent: InsertMenuItemValue,
): Array<EditorAction> {
  const element = pickedInsertableComponent.element()
  if (element.type !== 'JSX_ELEMENT') {
    return []
  }
  let actionsToDispatch: Array<EditorAction> = []
  const targetsForUpdates = getElementsToTarget(selectedViews)
  actionsToDispatch = targetsForUpdates.flatMap((path) => {
    return updateJSXElementName(
      path,
      { type: 'JSX_ELEMENT', name: element.name },
      pickedInsertableComponent.importsToAdd,
    )
  })

  return actionsToDispatch
}

export function getActionsToApplyConversion(
  selectedViews: Array<ElementPath>,
  insertMenuItemValue: InsertMenuItemValue,
): Array<EditorAction> {
  const element = insertMenuItemValue.element()
  switch (element.type) {
    case 'JSX_CONDITIONAL_EXPRESSION':
    case 'JSX_FRAGMENT':
      return convertToConditionalOrFragment(selectedViews, element)
    case 'JSX_ELEMENT':
      return convertToElement(selectedViews, insertMenuItemValue)
    default:
      assertNever(element)
  }
}

export function useConvertTo(): (convertTo: InsertMenuItem | null) => void {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

  return React.useCallback(
    (convertToMenuItem: InsertMenuItem | null) => {
      if (convertToMenuItem != null) {
        const convertTo = convertToMenuItem.value
        const actions = getActionsToApplyConversion(selectedViewsRef.current, convertTo)
        dispatch(actions, 'everyone')
      }
    },
    [dispatch, selectedViewsRef],
  )
}

interface ElementWrappingInfo {
  elementPath: ElementPath
  positionInBoundingBox: CanvasPoint
  indexInParent: number
}

function getElementWrappingInfo(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  selectedElementsAABB: LocalRectangle,
  selectedElements: ElementPath[],
): ElementWrappingInfo[] {
  return selectedElements.map((path) => {
    const frame =
      nullIfInfinity(MetadataUtils.findElementByElementPath(metadata, path)?.localFrame) ??
      zeroRectangle
    return {
      elementPath: path,
      indexInParent: MetadataUtils.getIndexInParent(metadata, elementPathTrees, path) ?? 0,
      positionInBoundingBox: canvasPoint({
        x: frame.x - selectedElementsAABB.x,
        y: frame.y - selectedElementsAABB.y,
      }),
    }
  })
}

export function wrapWithFragmentOrConditional(
  metadata: ElementInstanceMetadataMap,
  elementPathTrees: ElementPathTrees,
  projectContents: ProjectContentTreeRoot,
  storyboardPath: ElementPath,
  selectedViews: NonEmptyArray<ElementPath>,
  wrapperUid: string,
  element: JSXElementChild,
): CanvasCommand[] | null {
  if (element.type !== 'JSX_FRAGMENT' && element.type !== 'JSX_CONDITIONAL_EXPRESSION') {
    return null
  }

  const commonParent = EP.getCommonParentOfNonemptyPathArray(selectedViews)

  const targetParent = getTargetParentForOneShotInsertion(
    storyboardPath,
    projectContents,
    [commonParent],
    metadata,
    [element],
    elementPathTrees,
  )

  if (isLeft(targetParent)) {
    return null
  }

  const targetFilePath = withUnderlyingTarget(
    targetParent.value.parentPath.intendedParentPath,
    projectContents,
    null,
    (_, __, ___, underlyingFilePath) => {
      return underlyingFilePath
    },
  )
  if (targetFilePath == null) {
    return null
  }

  const indexInParent =
    MetadataUtils.getIndexInParent(metadata, elementPathTrees, selectedViews[0]) ?? 0

  const newParentPath = EP.appendToPath(
    getElementPathFromInsertionPath(targetParent.value.parentPath),
    element.uid,
  )

  if (element.type === 'JSX_FRAGMENT') {
    return [
      addElement('always', targetParent.value.parentPath, element, {
        indexPosition: absolute(indexInParent),
      }),
      ...selectedViews.map((path) =>
        reparentElement('always', path, childInsertionPath(newParentPath)),
      ),
      addImportsToFile('always', targetFilePath, {
        react: {
          importedAs: 'React',
          importedFromWithin: [],
          importedWithName: null,
        },
      }),
      updateSelectedViews('always', [newParentPath]),
    ]
  }

  const jsxElements = mapDropNulls(
    (path) => MetadataUtils.getJSXElementFromMetadata(metadata, path),
    selectedViews,
  )

  if (element.type === 'JSX_CONDITIONAL_EXPRESSION') {
    const insertBehaviour: ConditionalClauseInsertBehavior =
      selectedViews.length > 1
        ? replaceWithElementsWrappedInFragmentBehaviour(wrapperUid)
        : replaceWithSingleElement()

    return [
      addElement('always', targetParent.value.parentPath, element, {
        indexPosition: absolute(indexInParent),
      }),
      ...selectedViews.map((path) => deleteElement('always', path)),
      addElements(
        'always',
        conditionalClauseInsertionPath(newParentPath, 'true-case', insertBehaviour),
        jsxElements,
      ),
      updateSelectedViews('always', [newParentPath]),
    ]
  }

  assertNever(element)
}

function wrapRootElementOfInstance(targets: ElementPath[]): CanvasCommand[] | null {
  const anyTargetIsARootElement = targets.some(EP.isRootElementOfInstance)
  if (anyTargetIsARootElement) {
    return [
      showToastCommand(
        `Root elements can't be wrapped into other elements.`,
        'INFO',
        'wrap-root-element-toast',
      ),
    ]
  }

  return null
}

function wrapEmptyGroup(
  metadata: ElementInstanceMetadataMap,
  targets: ElementPath[],
): CanvasCommand[] | null {
  const anyTargetIsAnEmptyGroup = targets.some((path) => isEmptyGroup(metadata, path))
  if (anyTargetIsAnEmptyGroup) {
    return [showToastCommand(`Empty Groups cannot be wrapped`, 'ERROR', 'wrap-empty-group-toast')]
  }

  return null
}

function wrapInvalidGroupChildrenIntoGroup(
  metadata: ElementInstanceMetadataMap,
  targets: ElementPath[],
  wrapper: { element: JSXElementChild; imports: Imports },
): CanvasCommand[] | null {
  const wrapperIsGroup = isMaybeGroupForWrapping(wrapper.element, wrapper.imports)
  const anyElementInvalidGroupChild = targets.some((path) => {
    return !elementCanBeAGroupChild(
      MetadataUtils.getJsxElementChildFromMetadata(metadata, path),
      path,
      metadata,
    )
  })

  const elementsCannotBeWrappedInGroup = wrapperIsGroup && anyElementInvalidGroupChild
  if (elementsCannotBeWrappedInGroup) {
    return [
      showToastCommand(
        `Not all targets can be wrapped into a Group`,
        'ERROR',
        'wrap-invalid-element-into-group-toast',
      ),
    ]
  }
  return null
}

export function useWrapInto(): (wrapInto: InsertMenuItem | null) => void {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const allElementPropsRef = useRefEditorState((store) => store.editor.projectContents)
  const builtInDependenciesRef = useRefEditorState((store) => store.builtInDependencies)
  const nodeModulesRef = useRefEditorState((store) => store.editor.nodeModules)
  const openFileRef = useRefEditorState((store) => store.editor.canvas.openFile?.filename ?? null)

  return React.useCallback(
    (wrapIntoElement: InsertMenuItem | null) => {
      if (wrapIntoElement == null) {
        return
      }

      const storyboardPath = getStoryboardElementPath(
        projectContentsRef.current,
        openFileRef.current,
      )

      if (storyboardPath == null) {
        // if there's no storyboard, there's not much you can do
        return
      }

      const originalSelectedElements = [...selectedViewsRef.current]
      if (!isNonEmptyArray(originalSelectedElements)) {
        return
      }

      let element: JSXElementChild | ElementToReparent = elementFromInsertItem(
        projectContentsRef.current,
        wrapIntoElement,
      )

      const edgeCaseCommands =
        wrapRootElementOfInstance(originalSelectedElements) ??
        wrapEmptyGroup(jsxMetadataRef.current, originalSelectedElements) ??
        wrapInvalidGroupChildrenIntoGroup(jsxMetadataRef.current, originalSelectedElements, {
          element: element,
          imports: wrapIntoElement.value.importsToAdd,
        })

      if (edgeCaseCommands != null) {
        return dispatch([applyCommandsAction(edgeCaseCommands)])
      }

      const allElementUids = new Set(getAllUniqueUids(projectContentsRef.current).allIDs)
      allElementUids.add(element.uid)
      const wrapperUid = generateConsistentUID('fragment-wrapper', allElementUids)

      const fragmentOrConditionalCommands = wrapWithFragmentOrConditional(
        jsxMetadataRef.current,
        elementPathTreeRef.current,
        projectContentsRef.current,
        storyboardPath,
        originalSelectedElements,
        wrapperUid,
        element,
      )

      if (fragmentOrConditionalCommands != null) {
        dispatch([applyCommandsAction(fragmentOrConditionalCommands)])
        return
      }

      element = elementToReparent(element, wrapIntoElement.value.importsToAdd)

      const commonParent = EP.getCommonParentOfNonemptyPathArray(originalSelectedElements)

      const targetParent = getTargetParentForOneShotInsertion(
        storyboardPath,
        projectContentsRef.current,
        [commonParent],
        jsxMetadataRef.current,
        [element.element],
        elementPathTreeRef.current,
      )

      if (isLeft(targetParent)) {
        return
      }

      const indexInParent =
        MetadataUtils.getIndexInParent(
          jsxMetadataRef.current,
          elementPathTreeRef.current,
          originalSelectedElements[0],
        ) ?? 0

      const existingSelectedAABBs = mapDropNulls(
        (path) =>
          nullIfInfinity(
            MetadataUtils.findElementByElementPath(jsxMetadataRef.current, path)?.localFrame,
          ),
        originalSelectedElements,
      )

      if (!isNonEmptyArray(existingSelectedAABBs)) {
        return
      }

      const selectedViewsAABB = nonEmptyboundingRectangleArray(existingSelectedAABBs)

      const wrappingInfo = getElementWrappingInfo(
        jsxMetadataRef.current,
        elementPathTreeRef.current,
        selectedViewsAABB,
        originalSelectedElements,
      )

      const selectedViewsTopLeft = canvasPoint({ x: selectedViewsAABB.x, y: selectedViewsAABB.y })

      const wrapperInsertResult = insertWithStrategies(
        element,
        targetParent.value.parentPath,
        {
          metadata: jsxMetadataRef.current,
          elementPathTree: elementPathTreeRef.current,
          allElementProps: allElementPropsRef.current,
          selectedViews: selectedViewsRef.current,
          projectContents: projectContentsRef.current,
          builtInDependencies: builtInDependenciesRef.current,
          nodeModules: nodeModulesRef.current,
        },
        { position: selectedViewsTopLeft, indexPosition: absolute(indexInParent) },
      )

      if (wrapperInsertResult == null) {
        return
      }

      dispatch([applyCommandsAction(wrapperInsertResult.commands)])

      const newParentPath = wrapperInsertResult.newPath

      let moveElementsIntoWrapperCommands: CanvasCommand[] = []

      for (const elementInfo of wrappingInfo) {
        const result = insertWithStrategies(
          pathToReparent(elementInfo.elementPath),
          childInsertionPath(newParentPath),
          {
            metadata: jsxMetadataRef.current,
            elementPathTree: elementPathTreeRef.current,
            allElementProps: allElementPropsRef.current,
            selectedViews: selectedViewsRef.current,
            projectContents: projectContentsRef.current,
            builtInDependencies: builtInDependenciesRef.current,
            nodeModules: nodeModulesRef.current,
          },
          {
            indexPosition: absolute(elementInfo.indexInParent),
            position: elementInfo.positionInBoundingBox,
          },
        )
        if (result != null) {
          moveElementsIntoWrapperCommands = moveElementsIntoWrapperCommands.concat(result.commands)
        }
      }

      const isWrappingIntoFlex = MetadataUtils.isFlexLayoutedContainer(
        MetadataUtils.findElementByElementPath(jsxMetadataRef.current, newParentPath),
      )

      const widthHeightCommands = isWrappingIntoFlex
        ? [
            deleteProperties('always', newParentPath, [
              PP.create('style', 'width'),
              PP.create('style', 'height'),
            ]),
          ]
        : sizeToDimensionsFromFrame(
            jsxMetadataRef.current,
            elementPathTreeRef.current,
            newParentPath,
            { width: selectedViewsAABB.width, height: selectedViewsAABB.height },
          )

      dispatch([
        mergeWithPrevUndo([
          applyCommandsAction([...moveElementsIntoWrapperCommands, ...widthHeightCommands]),
          selectComponents([newParentPath], false),
        ]),
      ])
    },
    [
      allElementPropsRef,
      builtInDependenciesRef,
      dispatch,
      elementPathTreeRef,
      jsxMetadataRef,
      nodeModulesRef,
      openFileRef,
      projectContentsRef,
      selectedViewsRef,
    ],
  )
}
