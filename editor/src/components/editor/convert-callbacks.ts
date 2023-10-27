import * as React from 'react'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { isNonEmptyArray, mapDropNulls, safeIndex } from '../..//core/shared/array-utils'
import { emptyElementPath, getCommonParent } from '../..//core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  JSXConditionalExpressionWithoutUID,
  JSXFragmentWithoutUID,
} from '../..//core/shared/element-template'
import {
  emptyComments,
  jsExpressionValue,
  jsxElement,
  jsxTextBlock,
  setJSXAttributesAttribute,
} from '../..//core/shared/element-template'
import type { ElementPath, Imports } from '../..//core/shared/project-file-types'
import { assertNever } from '../..//core/shared/utils'
import { emptyImports } from '../..//core/workers/common/project-file-utils'
import type { ProjectContentTreeRoot } from '../assets'
import { getElementsToTarget } from '../inspector/common/inspector-utils'
import type { InsertableComponentGroupType } from '../shared/project-components'
import { insertableComponent, insertableComponentGroupDiv } from '../shared/project-components'
import type { EditorAction } from './action-types'
import {
  wrapInElement,
  insertInsertable,
  updateJSXElementName,
  applyCommandsAction,
  selectComponents,
  mergeWithPrevUndo,
} from './actions/action-creators'
import { useDispatch } from './store/dispatch-context'
import type { AllElementProps, FloatingInsertMenuState } from './store/editor-state'
import { useRefEditorState } from './store/store-hook'
import { childInsertionPath, getInsertionPath } from './store/insertion-path'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import type { InsertMenuItem, InsertMenuItemValue } from '../canvas/ui/floating-insert-menu'
import { wrapInDivCommands, wrapInDivStrategy } from './wrap-in-callbacks'
import { commandsForFirstApplicableStrategy } from '../inspector/inspector-strategies/inspector-strategy'
import { elementFromInsertMenuItem, insertWithStrategies } from './insert-callbacks'
import {
  elementToReparent,
  getTargetParentForOneShotInsertion,
  pathToReparent,
} from '../canvas/canvas-strategies/strategies/reparent-utils'
import {
  canvasPoint,
  nonEmptyboundingRectangleArray,
  nullIfInfinity,
} from '../../core/shared/math-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import * as PP from '../../core/shared/property-path'
import { sizeToDimensionsFromFrame } from '../inspector/inspector-common'
import { deleteProperties } from '../canvas/commands/delete-properties-command'
import { getStoryboardElementPath } from '../../core/model/scene-utils'
import { getAllUniqueUids } from '../../core/model/get-unique-ids'
import { generateConsistentUID, fixUtopiaElement } from '../../core/shared/uid-utils'
import { isLeft } from '../../core/shared/either'
import { absolute } from '../../utils/utils'

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

/**
 * Wrap with Strategies
 * - insert element into the parent of the selected elements, at the right index position
 * - if necessary, move the inserted element to where the BB of the selected elements was
 * - reparent the original selected elements into the new inserted element
 * - if absolute, size the element to the AABB of the original elements
 *
 * - do this all with a single undo step
 */

/**
 * TODO
 * - separate the convert and wrap code paths
 * - factor out element creation from `useWrapInto` and `useToInsert`
 * - insert all selected elements with `insertWithStrategies`
 * - tests
 */

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

      // TODO: not great
      if (wrapIntoElement.source === 'Div') {
        // TODO: could be a strategy
        const result = wrapInDivCommands(
          jsxMetadataRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          projectContentsRef.current,
          selectedViewsRef.current,
        )

        if (isLeft(result)) {
          return
        }

        dispatch([applyCommandsAction(result.value)])
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

      const allElementUids = new Set(getAllUniqueUids(projectContentsRef.current).uniqueIDs)

      const wrappedUid = generateConsistentUID('wrapper', allElementUids)

      allElementUids.add(wrappedUid)

      const elementUid = generateConsistentUID('element', allElementUids)

      const element = elementToReparent(
        fixUtopiaElement(
          elementFromInsertMenuItem(wrapIntoElement.value.element(), elementUid),
          allElementUids,
        ).value,
        wrapIntoElement.value.importsToAdd,
      )

      const originalSelectedElements = [...selectedViewsRef.current]
      if (!isNonEmptyArray(originalSelectedElements)) {
        return
      }

      const commonParent = getCommonParent(originalSelectedElements)
      if (commonParent == null) {
        return
      }

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

      const result = insertWithStrategies(
        pathToReparent(originalSelectedElements[0]), // TODO: all children
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
        {},
      )

      if (result == null) {
        return
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
          applyCommandsAction([...result.commands, ...widthHeightCommands]),
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
