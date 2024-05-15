import * as React from 'react'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { safeIndex } from '../..//core/shared/array-utils'
import { emptyElementPath } from '../..//core/shared/element-path'
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
import { insertableComponent } from '../shared/project-components'
import type { EditorAction } from './action-types'
import {
  wrapInElement,
  insertInsertable,
  updateJSXElementName,
  applyCommandsAction,
} from './actions/action-creators'
import { useDispatch } from './store/dispatch-context'
import type { AllElementProps, FloatingInsertMenuState } from './store/editor-state'
import { useRefEditorState } from './store/store-hook'
import { getInsertionPath } from './store/insertion-path'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import type { InsertMenuItem, InsertMenuItemValue } from '../canvas/ui/floating-insert-menu'
import { wrapInDivStrategy } from './wrap-in-callbacks'
import { commandsForFirstApplicableStrategy } from '../inspector/inspector-strategies/inspector-strategy'
import type { PropertyControlsInfo } from '../custom-code/code-file'

export function changeConditionalOrFragment(
  projectContents: ProjectContentTreeRoot,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  selectedViews: Array<ElementPath>,
  floatingMenuState: FloatingInsertMenuState,
  fixedSizeForInsertion: boolean,
  element: JSXConditionalExpressionWithoutUID | JSXFragmentWithoutUID,
  propertyControlsInfo: PropertyControlsInfo,
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

  switch (floatingMenuState.insertMenuMode) {
    case 'wrap':
      actionsToDispatch = [
        wrapInElement(selectedViews, {
          element: {
            ...element,
            uid: generateUidWithExistingComponents(projectContents),
          },
          importsToAdd: importsToAdd,
        }),
      ]
      break
    case 'insert':
      const targetParent = safeIndex(selectedViews, 0) ?? emptyElementPath

      const wrapperUID = generateUidWithExistingComponents(projectContents)

      const insertionPath = getInsertionPath(
        targetParent,
        projectContents,
        jsxMetadata,
        elementPathTree,
        wrapperUID,
        selectedViews.length,
        propertyControlsInfo,
      )
      actionsToDispatch = [
        insertInsertable(
          insertionPath,
          insertableComponent(importsToAdd, () => element, '', [], null, null, null),
          fixedSizeForInsertion ? 'add-size' : 'do-not-add',
          floatingMenuState.indexPosition,
        ),
      ]
      break
    case 'swap': {
      if (element.type === 'JSX_FRAGMENT') {
        const targetsForUpdates = getElementsToTarget(selectedViews)
        actionsToDispatch = targetsForUpdates.flatMap((path) => {
          return updateJSXElementName(path, { type: 'JSX_FRAGMENT' }, importsToAdd)
        })
      }
      break
    }
    case 'closed':
      break
    default:
      assertNever(floatingMenuState)
  }
  return actionsToDispatch
}

export function changeElement(
  projectContents: ProjectContentTreeRoot,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  selectedViews: Array<ElementPath>,
  floatingMenuState: FloatingInsertMenuState,
  fixedSizeForInsertion: boolean,
  addContentForInsertion: boolean,
  pickedInsertableComponent: InsertMenuItemValue,
  source: InsertableComponentGroupType | null,
  propertyControlsInfo: PropertyControlsInfo,
): Array<EditorAction> {
  const element = pickedInsertableComponent.element()
  if (element.type !== 'JSX_ELEMENT') {
    return []
  }
  let actionsToDispatch: Array<EditorAction> = []
  switch (floatingMenuState.insertMenuMode) {
    case 'wrap':
      if (source?.type === 'HTML_DIV') {
        const commands = commandsForFirstApplicableStrategy([
          wrapInDivStrategy(
            jsxMetadata,
            selectedViews,
            elementPathTree,
            allElementProps,
            projectContents,
            propertyControlsInfo,
          ),
        ])

        if (commands != null) {
          actionsToDispatch = [applyCommandsAction(commands)]
        }
        break
      }

      const newUID = generateUidWithExistingComponents(projectContents)

      const newElement = jsxElement(
        element.name,
        newUID,
        setJSXAttributesAttribute(
          element.props,
          'data-uid',
          jsExpressionValue(newUID, emptyComments),
        ),
        element.children,
      )

      actionsToDispatch = [
        wrapInElement(selectedViews, {
          element: newElement,
          importsToAdd: pickedInsertableComponent.importsToAdd,
        }),
      ]
      break
    case 'insert':
      let elementToInsert = pickedInsertableComponent
      if (addContentForInsertion && element.children.length === 0) {
        elementToInsert = {
          ...pickedInsertableComponent,
          element: () => ({
            ...element,
            children: [jsxTextBlock('Utopia')],
          }),
        }
      }

      const targetParent: ElementPath | null =
        floatingMenuState.parentPath ?? safeIndex(selectedViews, 0) ?? null
      if (targetParent != null) {
        const wrapperUID = generateUidWithExistingComponents(projectContents)

        const insertionPath = getInsertionPath(
          targetParent,
          projectContents,
          jsxMetadata,
          elementPathTree,
          wrapperUID,
          selectedViews.length,
          propertyControlsInfo,
        )
        // TODO multiselect?
        actionsToDispatch = [
          insertInsertable(
            insertionPath,
            elementToInsert,
            fixedSizeForInsertion ? 'add-size' : 'do-not-add',
            floatingMenuState.indexPosition,
          ),
        ]
      }
      break
    case 'swap':
      // this is taken from render-as.tsx
      const targetsForUpdates = getElementsToTarget(selectedViews)
      actionsToDispatch = targetsForUpdates.flatMap((path) => {
        return updateJSXElementName(
          path,
          { type: 'JSX_ELEMENT', name: element.name },
          pickedInsertableComponent.importsToAdd,
        )
      })
      break
    case 'closed':
      break
    default:
      assertNever(floatingMenuState)
  }

  return actionsToDispatch
}

export function getActionsToApplyChange(
  projectContents: ProjectContentTreeRoot,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  selectedViews: Array<ElementPath>,
  floatingMenuState: FloatingInsertMenuState,
  fixedSizeForInsertion: boolean,
  addContentForInsertion: boolean,
  insertMenuItemValue: InsertMenuItemValue,
  propertyControlsInfo: PropertyControlsInfo,
): Array<EditorAction> {
  const element = insertMenuItemValue.element()
  switch (element.type) {
    case 'JSX_CONDITIONAL_EXPRESSION':
    case 'JSX_FRAGMENT':
      return changeConditionalOrFragment(
        projectContents,
        jsxMetadata,
        elementPathTree,
        selectedViews,
        floatingMenuState,
        fixedSizeForInsertion,
        element,
        propertyControlsInfo,
      )
    case 'JSX_ELEMENT':
      return changeElement(
        projectContents,
        jsxMetadata,
        elementPathTree,
        allElementProps,
        selectedViews,
        floatingMenuState,
        fixedSizeForInsertion,
        addContentForInsertion,
        insertMenuItemValue,
        insertMenuItemValue.source,
        propertyControlsInfo,
      )
    case 'JSX_MAP_EXPRESSION':
      return [] // we don't support converting to maps
    default:
      assertNever(element)
  }
}

export function useConvertTo(): (convertTo: InsertMenuItem | null) => void {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const allElementPropsRef = useRefEditorState((store) => store.editor.projectContents)
  const floatingMenuStateRef = useRefEditorState((store) => store.editor.floatingInsertMenu)
  const propertyControlsInfoRef = useRefEditorState((store) => store.editor.propertyControlsInfo)

  return React.useCallback(
    (convertToMenuItem: InsertMenuItem | null) => {
      if (convertToMenuItem != null) {
        const convertTo = convertToMenuItem.value
        const actions = getActionsToApplyChange(
          projectContentsRef.current,
          jsxMetadataRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          selectedViewsRef.current,
          floatingMenuStateRef.current,
          false,
          false,
          convertTo,
          propertyControlsInfoRef.current,
        )
        dispatch(actions, 'everyone')
      }
    },
    [
      allElementPropsRef,
      dispatch,
      elementPathTreeRef,
      floatingMenuStateRef,
      jsxMetadataRef,
      projectContentsRef,
      selectedViewsRef,
      propertyControlsInfoRef,
    ],
  )
}
