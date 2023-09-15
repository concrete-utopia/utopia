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
import type { ElementPath, Imports, NodeModules } from '../..//core/shared/project-file-types'
import { assertNever } from '../..//core/shared/utils'
import { emptyImports } from '../..//core/workers/common/project-file-utils'
import type { ProjectContentTreeRoot } from '../assets'
import { getElementsToTarget } from '../inspector/common/inspector-utils'
import { insertableComponent } from '../shared/project-components'
import type { EditorAction } from './action-types'
import { wrapInElement, insertInsertable, updateJSXElementName } from './actions/action-creators'
import { useDispatch } from './store/dispatch-context'
import type { FloatingInsertMenuState } from './store/editor-state'
import { useRefEditorState } from './store/store-hook'
import { getInsertionPath } from './store/insertion-path'
import type { RemixRoutingTable } from './store/remix-derived-data'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import type { InsertMenuItem, InsertMenuItemValue } from '../canvas/ui/floating-insert-menu'

export function changeConditionalOrFragment(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  remixRoutingTable: RemixRoutingTable | null,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  openFile: string | null,
  selectedViews: Array<ElementPath>,
  floatingMenuState: FloatingInsertMenuState,
  fixedSizeForInsertion: boolean,
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
        nodeModules,
        remixRoutingTable,
        openFile,
        jsxMetadata,
        elementPathTree,
        wrapperUID,
        selectedViews.length,
      )
      actionsToDispatch = [
        insertInsertable(
          insertionPath,
          insertableComponent(importsToAdd, element, '', [], null),
          fixedSizeForInsertion ? 'add-size' : 'do-not-add',
          floatingMenuState.indexPosition,
        ),
      ]
      break
    case 'convert': {
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
  nodeModules: NodeModules,
  remixRoutingTable: RemixRoutingTable | null,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  openFile: string | null,
  selectedViews: Array<ElementPath>,
  floatingMenuState: FloatingInsertMenuState,
  fixedSizeForInsertion: boolean,
  addContentForInsertion: boolean,
  pickedInsertableComponent: InsertMenuItemValue,
): Array<EditorAction> {
  if (pickedInsertableComponent.element.type !== 'JSX_ELEMENT') {
    return []
  }
  let actionsToDispatch: Array<EditorAction> = []
  switch (floatingMenuState.insertMenuMode) {
    case 'wrap':
      const newUID = generateUidWithExistingComponents(projectContents)

      const newElement = jsxElement(
        pickedInsertableComponent.element.name,
        newUID,
        setJSXAttributesAttribute(
          pickedInsertableComponent.element.props,
          'data-uid',
          jsExpressionValue(newUID, emptyComments),
        ),
        pickedInsertableComponent.element.children,
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
      if (addContentForInsertion && pickedInsertableComponent.element.children.length === 0) {
        elementToInsert = {
          ...pickedInsertableComponent,
          element: {
            ...pickedInsertableComponent.element,
            children: [jsxTextBlock('Utopia')],
          },
        }
      }

      const targetParent: ElementPath | null =
        floatingMenuState.parentPath ?? safeIndex(selectedViews, 0) ?? null
      if (targetParent != null) {
        const wrapperUID = generateUidWithExistingComponents(projectContents)

        const insertionPath = getInsertionPath(
          targetParent,
          projectContents,
          nodeModules,
          remixRoutingTable,
          openFile,
          jsxMetadata,
          elementPathTree,
          wrapperUID,
          selectedViews.length,
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
    case 'convert':
      const { element, importsToAdd } = pickedInsertableComponent
      // this is taken from render-as.tsx
      const targetsForUpdates = getElementsToTarget(selectedViews)
      actionsToDispatch = targetsForUpdates.flatMap((path) => {
        return updateJSXElementName(path, { type: 'JSX_ELEMENT', name: element.name }, importsToAdd)
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
  nodeModules: NodeModules,
  remixRoutingTable: RemixRoutingTable | null,
  jsxMetadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  openFile: string | null,
  selectedViews: Array<ElementPath>,
  floatingMenuState: FloatingInsertMenuState,
  fixedSizeForInsertion: boolean,
  addContentForInsertion: boolean,
  insertMenuItemValue: InsertMenuItemValue,
): Array<EditorAction> {
  switch (insertMenuItemValue.element.type) {
    case 'JSX_CONDITIONAL_EXPRESSION':
    case 'JSX_FRAGMENT':
      return changeConditionalOrFragment(
        projectContents,
        nodeModules,
        remixRoutingTable,
        jsxMetadata,
        elementPathTree,
        openFile,
        selectedViews,
        floatingMenuState,
        fixedSizeForInsertion,
        insertMenuItemValue.element,
      )
    case 'JSX_ELEMENT':
      return changeElement(
        projectContents,
        nodeModules,
        remixRoutingTable,
        jsxMetadata,
        elementPathTree,
        openFile,
        selectedViews,
        floatingMenuState,
        fixedSizeForInsertion,
        addContentForInsertion,
        insertMenuItemValue,
      )
    default:
      assertNever(insertMenuItemValue.element)
  }
}

export function useConvertTo(): (convertTo: InsertMenuItem | null) => void {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const nodeModulesRef = useRefEditorState((store) => store.editor.nodeModules.files)
  const remixRoutingTableRef = useRefEditorState(
    (store) => store.derived.remixData?.routingTable ?? null,
  )
  const openFileRef = useRefEditorState((store) => store.editor.canvas.openFile?.filename ?? null)
  const floatingMenuStateRef = useRefEditorState((store) => store.editor.floatingInsertMenu)

  return React.useCallback(
    (convertToMenuItem: InsertMenuItem | null) => {
      if (convertToMenuItem != null) {
        const convertTo = convertToMenuItem.value
        const actions = getActionsToApplyChange(
          projectContentsRef.current,
          nodeModulesRef.current,
          remixRoutingTableRef.current,
          jsxMetadataRef.current,
          elementPathTreeRef.current,
          openFileRef.current,
          selectedViewsRef.current,
          floatingMenuStateRef.current,
          false,
          false,
          convertTo,
        )
        dispatch(actions, 'everyone')
      }
    },
    [
      dispatch,
      elementPathTreeRef,
      floatingMenuStateRef,
      jsxMetadataRef,
      nodeModulesRef,
      openFileRef,
      projectContentsRef,
      remixRoutingTableRef,
      selectedViewsRef,
    ],
  )
}
