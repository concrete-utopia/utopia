import React from 'react'
import {
  useContextMenu,
  Menu,
  type ContextMenuParams,
  contextMenu,
  type TriggerEvent,
} from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  type ElementInstanceMetadata,
  getJSXElementNameAsString,
  jsxAttributesFromMap,
  jsxElement,
  jsxElementFromJSXElementWithoutUID,
} from '../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import {
  deleteView,
  insertAsChildTarget,
  insertInsertable,
  insertJSXElement,
  replaceMappedElement,
  setProp_UNSAFE,
  showToast,
} from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import {
  ComponentPicker,
  elementToInsertToInsertableComponent,
  type ElementToInsert,
} from './component-picker'
import type { PreferredChildComponentDescriptor } from '../../custom-code/internal-property-controls'
import { fixUtopiaElement, generateConsistentUID } from '../../../core/shared/uid-utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import { elementFromInsertMenuItem } from '../../editor/insert-callbacks'
import { MomentumContextMenu } from '../../context-menu-wrapper'
import { NO_OP, assertNever } from '../../../core/shared/utils'
import { type ContextMenuItem } from '../../context-menu-items'
import { FlexRow, Icn, type IcnProps } from '../../../uuiui'
import type {
  EditorAction,
  EditorDispatch,
  InsertAsChildTarget,
  ReplaceKeepChildrenAndStyleTarget,
  ReplaceTarget,
} from '../../editor/action-types'
import { type ProjectContentTreeRoot } from '../../assets'
import type { PropertyControlsInfo, ComponentInfo } from '../../custom-code/code-file'
import { type Icon } from 'utopia-api'
import { getRegisteredComponent } from '../../../core/property-controls/property-controls-utils'
import { defaultImportsForComponentModule } from '../../../core/property-controls/property-controls-local'
import { useGetInsertableComponents } from '../../canvas/ui/floating-insert-menu'
import { atom, useAtom, useSetAtom } from 'jotai'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  replaceWithSingleElement,
} from '../../editor/store/insertion-path'
import type { InsertableComponent } from '../../shared/project-components'
import type { ConditionalCase } from '../../../core/model/conditionals'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { absolute } from '../../../utils/utils'
import { notice } from '../../common/notice'

type RenderPropTarget = { type: 'render-prop'; prop: string }
type ConditionalTarget = { type: 'conditional'; conditionalCase: ConditionalCase }

export type InsertionTarget =
  | RenderPropTarget
  | ReplaceTarget
  | ReplaceKeepChildrenAndStyleTarget
  | InsertAsChildTarget
  | ConditionalTarget

export function renderPropTarget(prop: string): RenderPropTarget {
  return {
    type: 'render-prop',
    prop: prop,
  }
}
export function conditionalTarget(conditionalCase: ConditionalCase): ConditionalTarget {
  return { type: 'conditional', conditionalCase: conditionalCase }
}

export function isReplaceTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ReplaceTarget {
  return insertionTarget.type === 'replace-target'
}

export function isReplaceKeepChildrenAndStyleTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ReplaceKeepChildrenAndStyleTarget {
  return insertionTarget.type === 'replace-target-keep-children-and-style'
}

export function isInsertAsChildTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is InsertAsChildTarget {
  return insertionTarget.type === 'insert-as-child'
}

export function isRenderPropTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is RenderPropTarget {
  return insertionTarget.type === 'render-prop'
}

export function isConditionalTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ConditionalTarget {
  return insertionTarget.type === 'conditional'
}

interface ComponentPickerContextMenuAtomData {
  target: ElementPath
  insertionTarget: InsertionTarget
}

const ComponentPickerContextMenuAtom = atom<ComponentPickerContextMenuAtomData>({
  target: EP.emptyElementPath,
  insertionTarget: insertAsChildTarget(),
})

function getIconForComponent(
  targetName: string,
  moduleName: string | null,
  propertyControlsInfo: PropertyControlsInfo,
): Icon {
  if (moduleName == null) {
    return 'regular'
  }

  const registeredComponent = getRegisteredComponent(targetName, moduleName, propertyControlsInfo)

  return registeredComponent?.icon ?? 'regular'
}

interface PreferredChildComponentDescriptorWithIcon extends PreferredChildComponentDescriptor {
  icon: Icon
}

export function preferredChildrenForTarget(
  targetElement: ElementInstanceMetadata | null,
  insertionTarget: InsertionTarget,
  propertyControlsInfo: PropertyControlsInfo,
): Array<PreferredChildComponentDescriptorWithIcon> {
  const targetJSXElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(targetElement)
  const elementImportInfo = targetElement?.importInfo
  if (elementImportInfo == null || targetJSXElement == null) {
    return []
  }

  const targetName = getJSXElementNameAsString(targetJSXElement.name)
  const registeredComponent = getRegisteredComponent(
    targetName,
    elementImportInfo.filePath,
    propertyControlsInfo,
  )

  // TODO: we don't deal with components registered with the same name in multiple files
  if (registeredComponent != null) {
    if (
      isInsertAsChildTarget(insertionTarget) ||
      isReplaceTarget(insertionTarget) ||
      isReplaceKeepChildrenAndStyleTarget(insertionTarget)
    ) {
      return registeredComponent.preferredChildComponents.map((v) => ({
        ...v,
        icon: getIconForComponent(v.name, v.moduleName, propertyControlsInfo),
      }))
    } else if (isRenderPropTarget(insertionTarget)) {
      for (const [registeredPropName, registeredPropValue] of Object.entries(
        registeredComponent.properties,
      )) {
        if (
          registeredPropName === insertionTarget.prop &&
          registeredPropValue.control === 'jsx' &&
          registeredPropValue.preferredChildComponents != null
        ) {
          return registeredPropValue.preferredChildComponents.map((v) => ({
            ...v,
            icon: getIconForComponent(v.name, v.moduleName, propertyControlsInfo),
          }))
        }
      }
    }
  }

  return []
}

const usePreferredChildrenForTarget = (
  target: ElementPath,
  insertionTarget: InsertionTarget,
): Array<PreferredChildComponentDescriptorWithIcon> => {
  const targetParent =
    isReplaceTarget(insertionTarget) || isReplaceKeepChildrenAndStyleTarget(insertionTarget)
      ? EP.parentPath(target)
      : target

  const targetElement = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, targetParent),
    'usePreferredChildrenForTarget targetElement',
  )

  return useEditorState(
    Substores.restOfEditor,
    (store) => {
      return preferredChildrenForTarget(
        targetElement,
        insertionTarget,
        store.editor.propertyControlsInfo,
      )
    },
    'usePreferredChildrenForSelectedElement propertyControlsInfo',
  )
}

export type ShowComponentPickerContextMenuCallback = (
  target: ElementPath,
  insertionTarget: InsertionTarget,
  pickerType?: 'preferred' | 'full',
) => ShowComponentPickerContextMenu

export type ShowComponentPickerContextMenu = (
  event: TriggerEvent,
  params?: Pick<ContextMenuParams, 'id' | 'props' | 'position'> | undefined,
) => void

const PreferredMenuId = 'component-picker-context-menu'
const FullMenuId = 'component-picker-context-menu-full'

export const useCreateCallbackToShowComponentPicker =
  (): ShowComponentPickerContextMenuCallback => {
    const { show: showPreferred } = useContextMenu({ id: PreferredMenuId })
    const { show: showFull } = useContextMenu({ id: FullMenuId })
    const setContextMenuProps = useSetAtom(ComponentPickerContextMenuAtom)
    const editorRef = useRefEditorState((store) => ({
      jsxMetadata: store.editor.jsxMetadata,
      propertyControlsInfo: store.editor.propertyControlsInfo,
    }))

    return React.useCallback(
      (
          target: ElementPath,
          insertionTarget: InsertionTarget,
          overridePickerType?: 'preferred' | 'full',
        ) =>
        (
          event: TriggerEvent,
          params?: Pick<ContextMenuParams, 'id' | 'props' | 'position'> | undefined,
        ) => {
          event.stopPropagation()
          event.preventDefault()

          let pickerType: 'preferred' | 'full'

          if (overridePickerType == null) {
            const targetParent =
              isReplaceTarget(insertionTarget) ||
              isReplaceKeepChildrenAndStyleTarget(insertionTarget)
                ? EP.parentPath(target)
                : target
            const targetElement = MetadataUtils.findElementByElementPath(
              editorRef.current.jsxMetadata,
              targetParent,
            )
            const preferredChildren = preferredChildrenForTarget(
              targetElement,
              insertionTarget,
              editorRef.current.propertyControlsInfo,
            )

            pickerType = preferredChildren.length > 0 ? 'preferred' : 'full'
          } else {
            pickerType = overridePickerType
          }

          setContextMenuProps({ target: target, insertionTarget: insertionTarget })
          const show = pickerType === 'preferred' ? showPreferred : showFull
          show(event, params)
        },
      [editorRef, showPreferred, showFull, setContextMenuProps],
    )
  }

function defaultVariantItem(
  elementName: string,
  label: string | React.ReactNode,
  imports: Imports,
  submenuName: string | React.ReactNode | null,
  onItemClick: (preferredChildToInsert: ElementToInsert) => void,
): ContextMenuItem<unknown> {
  return {
    name: label,
    submenuName: submenuName,
    enabled: true,
    action: () =>
      onItemClick({
        name: elementName,
        elementToInsert: (uid: string) =>
          jsxElement(elementName, uid, jsxAttributesFromMap({}), []),
        additionalImports: imports,
      }),
  }
}

function variantItem(
  variant: ComponentInfo,
  submenuName: string | React.ReactNode | null,
  onItemClick: (preferredChildToInsert: ElementToInsert) => void,
): ContextMenuItem<unknown> {
  return {
    name: variant.insertMenuLabel,
    submenuName: submenuName,
    enabled: true,
    action: () =>
      onItemClick({
        name: variant.insertMenuLabel,
        elementToInsert: (uid: string) => elementFromInsertMenuItem(variant.elementToInsert(), uid),
        additionalImports: variant.importsToAdd,
      }),
  }
}

const separatorItem: ContextMenuItem<unknown> = {
  name: <div key='separator' className='react-contexify__separator' />,
  enabled: false,
  isSeparator: true,
  action: () => null,
}

function moreItem(
  menuWrapperRef: React.RefObject<HTMLDivElement>,
  showComponentPickerContextMenu: ShowComponentPickerContextMenu,
): ContextMenuItem<unknown> {
  return {
    name: <FlexRow style={{ paddingLeft: 22 }}>More…</FlexRow>,
    enabled: true,
    action: (_data, _dispatch, _rightClickCoordinate, e) => {
      const currentMenu = (menuWrapperRef.current?.childNodes[0] as HTMLDivElement) ?? null
      const position =
        currentMenu == null
          ? undefined
          : {
              x: currentMenu.offsetLeft,
              y: currentMenu.offsetTop,
            }

      showComponentPickerContextMenu(e as React.MouseEvent<any>, {
        position: position,
      })
    },
  }
}

function insertComponentPickerItem(
  toInsert: InsertableComponent,
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  dispatch: EditorDispatch,
  insertionTarget: InsertionTarget,
) {
  const uniqueIds = new Set(getAllUniqueUids(projectContents).uniqueIDs)
  const uid = generateConsistentUID('prop', uniqueIds)
  const elementWithoutUID = toInsert.element()

  const actions = ((): Array<EditorAction> => {
    if (elementWithoutUID.type === 'JSX_ELEMENT') {
      const element = jsxElementFromJSXElementWithoutUID(elementWithoutUID, uid)
      const fixedElement = fixUtopiaElement(element, uniqueIds).value

      if (fixedElement.type !== 'JSX_ELEMENT') {
        throw new Error('JSXElementWithoutUid is not converted to JSXElement')
      }

      // if we are inserting into a render prop
      if (isRenderPropTarget(insertionTarget)) {
        return [
          setProp_UNSAFE(
            target,
            PP.create(insertionTarget.prop),
            fixedElement,
            toInsert.importsToAdd ?? undefined,
          ),
        ]
      }

      // if we are inserting into a map expression then we replace the mapped element
      if (MetadataUtils.isJSXMapExpression(EP.parentPath(target), metadata)) {
        return [replaceMappedElement(fixedElement, target, toInsert.importsToAdd)]
      }

      if (!isConditionalTarget(insertionTarget)) {
        return [
          insertJSXElement(
            fixedElement,
            target,
            toInsert.importsToAdd ?? undefined,
            insertionTarget,
          ),
        ]
      }
    }

    if (isInsertAsChildTarget(insertionTarget)) {
      return [insertInsertable(childInsertionPath(target), toInsert, 'do-not-add', null)]
    }

    if (isConditionalTarget(insertionTarget)) {
      return [
        insertInsertable(
          conditionalClauseInsertionPath(
            target,
            insertionTarget.conditionalCase,
            replaceWithSingleElement(),
          ),
          toInsert,
          'do-not-add',
          null,
        ),
      ]
    }

    if (isReplaceTarget(insertionTarget)) {
      if (
        MetadataUtils.isJSXMapExpression(EP.parentPath(target), metadata) &&
        elementWithoutUID.type !== 'JSX_ELEMENT'
      ) {
        return [
          showToast(
            notice(
              'We are working on support to insert Lists, Conditionals and Fragments into Lists',
              'INFO',
              false,
              'insert-component-picker-item-nested-map',
            ),
          ),
        ]
      }
      const index = MetadataUtils.getIndexInParent(metadata, pathTrees, target)
      return [
        deleteView(target),
        insertInsertable(
          childInsertionPath(EP.parentPath(target)),
          toInsert,
          'do-not-add',
          absolute(index),
        ),
      ]
    }

    return [
      showToast(
        notice(
          toastMessage(insertionTarget, toInsert),
          'INFO',
          false,
          'insert-component-picker-item-nested-map',
        ),
      ),
    ]
  })()

  dispatch(actions)
}

function toastMessage(insertionTarget: InsertionTarget, toInsert: InsertableComponent) {
  switch (insertionTarget.type) {
    case 'replace-target':
      return `Swapping to ${toInsert.name} isn't supported yet`
    case 'replace-target-keep-children-and-style':
      return `Replacing with ${toInsert.name} isn't supported yet`
    case 'insert-as-child':
      return `Inserting ${toInsert.name} as child isn't supported yet`
    case 'conditional':
      return `Inserting ${toInsert.name} into conditional ${insertionTarget.type} isn't supported yet`
    case 'render-prop':
      return `Inserting ${toInsert.name} into render prop ${insertionTarget.prop} isn't supported yet`
    default:
      assertNever(insertionTarget)
  }
}

function insertPreferredChild(
  preferredChildToInsert: ElementToInsert,
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  dispatch: EditorDispatch,
  insertionTarget: InsertionTarget,
) {
  const uniqueIds = new Set(getAllUniqueUids(projectContents).uniqueIDs)
  const uid = generateConsistentUID('prop', uniqueIds)
  const toInsert = elementToInsertToInsertableComponent(
    preferredChildToInsert,
    uid,
    ['do-not-add'],
    null,
    null,
    null,
  )

  insertComponentPickerItem(
    toInsert,
    target,
    projectContents,
    metadata,
    pathTrees,
    dispatch,
    insertionTarget,
  )
}

interface ComponentPickerContextMenuProps {
  target: ElementPath
  insertionTarget: InsertionTarget
}

export function iconPropsForIcon(icon: Icon): IcnProps {
  return {
    category: 'navigator-element',
    type: icon,
    color: 'white',
  }
}

export function labelTestIdForComponentIcon(
  componentName: string,
  moduleName: string,
  icon: Icon,
): string {
  return `variant-label-${componentName}-${moduleName}-${icon}`
}

const ComponentPickerContextMenuSimple = React.memo<ComponentPickerContextMenuProps>(
  ({ target, insertionTarget }) => {
    const showFullMenu = useCreateCallbackToShowComponentPicker()(target, insertionTarget, 'full')

    const preferredChildren = usePreferredChildrenForTarget(target, insertionTarget)

    const dispatch = useDispatch()

    const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)
    const metadataRef = useRefEditorState((state) => state.editor.jsxMetadata)
    const elementPathTreesRef = useRefEditorState((state) => state.editor.elementPathTree)

    const onItemClick = React.useCallback(
      (preferredChildToInsert: ElementToInsert) =>
        insertPreferredChild(
          preferredChildToInsert,
          target,
          projectContentsRef.current,
          metadataRef.current,
          elementPathTreesRef.current,
          dispatch,
          insertionTarget,
        ),
      [target, projectContentsRef, metadataRef, elementPathTreesRef, dispatch, insertionTarget],
    )
    const wrapperRef = React.useRef<HTMLDivElement>(null)

    const items: Array<ContextMenuItem<unknown>> = preferredChildren
      .flatMap<ContextMenuItem<unknown>>((data) => {
        const iconProps = iconPropsForIcon(data.icon)

        const submenuLabel = (
          <FlexRow
            style={{ gap: 10 }}
            data-testId={labelTestIdForComponentIcon(data.name, data.moduleName ?? '', data.icon)}
          >
            <Icn {...iconProps} width={12} height={12} />
            {data.name}
          </FlexRow>
        )

        const defaultVariantImports = defaultImportsForComponentModule(data.name, data.moduleName)

        if (data.variants == null || data.variants.length === 0) {
          return [
            defaultVariantItem(data.name, submenuLabel, defaultVariantImports, null, onItemClick),
          ]
        } else {
          return [
            defaultVariantItem(
              data.name,
              '(empty)',
              defaultVariantImports,
              submenuLabel,
              onItemClick,
            ),
            ...data.variants.map((variant) => {
              return variantItem(variant, submenuLabel, onItemClick)
            }),
          ]
        }
      })
      .concat([separatorItem, moreItem(wrapperRef, showFullMenu)])

    return (
      <div ref={wrapperRef}>
        <MomentumContextMenu id={PreferredMenuId} items={items} getData={NO_OP} />
      </div>
    )
  },
)

const ComponentPickerContextMenuFull = React.memo<ComponentPickerContextMenuProps>(
  ({ target, insertionTarget }) => {
    const allInsertableComponents = useGetInsertableComponents('insert').flatMap((g) => ({
      label: g.label,
      options: g.options.filter((o) => {
        if (
          isInsertAsChildTarget(insertionTarget) ||
          isConditionalTarget(insertionTarget) ||
          isReplaceTarget(insertionTarget)
        ) {
          return true
        }
        // Right now we only support inserting JSX elements when we insert into a render prop or when replacing elements
        return o.value.element().type === 'JSX_ELEMENT'
      }),
    }))

    const dispatch = useDispatch()

    const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)
    const metadataRef = useRefEditorState((state) => state.editor.jsxMetadata)
    const elementPathTreesRef = useRefEditorState((state) => state.editor.elementPathTree)

    const onItemClick = React.useCallback(
      (preferredChildToInsert: InsertableComponent) => (e: React.UIEvent) => {
        e.stopPropagation()
        e.preventDefault()

        insertComponentPickerItem(
          preferredChildToInsert,
          target,
          projectContentsRef.current,
          metadataRef.current,
          elementPathTreesRef.current,
          dispatch,
          insertionTarget,
        )

        contextMenu.hideAll()
      },
      [target, projectContentsRef, metadataRef, elementPathTreesRef, dispatch, insertionTarget],
    )

    const squashEvents = React.useCallback((e: React.UIEvent<unknown>) => {
      e.stopPropagation()
    }, [])

    return (
      <Menu id={FullMenuId} animation={false} style={{ width: 260 }} onClick={squashEvents}>
        <ComponentPicker allComponents={allInsertableComponents} onItemClick={onItemClick} />
      </Menu>
    )
  },
)

export const ComponentPickerContextMenu = React.memo(() => {
  const [{ target, insertionTarget }] = useAtom(ComponentPickerContextMenuAtom)

  return (
    <React.Fragment>
      <ComponentPickerContextMenuSimple target={target} insertionTarget={insertionTarget} />
      <ComponentPickerContextMenuFull target={target} insertionTarget={insertionTarget} />
    </React.Fragment>
  )
})
