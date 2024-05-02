import React from 'react'
import {
  useContextMenu,
  Menu,
  type ContextMenuParams,
  contextMenu,
  type TriggerEvent,
} from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
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
  insertInsertable,
  insertJSXElement,
  replaceMappedElement,
  setProp_UNSAFE,
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
import type { EditorAction, EditorDispatch } from '../../editor/action-types'
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

export type RenderPropInsertionTarget = { prop: string }
export type ReplaceInsertionTarget = 'replace-target' | 'replace-target-keep-children-and-style'
export type ChildInsertionTarget = 'insert-as-child'

export type InsertionTarget =
  | RenderPropInsertionTarget
  | ReplaceInsertionTarget
  | ChildInsertionTarget
  | ConditionalCase

export function isRenderPropInsertionTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is RenderPropInsertionTarget {
  return (
    !isChildInsertionTarget(insertionTarget) &&
    !isReplaceInsertionTarget(insertionTarget) &&
    !isConditionalCaseInsertionTarget(insertionTarget)
  )
}

export function isReplaceInsertionTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ReplaceInsertionTarget {
  return (
    insertionTarget === 'replace-target' ||
    insertionTarget === 'replace-target-keep-children-and-style'
  )
}

export function isChildInsertionTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ChildInsertionTarget {
  return insertionTarget === 'insert-as-child'
}

export function isConditionalCaseInsertionTarget(
  insertionTarget: InsertionTarget,
): insertionTarget is ConditionalCase {
  return insertionTarget === 'true-case' || insertionTarget === 'false-case'
}

interface ComponentPickerContextMenuAtomData {
  target: ElementPath
  insertionTarget: InsertionTarget
}

const ComponentPickerContextMenuAtom = atom<ComponentPickerContextMenuAtomData>({
  target: EP.emptyElementPath,
  insertionTarget: 'insert-as-child',
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
    if (isChildInsertionTarget(insertionTarget) || isReplaceInsertionTarget('replace-target')) {
      return registeredComponent.preferredChildComponents.map((v) => ({
        ...v,
        icon: getIconForComponent(v.name, v.moduleName, propertyControlsInfo),
      }))
    } else if (isRenderPropInsertionTarget(insertionTarget)) {
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
  const targetParent = isReplaceInsertionTarget(insertionTarget) ? EP.parentPath(target) : target

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
          let pickerType: 'preferred' | 'full'

          if (overridePickerType == null) {
            const targetParent = isReplaceInsertionTarget(insertionTarget)
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
    name: (
      <FlexRow>
        <div
          style={{
            width: 18,
            height: 18,
            display: 'flex',
            justifyItems: 'center',
            alignItems: 'center',
            position: 'relative',
          }}
        ></div>{' '}
        More...
      </FlexRow>
    ),
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
      if (isRenderPropInsertionTarget(insertionTarget)) {
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

      if (!isConditionalCaseInsertionTarget(insertionTarget)) {
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

    // TODO: for non-jsx-elements we only support insertion as a child today, this should be extended
    if (insertionTarget === 'insert-as-child') {
      return [insertInsertable(childInsertionPath(target), toInsert, 'do-not-add', null)]
    }

    if (isConditionalCaseInsertionTarget(insertionTarget)) {
      return [
        insertInsertable(
          conditionalClauseInsertionPath(target, insertionTarget, replaceWithSingleElement()),
          toInsert,
          'do-not-add',
          null,
        ),
      ]
    }

    console.warn(errorMessage(insertionTarget, toInsert))
    return []
  })()

  dispatch(actions)
}

function errorMessage(insertionTarget: InsertionTarget, toInsert: InsertableComponent) {
  switch (insertionTarget) {
    case 'replace-target':
      return `Component picker error: can not swap to "${toInsert.name}"`
    case 'replace-target-keep-children-and-style':
      return `Component picker error: can not replace to "${toInsert.name}"`
    case 'insert-as-child':
      return `Component picker error: can not insert "${toInsert.name}" as child`
    case 'false-case':
    case 'true-case':
      return `Component picker error: can not insert "${toInsert.name}" into conditional`
    default:
      return `Component picker error: can not insert "${toInsert.name}" into render prop "${insertionTarget.prop}"`
  }
}

function insertPreferredChild(
  preferredChildToInsert: ElementToInsert,
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
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

  insertComponentPickerItem(toInsert, target, projectContents, metadata, dispatch, insertionTarget)
}

interface ComponentPickerContextMenuProps {
  target: ElementPath
  insertionTarget: InsertionTarget
}

function iconPropsForIcon(icon: Icon): IcnProps {
  switch (icon) {
    case 'column':
      return {
        category: 'navigator-element',
        type: 'flex-column',
        color: 'white',
      }
    case 'row':
      return {
        category: 'navigator-element',
        type: 'flex-row',
        color: 'white',
      }
    case 'regular':
      return {
        category: 'navigator-element',
        type: 'component',
        color: 'white',
      }
    default:
      assertNever(icon)
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

    const onItemClick = React.useCallback(
      (preferredChildToInsert: ElementToInsert) =>
        insertPreferredChild(
          preferredChildToInsert,
          target,
          projectContentsRef.current,
          metadataRef.current,
          dispatch,
          insertionTarget,
        ),
      [target, projectContentsRef, metadataRef, dispatch, insertionTarget],
    )
    const wrapperRef = React.useRef<HTMLDivElement>(null)

    const items: Array<ContextMenuItem<unknown>> = preferredChildren
      .flatMap<ContextMenuItem<unknown>>((data) => {
        const iconProps = iconPropsForIcon(data.icon)

        const submenuLabel = (
          <FlexRow
            style={{ gap: 5 }}
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
          insertionTarget === 'insert-as-child' ||
          isConditionalCaseInsertionTarget(insertionTarget)
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

    const onItemClick = React.useCallback(
      (preferredChildToInsert: InsertableComponent) => (e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()

        insertComponentPickerItem(
          preferredChildToInsert,
          target,
          projectContentsRef.current,
          metadataRef.current,
          dispatch,
          insertionTarget,
        )

        contextMenu.hideAll()
      },
      [target, projectContentsRef, metadataRef, dispatch, insertionTarget],
    )

    const squashEvents = React.useCallback((e: React.MouseEvent<unknown>) => {
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
