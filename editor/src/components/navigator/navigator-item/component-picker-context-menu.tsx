import React from 'react'
import { useContextMenu, Menu, type ContextMenuParams, contextMenu } from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getJSXElementNameAsString,
  jsxAttributesFromMap,
  jsxElement,
} from '../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { insertJSXElement, setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { ComponentPicker, type ElementToInsert } from './component-picker'
import type { PreferredChildComponentDescriptor } from '../../custom-code/internal-property-controls'
import { fixUtopiaElement, generateConsistentUID } from '../../../core/shared/uid-utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import { elementFromInsertMenuItem } from '../../editor/insert-callbacks'
import { MomentumContextMenu } from '../../context-menu-wrapper'
import { NO_OP, assertNever } from '../../../core/shared/utils'
import { type ContextMenuItem } from '../../context-menu-items'
import { FlexRow, Icn, type IcnProps } from '../../../uuiui'
import { type EditorDispatch } from '../../editor/action-types'
import { type ProjectContentTreeRoot } from '../../assets'
import {
  type PropertyControlsInfo,
  type ComponentInfo,
  ComponentDescriptor,
} from '../../custom-code/code-file'
import { type Icon } from 'utopia-api'
import { getRegisteredComponent } from '../../../core/property-controls/property-controls-utils'
import { defaultImportsForComponentModule } from '../../../core/property-controls/property-controls-local'
import { useGetInsertableComponents } from '../../canvas/ui/floating-insert-menu'
import { atom, useAtom } from 'jotai'

export type InsertionTarget = { prop: string } | 'replace-target' | 'insert-as-child'
interface ComponentPickerContextMenuAtomData {
  target: ElementPath
  insertionTarget: InsertionTarget
}

export const ComponentPickerContextMenuAtom = atom<ComponentPickerContextMenuAtomData>({
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

const usePreferredChildrenForTarget = (
  target: ElementPath,
  insertionTarget: InsertionTarget,
): Array<PreferredChildComponentDescriptorWithIcon> => {
  const targetParent = insertionTarget === 'replace-target' ? EP.parentPath(target) : target

  const targetElement = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, targetParent),
    'usePreferredChildrenForTarget targetElement',
  )

  const preferredChildrenForTarget = useEditorState(
    Substores.restOfEditor,
    (store) => {
      const targetJSXElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(targetElement)
      const elementImportInfo = targetElement?.importInfo
      if (elementImportInfo == null || targetJSXElement == null) {
        return null
      }

      const targetName = getJSXElementNameAsString(targetJSXElement.name)
      const registeredComponent = getRegisteredComponent(
        targetName,
        elementImportInfo.filePath,
        store.editor.propertyControlsInfo,
      )

      // TODO: we don't deal with components registered with the same name in multiple files
      if (registeredComponent != null) {
        if (insertionTarget === 'insert-as-child' || insertionTarget === 'replace-target') {
          return registeredComponent.preferredChildComponents.map((v) => ({
            ...v,
            icon: getIconForComponent(v.name, v.moduleName, store.editor.propertyControlsInfo),
          }))
        } else {
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
                icon: getIconForComponent(v.name, v.moduleName, store.editor.propertyControlsInfo),
              }))
            }
          }
        }
      }

      return null
    },
    'usePreferredChildrenForSelectedElement propertyControlsInfo',
  )

  return preferredChildrenForTarget ?? []
}

type ShowComponentPickerContextMenu = (
  event: React.MouseEvent<HTMLDivElement>,
  params?: Pick<ContextMenuParams, 'id' | 'props' | 'position'> | undefined,
) => void

const useShowComponentPickerContextMenuInner = (
  id: string,
): {
  showComponentPickerContextMenu: ShowComponentPickerContextMenu
  hideComponentPickerContextMenu: () => void
} => {
  const { show, hideAll } = useContextMenu({ id })
  const onClick = React.useCallback(
    (
      event: React.MouseEvent<HTMLDivElement>,
      params?: Pick<ContextMenuParams, 'id' | 'props' | 'position'> | undefined,
    ) => {
      show(event, params)
    },
    [show],
  )

  return { showComponentPickerContextMenu: onClick, hideComponentPickerContextMenu: hideAll }
}

const ComponentPickerContextMenuId = 'component-picker-context-menu'
const ComponentPickerContextMenuFullId = 'component-picker-context-menu-full'

export const useShowComponentPickerContextMenu = () =>
  useShowComponentPickerContextMenuInner(ComponentPickerContextMenuId)
export const useShowComponentPickerContextMenuFull = () =>
  useShowComponentPickerContextMenuInner(ComponentPickerContextMenuFullId)

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

function insertPreferredChild(
  preferredChildToInsert: ElementToInsert,
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  dispatch: EditorDispatch,
  insertionTarget: InsertionTarget,
) {
  const uniqueIds = new Set(getAllUniqueUids(projectContents).uniqueIDs)
  const uid = generateConsistentUID('prop', uniqueIds)
  let element = preferredChildToInsert.elementToInsert(uid)

  element = fixUtopiaElement(element, uniqueIds).value

  if (element.type !== 'JSX_ELEMENT') {
    throw new Error('only JSX elements are supported as preferred components')
  }

  const insertionAction =
    insertionTarget === 'replace-target' || insertionTarget === 'insert-as-child'
      ? insertJSXElement(
          element,
          target,
          preferredChildToInsert.additionalImports ?? undefined,
          insertionTarget,
        )
      : setProp_UNSAFE(
          target,
          PP.create(insertionTarget.prop),
          element,
          preferredChildToInsert.additionalImports ?? undefined,
        )

  dispatch([insertionAction])
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
    const { showComponentPickerContextMenu } = useShowComponentPickerContextMenuFull()

    const preferredChildrenForTarget = usePreferredChildrenForTarget(target, insertionTarget)

    const dispatch = useDispatch()

    const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)

    const onItemClick = React.useCallback(
      (preferredChildToInsert: ElementToInsert) =>
        insertPreferredChild(
          preferredChildToInsert,
          target,
          projectContentsRef.current,
          dispatch,
          insertionTarget,
        ),
      [dispatch, projectContentsRef, insertionTarget, target],
    )
    const wrapperRef = React.useRef<HTMLDivElement>(null)

    if (preferredChildrenForTarget == null) {
      return null
    }

    const items: Array<ContextMenuItem<unknown>> = preferredChildrenForTarget
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
      .concat([separatorItem, moreItem(wrapperRef, showComponentPickerContextMenu)])

    return (
      <div ref={wrapperRef}>
        <MomentumContextMenu id={ComponentPickerContextMenuId} items={items} getData={NO_OP} />
      </div>
    )
  },
)

const ComponentPickerContextMenuFull = React.memo<ComponentPickerContextMenuProps>(
  ({ target, insertionTarget }) => {
    const allInsertableComponents = useGetInsertableComponents('insert').flatMap((g) => ({
      label: g.label,
      options: g.options,
    }))

    const dispatch = useDispatch()

    const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)

    const onItemClick = React.useCallback(
      (preferredChildToInsert: ElementToInsert) => (e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()

        insertPreferredChild(
          preferredChildToInsert,
          target,
          projectContentsRef.current,
          dispatch,
          insertionTarget,
        )

        contextMenu.hideAll()
      },
      [dispatch, projectContentsRef, insertionTarget, target],
    )

    const squashEvents = React.useCallback((e: React.MouseEvent<unknown>) => {
      e.stopPropagation()
    }, [])

    return (
      <Menu
        id={ComponentPickerContextMenuFullId}
        animation={false}
        style={{ width: 260 }}
        onClick={squashEvents}
      >
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
