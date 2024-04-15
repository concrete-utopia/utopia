import React from 'react'
import { useContextMenu, Menu, type ContextMenuParams } from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getJSXElementNameAsString,
  jsxAttributesFromMap,
  jsxElement,
} from '../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { ComponentPicker, type ElementToInsert } from './component-picker'
import type { PreferredChildComponentDescriptor } from '../../custom-code/internal-property-controls'
import { fixUtopiaElement, generateConsistentUID } from '../../../core/shared/uid-utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'
import { elementFromInsertMenuItem } from '../../editor/insert-callbacks'
import { MomentumContextMenu } from '../../context-menu-wrapper'
import { NO_OP } from '../../../core/shared/utils'
import { type ContextMenuItem } from '../../context-menu-items'
import { FlexRow, Icn } from '../../../uuiui'
import { type EditorDispatch } from '../../editor/action-types'
import { type ProjectContentTreeRoot } from '../../assets'
import { type ComponentInfo } from '../../custom-code/code-file'

const usePreferredChildrenForTargetProp = (
  target: ElementPath,
  prop: string,
): Array<PreferredChildComponentDescriptor> => {
  const selectedJSXElement = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.getJSXElementFromMetadata(store.editor.jsxMetadata, target),
    'usePreferredChildrenForSelectedElement selectedJSXElement',
  )

  const preferredChildrenForTargetProp = useEditorState(
    Substores.restOfEditor,
    (store) => {
      if (selectedJSXElement == null) {
        return null
      }

      const targetName = getJSXElementNameAsString(selectedJSXElement.name)
      // TODO: we don't deal with components registered with the same name in multiple files
      for (const file of Object.values(store.editor.propertyControlsInfo)) {
        for (const [name, value] of Object.entries(file)) {
          if (name === targetName) {
            for (const [registeredPropName, registeredPropValue] of Object.entries(
              value.properties,
            )) {
              if (
                registeredPropName === prop &&
                registeredPropValue.control === 'jsx' &&
                registeredPropValue.preferredChildComponents != null
              ) {
                return registeredPropValue.preferredChildComponents
              }
            }
          }
        }
      }

      return null
    },
    'usePreferredChildrenForSelectedElement propertyControlsInfo',
  )

  if (selectedJSXElement == null || preferredChildrenForTargetProp == null) {
    return []
  }

  return preferredChildrenForTargetProp
}

type ShowRenderPropPicker = (
  event: React.MouseEvent<HTMLDivElement>,
  params?: Pick<ContextMenuParams, 'id' | 'props' | 'position'> | undefined,
) => void

export const useShowRenderPropPicker = (
  id: string,
): {
  showRenderPropPicker: ShowRenderPropPicker
  hideRenderPropPicker: () => void
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

  return { showRenderPropPicker: onClick, hideRenderPropPicker: hideAll }
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
        elementToInsert: (uid: string) =>
          elementFromInsertMenuItem(variant.elementToInsert(), uid, 'no-defaults'),
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
  showRenderPropPicker: ShowRenderPropPicker,
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

      showRenderPropPicker(e as React.MouseEvent<any>, {
        position: position,
      })
    },
  }
}

function insertPreferredChild(
  preferredChildToInsert: ElementToInsert,
  target: ElementPath,
  prop: string,
  projectContents: ProjectContentTreeRoot,
  dispatch: EditorDispatch,
) {
  const uniqueIds = new Set(getAllUniqueUids(projectContents).uniqueIDs)
  const uid = generateConsistentUID('prop', new Set(getAllUniqueUids(projectContents).uniqueIDs))
  let element = preferredChildToInsert.elementToInsert(uid)
  uniqueIds.add(uid)

  element = fixUtopiaElement(element, uniqueIds).value

  if (element.type !== 'JSX_ELEMENT') {
    throw new Error('only JSX elements are supported as preferred components')
  }

  dispatch([
    setProp_UNSAFE(
      EP.parentPath(target),
      PP.create(prop),
      element,
      preferredChildToInsert.additionalImports ?? undefined,
    ),
  ])
}

interface RenderPropPickerProps {
  target: ElementPath
  prop: string
  key: string
  id: string
}

const RenderPropPickerSimple = React.memo<RenderPropPickerProps>(({ key, id, target, prop }) => {
  const { showRenderPropPicker } = useShowRenderPropPicker(`${id}-full`)

  const preferredChildrenForTargetProp = usePreferredChildrenForTargetProp(
    EP.parentPath(target),
    prop,
  )

  const dispatch = useDispatch()

  const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)

  const onItemClick = React.useCallback(
    (preferredChildToInsert: ElementToInsert) =>
      insertPreferredChild(
        preferredChildToInsert,
        target,
        prop,
        projectContentsRef.current,
        dispatch,
      ),
    [dispatch, projectContentsRef, prop, target],
  )
  const wrapperRef = React.useRef<HTMLDivElement>(null)

  if (preferredChildrenForTargetProp == null) {
    return null
  }

  const items: Array<ContextMenuItem<unknown>> = preferredChildrenForTargetProp
    .flatMap<ContextMenuItem<unknown>>((data) => {
      const submenuLabel = (
        <FlexRow>
          <Icn category='component' type='default' width={18} height={18} />
          {data.name}
        </FlexRow>
      )

      if (data.variants == null || data.variants.length === 0) {
        return [defaultVariantItem(data.name, submenuLabel, data.imports, null, onItemClick)]
      } else {
        return [
          defaultVariantItem(data.name, '(empty)', data.imports, submenuLabel, onItemClick),
          ...data.variants.map((variant) => {
            return variantItem(variant, submenuLabel, onItemClick)
          }),
        ]
      }
    })
    .concat([separatorItem, moreItem(wrapperRef, showRenderPropPicker)])

  return (
    <div ref={wrapperRef}>
      <MomentumContextMenu id={id} items={items} getData={NO_OP} />
    </div>
  )
})

const RenderPropPickerFull = React.memo<RenderPropPickerProps>(({ key, id, target, prop }) => {
  const { hideRenderPropPicker } = useShowRenderPropPicker(`${id}-full`)

  const preferredChildrenForTargetProp = usePreferredChildrenForTargetProp(
    EP.parentPath(target),
    prop,
  )

  const dispatch = useDispatch()

  const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)

  const onItemClick = React.useCallback(
    (preferredChildToInsert: ElementToInsert) => (e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()

      insertPreferredChild(
        preferredChildToInsert,
        target,
        prop,
        projectContentsRef.current,
        dispatch,
      )
    },
    [dispatch, projectContentsRef, prop, target],
  )

  const squashEvents = React.useCallback((e: React.MouseEvent<unknown>) => {
    e.stopPropagation()
  }, [])

  if (preferredChildrenForTargetProp == null) {
    return null
  }

  return (
    <Menu key={key} id={id} animation={false} style={{ width: 457 }} onClick={squashEvents}>
      <ComponentPicker
        insertionTargetName={prop}
        preferredComponents={preferredChildrenForTargetProp}
        allComponents={preferredChildrenForTargetProp}
        onItemClick={onItemClick}
        onClickCloseButton={hideRenderPropPicker}
      />
    </Menu>
  )
})

export const RenderPropPicker = React.memo<RenderPropPickerProps>(({ key, id, target, prop }) => {
  return (
    <React.Fragment>
      <RenderPropPickerSimple target={target} key={key} id={id} prop={prop} />
      <RenderPropPickerFull target={target} key={`${key}-full`} id={`${id}-full`} prop={prop} />
    </React.Fragment>
  )
})
