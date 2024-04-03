import React from 'react'
import { useContextMenu, Menu } from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getJSXElementNameAsString } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { ComponentPicker, type ElementToInsert } from './component-picker'
import type { PreferredChildComponentDescriptor } from '../../custom-code/internal-property-controls'
import { generateConsistentUID } from '../../../core/shared/uid-utils'
import { getAllUniqueUids } from '../../../core/model/get-unique-ids'

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

export const useShowRenderPropPicker = (id: string) => {
  const { show, hideAll } = useContextMenu({ id })
  const onClick = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      show(event)
    },
    [show],
  )

  return { showRenderPropPicker: onClick, hideRenderPropPicker: hideAll }
}

interface RenderPropPickerProps {
  target: ElementPath
  prop: string
  key: string
  id: string
}

export const RenderPropPicker = React.memo<RenderPropPickerProps>(({ key, id, target, prop }) => {
  const { hideRenderPropPicker } = useShowRenderPropPicker(id)

  const preferredChildrenForTargetProp = usePreferredChildrenForTargetProp(target, prop)

  const dispatch = useDispatch()
  const targetPropCurrentElementName: string | null = useEditorState(
    Substores.metadata,
    (store) => {
      const targetPropCurrentValue = store.editor.allElementProps[EP.toString(target)]?.[prop]
      if (targetPropCurrentValue == null) {
        return null
      }

      if (typeof targetPropCurrentValue !== 'object' || Array.isArray(targetPropCurrentValue)) {
        return null
      }

      return targetPropCurrentValue.props?.elementToRender?.originalName ?? null
    },
    'RenderPropPicker targetElementProps',
  )

  const projectContentsRef = useRefEditorState((state) => state.editor.projectContents)

  const onItemClick = React.useCallback(
    (preferredChildToInsert: ElementToInsert, isSelectedVariant: boolean) => {
      if (isSelectedVariant) {
        hideRenderPropPicker()
        return
      }

      const uid = generateConsistentUID(
        'prop',
        new Set(getAllUniqueUids(projectContentsRef.current).uniqueIDs),
      )

      const element = preferredChildToInsert.elementToInsert(uid)
      if (element.type !== 'JSX_ELEMENT') {
        throw new Error('only JSX elements are supported as preferred components')
      }

      dispatch([
        setProp_UNSAFE(
          target,
          PP.create(prop),
          element,
          preferredChildToInsert.additionalImports ?? undefined,
        ),
      ])
    },
    [dispatch, projectContentsRef, prop, target, hideRenderPropPicker],
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
        currentElementName={targetPropCurrentElementName}
      />
    </Menu>
  )
})
