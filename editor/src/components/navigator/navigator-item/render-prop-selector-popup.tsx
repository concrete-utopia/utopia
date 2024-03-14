import React from 'react'
import { useContextMenu, Menu } from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { JSXElement } from '../../../core/shared/element-template'
import {
  getJSXElementNameAsString,
  jsExpressionOtherJavaScriptSimple,
  jsxAttributesFromMap,
  jsxElement,
} from '../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { element } from 'prop-types'
import { v4 as UUID } from 'uuid'
import type { PreferredChildComponentDescriptor } from '../../custom-code/code-file'
import { elementFromInsertMenuItem } from '../../editor/insert-callbacks'

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

  // TODO: the problem is that this gets the `preferredChildComponents` from the
  // jsx control, where it's not parsed what needs to happen is that
  // propertyControls in the editor cannot directly reuse the types from
  // utopia-api, but the jsx control needs its internal type which represents
  // the parsed stuff
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

interface PreferredChildToInsert {
  elementToInsert: JSXElement
  additionalImports: Imports | null
  label: string
}

export const RenderPropPicker = React.memo<RenderPropPickerProps>(({ key, id, target, prop }) => {
  const preferredChildrenForTargetProp = usePreferredChildrenForTargetProp(
    EP.parentPath(target),
    prop,
  )

  const dispatch = useDispatch()

  const onItemClick = React.useCallback(
    (preferredChildToInsert: PreferredChildToInsert) => (e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()

      dispatch([
        // TODO: merge imports stuff
        setProp_UNSAFE(
          EP.parentPath(target),
          PP.create(prop),
          preferredChildToInsert.elementToInsert,
        ),
      ])
    },
    [dispatch, prop, target],
  )

  if (preferredChildrenForTargetProp == null) {
    return null
  }

  const preferredChildComponentsToInsert: Array<PreferredChildToInsert> = (
    preferredChildrenForTargetProp ?? []
  ).flatMap((data) => {
    if (data.variants == null) {
      return [
        {
          label: element.name,
          elementToInsert: jsxElement(element.name, UUID(), jsxAttributesFromMap({}), []),
          additionalImports: data.imports,
        },
      ]
    }
    return data.variants.flatMap((variant) => {
      const elementToInsert = elementFromInsertMenuItem(variant.elementToInsert(), UUID())
      if (elementToInsert.type !== 'JSX_ELEMENT') {
        return []
      }
      return [
        {
          label: variant.insertMenuLabel,
          elementToInsert: elementToInsert,
          additionalImports: variant.importsToAdd,
        },
      ]
    })
  })

  return (
    <Menu key={key} id={id} animation={false} style={{ padding: 8 }}>
      {preferredChildComponentsToInsert.map((option, idx) => (
        <div key={idx} onClick={onItemClick(option)}>
          {option.label}
        </div>
      ))}
    </Menu>
  )
})
