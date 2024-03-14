import React from 'react'
import { useContextMenu, Menu } from 'react-contexify'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  getJSXElementNameAsString,
  jsExpressionOtherJavaScriptSimple,
} from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { setProp_UNSAFE } from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'

const usePreferredChildrenForTargetProp = (target: ElementPath, prop: string) => {
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
    return null
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
  const preferredChildrenForTargetProp = usePreferredChildrenForTargetProp(
    EP.parentPath(target),
    prop,
  )

  const dispatch = useDispatch()

  const onItemClick = React.useCallback(
    (rawJSCodeForRenderProp: string) => (e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()

      dispatch([
        setProp_UNSAFE(
          EP.parentPath(target),
          PP.create(prop),
          jsExpressionOtherJavaScriptSimple(rawJSCodeForRenderProp, []),
        ),
      ])
    },
    [dispatch, prop, target],
  )

  if (preferredChildrenForTargetProp == null) {
    return null
  }

  const rawJSToInsert: Array<string> = (preferredChildrenForTargetProp ?? []).flatMap((data) =>
    data.variants == null ? `<${data.name} />` : data.variants.map((v) => v.code),
  )

  return (
    <Menu key={key} id={id} animation={false} style={{ padding: 8 }}>
      {rawJSToInsert.map((option, idx) => (
        <div key={idx} onClick={onItemClick(option)}>
          {option}
        </div>
      ))}
    </Menu>
  )
})
