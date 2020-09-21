import * as React from 'react'
import styled from '@emotion/styled'
import { useEditorState } from '../editor/store/store-hook'
import { isInstancePath, parentPath } from '../../core/shared/template-path'
import { findJSXElementAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { getOpenUtopiaJSXComponentsFromState } from '../editor/store/editor-state'
import { getUtopiaID } from '../../core/model/element-template-utils'
import {
  ComponentMetadata,
  getJSXElementNameAsString,
  JSXElement,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import { InstancePath, TemplatePath } from '../../core/shared/project-file-types'

interface NavigatorItemData {
  id: string
  name: string
  indentation: number
  layoutType: string
}

function getNavigatorItemDataFromJsxElement(
  metadata: ComponentMetadata[],
  path: InstancePath,
  element: JSXElement,
  indentation: number,
): NavigatorItemData {
  return {
    id: getUtopiaID(element),
    name: MetadataUtils.getElementLabel(path, metadata),
    indentation: indentation,
    layoutType:
      MetadataUtils.getElementByInstancePathMaybe(metadata, path)?.specialSizeMeasurements
        .layoutSystemForChildren ?? '',
  }
}

function useGetNavigatorItemsToShow(): Array<NavigatorItemData> {
  return useEditorState((store) => {
    const selectedView = store.editor.selectedViews[0]
    const editedComponents = getOpenUtopiaJSXComponentsFromState(store.editor)
    const metadata = store.editor.jsxMetadataKILLME

    if (selectedView != null) {
      const parent = parentPath(selectedView)
      const parentElement = findJSXElementAtPath(parent, editedComponents, metadata)
      const parentNavigatorEntry: Array<NavigatorItemData> =
        parentElement == null || !isInstancePath(parent)
          ? []
          : [getNavigatorItemDataFromJsxElement(metadata, parent, parentElement, 0)]

      const selectedElement = findJSXElementAtPath(selectedView, editedComponents, metadata)
      const selectedElementEntry: Array<NavigatorItemData> =
        selectedElement == null || !isInstancePath(selectedView)
          ? []
          : [getNavigatorItemDataFromJsxElement(metadata, selectedView, selectedElement, 1)]

      return [...parentNavigatorEntry, ...selectedElementEntry]
    } else {
      return []
    }
  })
}

const MiniNavigatorRoot = styled.div({ position: 'absolute', top: 25, left: 25 })

export const MiniNavigator: React.FunctionComponent = () => {
  const items = useGetNavigatorItemsToShow()

  return (
    <MiniNavigatorRoot>
      {items.map((i) => (
        <MiniNavigatorItem key={i.id} item={i} />
      ))}
    </MiniNavigatorRoot>
  )
}

const LayoutTypeCartouche = styled.span({
  border: '1px solid red',
  borderRadius: 5,
  textTransform: 'uppercase',
  fontSize: '70%',
  color: 'red',
  padding: 1,
  fontWeight: 800,
})

const MiniNavigatorItem: React.FunctionComponent<{ item: NavigatorItemData }> = (props) => {
  return (
    <div>
      <span style={{ width: 10 * props.item.indentation, display: 'inline-block' }}></span>
      <span>⚄ </span>
      {props.item.name} <LayoutTypeCartouche>{props.item.layoutType}</LayoutTypeCartouche>
    </div>
  )
}
