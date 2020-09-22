import * as React from 'react'
import styled from '@emotion/styled'
import { useEditorState } from '../editor/store/store-hook'
import {
  appendToPath,
  instancePath,
  instancePathForPath,
  isInstancePath,
  parentPath,
} from '../../core/shared/template-path'
import { findJSXElementAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { getOpenUtopiaJSXComponentsFromState } from '../editor/store/editor-state'
import { getUtopiaID } from '../../core/model/element-template-utils'
import {
  ComponentMetadata,
  getJSXElementNameAsString,
  getJSXElementNameLastPart,
  JSXElement,
  JSXElementChild,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'
import { InstancePath, TemplatePath } from '../../core/shared/project-file-types'
import { last } from '../../core/shared/array-utils'

interface NavigatorItemData {
  id: string
  name: string
  indentation: number
  itemType: string
  layoutType: string
  selected: boolean
}

function getElementType(element: JSXElementChild): string {
  switch (element.type) {
    case 'JSX_ARBITRARY_BLOCK':
      return 'Expression'
    case 'JSX_ELEMENT':
      return getJSXElementNameLastPart(element.name)
    case 'JSX_FRAGMENT':
      return 'Fragment'
    case 'JSX_TEXT_BLOCK':
      return 'Text'
  }
}

function getNavigatorItemDataFromJsxElement(
  metadata: ComponentMetadata[],
  path: InstancePath,
  element: JSXElementChild,
  indentation: number,
  selected: boolean,
): NavigatorItemData {
  return {
    id: getUtopiaID(element),
    name: MetadataUtils.getElementLabel(path, metadata),
    indentation: indentation,
    layoutType:
      MetadataUtils.getElementByInstancePathMaybe(metadata, path)?.specialSizeMeasurements
        .layoutSystemForChildren ?? '',
    selected,
    itemType: getElementType(element),
  }
}

function useGetNavigatorItemsToShow(): Array<NavigatorItemData> {
  return useEditorState((store) => {
    const selectedView = instancePathForPath(store.editor.selectedViews[0])
    const editedComponents = getOpenUtopiaJSXComponentsFromState(store.editor)
    const metadata = store.editor.jsxMetadataKILLME

    if (selectedView != null) {
      const selectedViewUid = (isInstancePath(selectedView) && last(selectedView.element)) || null

      const parent = instancePathForPath(parentPath(selectedView))
      const parentElement = findJSXElementAtPath(parent, editedComponents, metadata)
      const parentNavigatorEntry: Array<NavigatorItemData> =
        parentElement == null || parent == null
          ? []
          : [getNavigatorItemDataFromJsxElement(metadata, parent, parentElement, 0, false)]

      const siblings = parentElement?.children ?? []

      const siblingElements =
        parent == null
          ? []
          : siblings.map((s) => {
              const uid = getUtopiaID(s)
              const selected = uid === selectedViewUid
              return getNavigatorItemDataFromJsxElement(
                metadata,
                appendToPath(parent, uid),
                s,
                1,
                selected,
              )
            })

      return [...parentNavigatorEntry, ...siblingElements]
    } else {
      return []
    }
  })
}

const MiniNavigatorRoot = styled.div({
  position: 'absolute',
  top: 25,
  left: 25,
  backgroundImage: 'radial-gradient(at 0% 0%, #FFFFFF 75%, #03030300 100%)',
})

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

const Cartouche = styled.span({
  border: '1px solid black',
  borderRadius: 5,
  textTransform: 'uppercase',
  fontSize: '70%',
  padding: 1,
  fontWeight: 800,
})

const LayoutTypeCartouche = styled(Cartouche)({
  color: 'red',
  borderColor: 'red',
})

const ElementTypeCartouche = styled(Cartouche)({
  color: 'green',
  borderColor: 'green',
})

const MiniNavigatorItem: React.FunctionComponent<{ item: NavigatorItemData }> = (props) => {
  return (
    <div>
      <span style={{ width: 10 * props.item.indentation, display: 'inline-block' }}></span>
      <span>⚄ </span>
      <ElementTypeCartouche>{props.item.itemType}</ElementTypeCartouche>
      <span
        style={{
          color: props.item.selected ? 'blue' : 'black',
        }}
      >
        {' '}
        {props.item.name}{' '}
      </span>
      {props.item.layoutType ? (
        <LayoutTypeCartouche>{props.item.layoutType}</LayoutTypeCartouche>
      ) : null}
    </div>
  )
}
