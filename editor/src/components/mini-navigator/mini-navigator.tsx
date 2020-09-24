import * as React from 'react'
import styled from '@emotion/styled'
import { motion } from 'framer-motion'
import { useEditorState } from '../editor/store/store-hook'
import {
  appendToPath,
  instancePath,
  instancePathForPath,
  isInstancePath,
  parentPath,
  pathsEqual,
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
import { colorTheme } from '../../uuiui'
import {
  clearHighlightedViews,
  selectComponents,
  setHighlightedView,
} from '../editor/actions/actions'

interface NavigatorItemData {
  id: string
  templatePath: TemplatePath
  name: string
  indentation: number
  itemType: string
  layoutType: string
  selected: boolean
  highlighted: boolean
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
  highlighted: boolean,
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
    highlighted,
    templatePath: path,
  }
}

function isHighlighted(highlightedViews: TemplatePath[], view: TemplatePath) {
  return highlightedViews.findIndex((h) => pathsEqual(h, view)) > -1
}

function useGetNavigatorItemsToShow(): Array<NavigatorItemData> {
  return useEditorState((store) => {
    const selectedView = instancePathForPath(store.editor.selectedViews[0])
    const highlightedViews = store.editor.highlightedViews
    const editedComponents = getOpenUtopiaJSXComponentsFromState(store.editor)
    const metadata = store.editor.jsxMetadataKILLME

    if (selectedView != null) {
      const selectedViewUid = (isInstancePath(selectedView) && last(selectedView.element)) || null

      const parent = instancePathForPath(parentPath(selectedView))
      const parentElement = findJSXElementAtPath(parent, editedComponents, metadata)
      const parentNavigatorEntry: Array<NavigatorItemData> =
        parentElement == null || parent == null
          ? []
          : [
              getNavigatorItemDataFromJsxElement(
                metadata,
                parent,
                parentElement,
                0,
                false,
                isHighlighted(highlightedViews, parent),
              ),
            ]

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
                isHighlighted(highlightedViews, appendToPath(parent, uid)),
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
  backgroundImage: 'transparent',
})

export const MiniNavigator: React.FunctionComponent = () => {
  const items = useGetNavigatorItemsToShow()
  const dispatch = useEditorState((store) => store.dispatch)

  return (
    <MiniNavigatorRoot
      // eslint-disable-next-line react/jsx-no-bind
      onMouseOut={() => {
        dispatch([clearHighlightedViews()])
      }}
      // eslint-disable-next-line react/jsx-no-bind
      onMouseDown={(e) => {
        e.stopPropagation()
        e.preventDefault()
      }}
      style={{
        height: ItemHeight * items.length,
        width: 180,
      }}
    >
      {items.map((i, index) => (
        <MiniNavigatorItem key={i.id} item={i} index={index} />
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

const ItemHeight = 24

const MiniNavigatorItem: React.FunctionComponent<{ item: NavigatorItemData; index: number }> = (
  props,
) => {
  const dispatch = useEditorState((store) => store.dispatch)
  const isParent = props.item.indentation === 0
  return (
    <motion.div
      onMouseOver={() => {
        dispatch([setHighlightedView(props.item.templatePath)])
      }}
      onMouseDown={() => {
        dispatch([selectComponents([props.item.templatePath], false)])
      }}
      style={{
        position: 'absolute',
        left: 10 * props.item.indentation,
        opacity: 1,
        top: ItemHeight * props.index,
        // transition: 'top 1s, background-color 0.3s, color 0.3s',
        backgroundColor: props.item.selected
          ? colorTheme.primary.value
          : props.item.highlighted
          ? colorTheme.primary.shade(50).value
          : 'white',
        color: props.item.selected || props.item.highlighted ? colorTheme.white.value : 'black',
        borderRadius: 5,
        padding: 2,
      }}
    >
      <span>⚄ </span>
      <ElementTypeCartouche>{props.item.itemType}</ElementTypeCartouche>
      <span> {props.item.name} </span>
      {props.item.layoutType && (isParent || props.item.selected) ? (
        <LayoutTypeCartouche>{props.item.layoutType}</LayoutTypeCartouche>
      ) : null}
    </motion.div>
  )
}
