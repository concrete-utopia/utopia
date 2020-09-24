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
import { colorTheme, Icn } from '../../uuiui'
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
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'flex-start',
  position: 'absolute',
  top: 25,
  left: 25,
  backgroundImage: 'transparent',
  backgroundColor: 'white',
  borderRadius: 5,
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
    >
      {items.map((i, index) => (
        <MiniNavigatorItem key={i.id} item={i} index={index} isParent={i.indentation === 0} />
      ))}
    </MiniNavigatorRoot>
  )
}

const Cartouche = styled.span({
  borderRadius: 2,
  textTransform: 'uppercase',
  fontSize: '80%',
  padding: 1,
  fontWeight: 800,
})

const LayoutTypeCartouche = styled(Cartouche)({
  color: '#fa6400',
  backgroundColor: '#ffe8d9',
})

const MiniNavigatorItem: React.FunctionComponent<{
  item: NavigatorItemData
  index: number
  isParent: boolean
}> = (props) => {
  const dispatch = useEditorState((store) => store.dispatch)
  return (
    <>
      <div
        onMouseOver={() => {
          dispatch([setHighlightedView(props.item.templatePath)])
        }}
        onMouseDown={() => {
          dispatch([selectComponents([props.item.templatePath], false)])
        }}
        style={{
          position: 'relative',
          // left: 10 * props.item.indentation,
          opacity: 1,
          minWidth: props.isParent ? 0 : 150,
          // top: ItemHeight * props.index,
          // transition: 'top 1s, background-color 0.3s, color 0.3s',
          backgroundColor: props.isParent
            ? '#ffe8d9'
            : props.item.selected
            ? '#e5e5e5'
            : props.item.highlighted
            ? '#f2f2f2'
            : 'white',
          color: '#a0a0a0',
          borderRadius: 5,
          padding: 2,
        }}
      >
        <Icn
          category='element'
          type={`div`}
          color={'darkgray'}
          width={18}
          height={18}
          style={{ opacity: props.isParent ? 0.5 : 1 }}
        />
        <span style={{ paddingRight: 3 }}>{props.item.itemType}</span>
        {props.isParent ? null : <span style={{ float: 'right' }}> {props.item.name} </span>}
      </div>
      {props.isParent ? (
        <div>
          <LayoutTypeCartouche>{props.item.layoutType}</LayoutTypeCartouche>
        </div>
      ) : null}
    </>
  )
}
