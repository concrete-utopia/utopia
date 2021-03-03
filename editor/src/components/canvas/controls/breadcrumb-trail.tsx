import * as React from 'react'
import { useEditorState } from '../../../components/editor/store/store-hook'
import { ElementPathElement } from '../../../components/inspector/sections/header-section/element-path'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../utils/react-performance'
import { Utils } from '../../../uuiui-deps'
import * as TP from '../../../core/shared/template-path'
import { getOpenUtopiaJSXComponentsFromState } from '../../editor/store/editor-state'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { selectComponents } from '../../../components/editor/actions/action-creators'
import { FlexRow, Icons } from '../../../uuiui'
import { GridRow } from '../../../components/inspector/widgets/grid-row'

export const BreadcrumbTrail = () => {
  const { dispatch, jsxMetadataKILLME, rootComponents } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      jsxMetadataKILLME: store.editor.jsxMetadataKILLME,
      rootComponents: getOpenUtopiaJSXComponentsFromState(store.editor),
    }
  }, 'InspectorContextProvider')

  const onSelect = React.useCallback(
    (path) => dispatch([selectComponents([path], false)], 'everyone'),
    [dispatch],
  )

  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'InspectorEntryPoint selectedViews',
  )

  const elementPath = useKeepReferenceEqualityIfPossible(
    React.useMemo(() => {
      if (selectedViews.length === 0) {
        return []
      }
      let elements: Array<ElementPathElement> = []
      Utils.fastForEach(TP.allPaths(selectedViews[0]), (path) => {
        // TODO Scene Implementation
        if (TP.isInstancePath(path)) {
          const component = MetadataUtils.getElementByInstancePathMaybe(
            jsxMetadataKILLME.elements,
            path,
          )
          if (component != null) {
            elements.push({
              name: MetadataUtils.getElementLabel(path, jsxMetadataKILLME),
              path: path,
            })
          }
        } else {
          const scene = MetadataUtils.findSceneByTemplatePath(jsxMetadataKILLME.components, path)
          if (scene != null) {
            elements.push({
              name: scene.label,
              path: path,
            })
          }
        }
      })
      return elements
    }, [selectedViews, jsxMetadataKILLME]),
  )

  const ElementPathButtons = betterReactMemo('ElementPathButtons', () => {
    const lastElemIndex = elementPath.length - 1
    const elements = elementPath.map((elem, index) => {
      const isSelected = index === lastElemIndex
      return (
        <React.Fragment key={`elem-path-${TP.toComponentId(elem.path)}`}>
          <div
            onMouseDown={() => onSelect(elem.path)}
            style={{ fontWeight: isSelected ? 500 : 400 }}
          >
            {elem.name}
          </div>
          {isSelected ? null : <span style={{ paddingLeft: 4, paddingRight: 4 }}>&gt;</span>}
        </React.Fragment>
      )
    })

    return (
      <FlexRow style={{ paddingLeft: 6, paddingTop: 4 }}>
        <Icons.Component />
        {elements}
      </FlexRow>
    )
  })

  return (
    <ElementPathButtons />
  )
}
