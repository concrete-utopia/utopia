import * as React from 'react'
import { useEditorState } from '../../../components/editor/store/store-hook'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../utils/react-performance'
import { Utils } from '../../../uuiui-deps'
import * as TP from '../../../core/shared/template-path'
import { getOpenUtopiaJSXComponentsFromState } from '../../editor/store/editor-state'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { selectComponents } from '../../../components/editor/actions/action-creators'
import { FlexRow, Icons, UtopiaTheme } from '../../../uuiui'
import { TemplatePath } from 'src/core/shared/project-file-types'

interface ElementPathElement {
  name?: string
  path: TemplatePath
}

export const BreadcrumbTrail = betterReactMemo('BreadcrumbTrail', () => {
  const { dispatch, jsxMetadata, rootComponents, selectedViews } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      jsxMetadata: store.editor.jsxMetadata,
      rootComponents: getOpenUtopiaJSXComponentsFromState(store.editor),
      selectedViews: store.editor.selectedViews,
    }
  }, 'TopMenuContextProvider')

  const onSelect = React.useCallback(
    (path) => dispatch([selectComponents([path], false)], 'everyone'),
    [dispatch],
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
          const component = MetadataUtils.getElementByInstancePathMaybe(jsxMetadata, path)
          if (component != null) {
            elements.push({
              name: MetadataUtils.getElementLabel(path, jsxMetadata),
              path: path,
            })
          }
        } else {
          const scene = MetadataUtils.findElementByTemplatePath(jsxMetadata, path)
          if (scene != null) {
            elements.push({
              name: scene.label ?? undefined,
              path: path,
            })
          }
        }
      })
      return elements
    }, [selectedViews, jsxMetadata]),
  )

  const lastElemIndex = elementPath.length - 1
  const elements = elementPath.map((elem, index) => {
    const isSelected = index === lastElemIndex
    return (
      <React.Fragment key={`elem-path-${TP.toComponentId(elem.path)}`}>
        <div onMouseDown={() => onSelect(elem.path)} style={{ fontWeight: isSelected ? 500 : 400 }}>
          {elem.name}
        </div>
        {isSelected ? null : <span style={{ paddingLeft: 4, paddingRight: 4 }}>&gt;</span>}
      </React.Fragment>
    )
  })

  return (
    <FlexRow
      style={{
        height: UtopiaTheme.layout.rowHeight.medium,
        padding: `0px ${UtopiaTheme.layout.rowHorizontalPadding}px`,
      }}
    >
      <Icons.Component style={{ marginRight: 4 }} />
      {elements}
    </FlexRow>
  )
})
