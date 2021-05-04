import * as React from 'react'
import { useEditorState } from '../../../components/editor/store/store-hook'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../utils/react-performance'
import { Utils } from '../../../uuiui-deps'
import * as EP from '../../../core/shared/element-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { selectComponents } from '../../../components/editor/actions/action-creators'
import { Icons, UIRow, UtopiaTheme } from '../../../uuiui'
import { ElementPath } from '../../../core/shared/project-file-types'

interface ElementPathElement {
  name?: string
  path: ElementPath
}

export const BreadcrumbTrail = betterReactMemo('BreadcrumbTrail', () => {
  const { dispatch, jsxMetadata, selectedViews } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      jsxMetadata: store.editor.jsxMetadata,
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
      Utils.fastForEach(EP.allPathsForLastPart(selectedViews[0]), (path) => {
        const component = MetadataUtils.findElementByElementPath(jsxMetadata, path)
        if (component != null) {
          elements.push({
            name: MetadataUtils.getElementLabel(path, jsxMetadata),
            path: path,
          })
        }
      })
      return elements
    }, [selectedViews, jsxMetadata]),
  )

  const lastElemIndex = elementPath.length - 1
  const elements = elementPath.map((elem, index) => {
    const isSelected = index === lastElemIndex
    return (
      <React.Fragment key={`elem-path-${EP.toComponentId(elem.path)}`}>
        <div onMouseDown={() => onSelect(elem.path)} style={{ fontWeight: isSelected ? 500 : 400 }}>
          {elem.name}
        </div>
        {isSelected ? null : <span style={{ paddingLeft: 4, paddingRight: 4 }}>&gt;</span>}
      </React.Fragment>
    )
  })

  return (
    <UIRow rowHeight={'normal'} padded={true}>
      <Icons.Component style={{ marginRight: 4 }} />
      {elements}
    </UIRow>
  )
})
