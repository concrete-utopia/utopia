import React from 'react'
import { Substores, useEditorState } from '../../../components/editor/store/store-hook'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import { Utils } from '../../../uuiui-deps'
import * as EP from '../../../core/shared/element-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { selectComponents } from '../../../components/editor/actions/meta-actions'
import { Icons, UIRow, UtopiaTheme } from '../../../uuiui'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useDispatch } from '../../editor/store/dispatch-context'

interface ElementPathElement {
  name?: string
  path: ElementPath
}

export const BreadcrumbTrail = React.memo(() => {
  const dispatch = useDispatch()
  const { jsxMetadata, selectedViews, allElementProps, pathTrees } = useEditorState(
    Substores.metadata,
    (store) => {
      return {
        jsxMetadata: store.editor.jsxMetadata,
        selectedViews: store.editor.selectedViews,
        allElementProps: store.editor.allElementProps,
        pathTrees: store.editor.elementPathTree,
      }
    },
    'TopMenuContextProvider',
  )

  const onSelect = React.useCallback(
    (path: ElementPath) => dispatch(selectComponents([path], false), 'everyone'),
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
            name: MetadataUtils.getElementLabel(allElementProps, path, pathTrees, jsxMetadata),
            path: path,
          })
        }
      })
      return elements
    }, [selectedViews, jsxMetadata, allElementProps, pathTrees]),
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
