import React from 'react'
import { countSetProperties, MetadataUtils } from '../../core/model/element-metadata-utils'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { isLeft, right } from '../../core/shared/either'
import { isJSXElement } from '../../core/shared/element-template'
import { styleP } from './inspector-common'

/**
 * These should match the props managed by the Advanced menu of the Grid inspector section.
 */
const advancedGridProps = [
  styleP('justifyContent'),
  styleP('alignContent'),
  styleP('justifyItems'),
  styleP('alignItems'),
]

export function useGridAdvancedPropertiesCount(): number {
  const grid = useEditorState(
    Substores.metadata,
    (store) => {
      if (store.editor.selectedViews.length !== 1) {
        return null
      }
      const element = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )
      return MetadataUtils.isGridLayoutedContainer(element) ? element : null
    },
    'useGridAdvancedProperties grids',
  )

  return React.useMemo(() => {
    if (grid == null || isLeft(grid.element) || !isJSXElement(grid.element.value)) {
      return 0
    }

    return countSetProperties(advancedGridProps, right(grid.element.value.props))
  }, [grid])
}
