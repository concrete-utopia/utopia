import React from 'react'
import type { PropsOrJSXAttributes } from '../../core/model/element-metadata-utils'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../core/model/element-metadata-utils'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { isLeft, isRight, right } from '../../core/shared/either'
import { isJSXElement } from '../../core/shared/element-template'
import { styleP } from './inspector-common'
import type { PropertyPath } from 'utopia-shared/src/types'

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

    function countIfPropertySet(props: PropsOrJSXAttributes, path: PropertyPath): number {
      const attr = getSimpleAttributeAtPath(props, path)
      return isRight(attr) && attr.value != null ? 1 : 0
    }

    return (
      countIfPropertySet(right(grid.element.value.props), styleP('justifyContent')) +
      countIfPropertySet(right(grid.element.value.props), styleP('alignContent')) +
      countIfPropertySet(right(grid.element.value.props), styleP('justifyItems')) +
      countIfPropertySet(right(grid.element.value.props), styleP('alignItems'))
    )
  }, [grid])
}
