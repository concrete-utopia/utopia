import React from 'react'
import { arrayAccumulate } from '../../../core/shared/array-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { treatElementAsGroupLike } from '../canvas-strategies/strategies/group-helpers'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { OutlineControl } from './select-mode/simple-outline-control'

export const AbsoluteChildrenOutline = React.memo(() => {
  const absoluteChildren = useEditorState(
    Substores.metadata,
    (store) => {
      return arrayAccumulate<ElementPath>((childrenArray) => {
        // Work through the selected views...
        for (const selectedView of store.editor.selectedViews) {
          // ...As long as they are not groups.
          if (!treatElementAsGroupLike(store.editor.jsxMetadata, selectedView)) {
            // Taking the children of those selected views...
            const selectedViewChildren = MetadataUtils.getChildrenPathsOrdered(
              store.editor.elementPathTree,
              selectedView,
            )
            for (const selectedViewChild of selectedViewChildren) {
              const metadata = MetadataUtils.findElementByElementPath(
                store.editor.jsxMetadata,
                selectedViewChild,
              )
              // ...Only including those children if they are positioned absolutely.
              if (MetadataUtils.isPositionAbsolute(metadata)) {
                childrenArray.push(selectedViewChild)
              }
            }
          }
        }
      })
    },
    'AbsoluteChildrenOutline absoluteChildrenBoundingBox',
  )

  if (absoluteChildren.length === 0) {
    return null
  }
  return (
    <CanvasOffsetWrapper>
      <OutlineControl
        testId={`absolute-children-outline`}
        targets={absoluteChildren}
        outlineStyle={'dotted'}
        color='multiselect-bounds'
      />
    </CanvasOffsetWrapper>
  )
})
