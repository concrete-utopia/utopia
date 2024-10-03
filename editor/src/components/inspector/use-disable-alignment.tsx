import React from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementInstanceMetadata } from '../../core/shared/element-template'
import { useEditorState, Substores } from '../editor/store/store-hook'
import * as EP from '../../core/shared/element-path'

export function useDisableAlignment(
  selectedViews: ElementPath[],
  orientation: 'horizontal' | 'vertical',
) {
  const selectedElements = useEditorState(
    Substores.metadata,
    (store) =>
      selectedViews.map((path) => {
        return {
          element: MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, path),
          parent: MetadataUtils.findElementByElementPath(
            store.editor.jsxMetadata,
            EP.parentPath(path),
          ),
        }
      }),
    'useDisableAlignment selectedElements',
  )

  return React.useMemo(() => {
    if (selectedViews.length === 0) {
      return true
    }

    return selectedElements.some(({ element, parent }) => {
      return isAlignmentGroupDisabled(element, parent, orientation, selectedViews)
    })
  }, [selectedViews, selectedElements, orientation])
}

export function isAlignmentGroupDisabled(
  element: ElementInstanceMetadata | null,
  parent: ElementInstanceMetadata | null,
  orientation: 'horizontal' | 'vertical',
  selection: ElementPath[],
): boolean {
  if (element == null || parent == null) {
    return true
  }

  // grid cells have all alignments available
  const isGridCell = MetadataUtils.isGridLayoutedContainer(parent)
  if (isGridCell) {
    return false
  }

  // flex children have alignment enabled on the opposite orientation to their parent's flex direction
  const isFlexChild = MetadataUtils.isFlexLayoutedContainer(parent)
  if (isFlexChild) {
    const flexDirection = MetadataUtils.getFlexDirection(parent)
    return flexDirection === 'column' || flexDirection === 'column-reverse'
      ? orientation === 'vertical'
      : orientation === 'horizontal'
  }

  // absolute elements have all alignments available, unless they are storyboard children or all
  // selected elements are storyboard children
  if (
    MetadataUtils.isPositionAbsolute(element) &&
    (!EP.isStoryboardChild(element.elementPath) ||
      (selection.length > 1 && selection.every((other) => EP.isStoryboardChild(other))))
  ) {
    return false
  }

  return true
}
