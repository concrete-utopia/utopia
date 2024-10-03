import React from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { useEditorState, Substores } from '../editor/store/store-hook'
import * as EP from '../../core/shared/element-path'

export function useDisableAlignment(
  selectedViews: ElementPath[],
  orientation: 'horizontal' | 'vertical',
) {
  const jsxMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'useDisableAlignment jsxMetadata',
  )

  return React.useMemo(() => {
    if (selectedViews.length === 0) {
      return true
    }

    return selectedViews.some((path, _, array) => {
      return isAlignmentGroupDisabled(jsxMetadata, path, orientation, array)
    })
  }, [selectedViews, jsxMetadata, orientation])
}

export function isAlignmentGroupDisabled(
  jsxMetadata: ElementInstanceMetadataMap,
  path: ElementPath,
  orientation: 'horizontal' | 'vertical',
  selection: ElementPath[],
): boolean {
  // grid cells have all alignments available
  const isGridCell = MetadataUtils.isGridCell(jsxMetadata, path)
  if (isGridCell) {
    return false
  }

  // flex children have alignment enabled on the opposite orientation to their parent's flex direction
  const isFlexChild = MetadataUtils.isFlexLayoutedContainer(
    MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(path)),
  )
  if (isFlexChild) {
    const flexDirection = MetadataUtils.getFlexDirection(
      MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(path)),
    )
    return flexDirection === 'column' || flexDirection === 'column-reverse'
      ? orientation === 'vertical'
      : orientation === 'horizontal'
  }

  // absolute elements have all alignments available, unless they are storyboard children or all
  // selected elements are storyboard children
  if (
    MetadataUtils.isPositionAbsolute(MetadataUtils.findElementByElementPath(jsxMetadata, path)) &&
    (!EP.isStoryboardChild(path) ||
      (selection.length > 1 && selection.every((other) => EP.isStoryboardChild(other))))
  ) {
    return false
  }

  return true
}
