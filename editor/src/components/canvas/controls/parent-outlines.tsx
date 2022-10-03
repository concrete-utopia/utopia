import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme } from '../../../uuiui'
import { isInsertMode } from '../../editor/editor-modes'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const ParentOutlines = React.memo(() => {
  const colorTheme = useColorTheme()
  const scale = useEditorState((store) => store.editor.canvas.scale, 'ParentOutlines canvas scale')
  const parentFrame = useEditorState((store) => {
    const parentHighlightPaths = store.editor.canvas.controls.parentHighlightPaths
    if (parentHighlightPaths != null && parentHighlightPaths.length === 1) {
      return MetadataUtils.getFrameInCanvasCoords(parentHighlightPaths[0], store.editor.jsxMetadata)
    }

    if (!isInsertMode(store.editor.mode)) {
      const targetParents = uniqBy(
        stripNulls(store.editor.selectedViews.map((view) => EP.parentPath(view))),
        EP.pathsEqual,
      )
      if (targetParents.length === 1 && !EP.isStoryboardPath(targetParents[0])) {
        return MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          store.editor.selectedViews[0],
        )?.specialSizeMeasurements.immediateParentBounds
      }
    }
    return null
  }, 'ParentOutlines frame')

  return parentFrame != null ? (
    <CanvasOffsetWrapper key={`parent-outline`}>
      <div
        style={{
          position: 'absolute',
          left: parentFrame.x,
          top: parentFrame.y,
          width: parentFrame.width,
          height: parentFrame.height,
          outlineStyle: 'dotted',
          outlineColor: colorTheme.primary.value,
          outlineWidth: 1 / scale,
          pointerEvents: 'none',
        }}
        data-testid='parent-outlines-control'
      />
    </CanvasOffsetWrapper>
  ) : null
})
