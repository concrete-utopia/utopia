import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { isInsertMode } from '../../editor/editor-modes'
import { useEditorState } from '../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { CenteredCrossSVG } from './outline-control'

export const ParentBounds = controlForStrategyMemoized(() => {
  const scale = useEditorState((store) => store.editor.canvas.scale, 'ParentBounds canvas scale')
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
      <div style={{ pointerEvents: 'none' }} data-testid='parent-bounds-control'>
        <CenteredCrossSVG
          id='parent-cross-top-left'
          centerX={parentFrame.x}
          centerY={parentFrame.y}
          scale={scale}
        />
        <CenteredCrossSVG
          id='parent-cross-top-right'
          centerX={parentFrame.x + parentFrame.width}
          centerY={parentFrame.y}
          scale={scale}
        />
        <CenteredCrossSVG
          id='parent-cross-bottom-right'
          centerX={parentFrame.x + parentFrame.width}
          centerY={parentFrame.y + parentFrame.height}
          scale={scale}
        />
        <CenteredCrossSVG
          id='parent-cross-bottom-left'
          centerX={parentFrame.x}
          centerY={parentFrame.y + parentFrame.height}
          scale={scale}
        />
      </div>
    </CanvasOffsetWrapper>
  ) : null
})
