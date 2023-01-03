import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { stripNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { CanvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { ThemeObject } from '../../../uuiui/styles/theme/theme-helpers'
import { isInsertMode } from '../../editor/editor-modes'
import { useEditorState } from '../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

interface ImmediateParentOutlinesProps {
  targets: Array<ElementPath>
}

export const ImmediateParentOutlines = controlForStrategyMemoized(
  ({ targets }: ImmediateParentOutlinesProps) => {
    const colorTheme = useColorTheme()
    const scale = useEditorState('canvas')(
      (store) => store.editor.canvas.scale,
      'ImmediateParentOutlines canvas scale',
    )
    const parentFrame = useEditorState('fullOldStore')((store) => {
      const parentHighlightPaths = store.editor.canvas.controls.parentHighlightPaths
      if (parentHighlightPaths != null && parentHighlightPaths.length === 1) {
        return MetadataUtils.getFrameInCanvasCoords(
          parentHighlightPaths[0],
          store.editor.jsxMetadata,
        )
      }

      if (!isInsertMode(store.editor.mode)) {
        const targetParents = uniqBy(
          stripNulls(targets.map((view) => EP.parentPath(view))),
          EP.pathsEqual,
        )
        if (targetParents.length === 1 && !EP.isStoryboardPath(targetParents[0])) {
          return MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, targets[0])
            ?.specialSizeMeasurements.immediateParentBounds
        }
      }
      return null
    }, 'ImmediateParentOutlines frame')

    return parentFrame == null ? null : drawOutlines(parentFrame, scale, colorTheme)
  },
)

interface ParentOutlinesProps {
  targetParent: ElementPath
}
export const ParentOutlines = controlForStrategyMemoized(
  ({ targetParent }: ParentOutlinesProps) => {
    const colorTheme = useColorTheme()
    const scale = useEditorState('canvas')(
      (store) => store.editor.canvas.scale,
      'ParentOutlines canvas scale',
    )

    const parentFrame = useEditorState('metadata')((store) => {
      if (!EP.isStoryboardPath(targetParent)) {
        return MetadataUtils.getFrameInCanvasCoords(targetParent, store.editor.jsxMetadata)
      } else {
        return null
      }
    }, 'ImmediateParentOutlines frame')

    return parentFrame == null ? null : drawOutlines(parentFrame, scale, colorTheme)
  },
)

function drawOutlines(parentFrame: CanvasRectangle, scale: number, colorTheme: ThemeObject) {
  return (
    <CanvasOffsetWrapper key={`parent-outlines`}>
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
  )
}
