import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls, stripNulls, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const ParentOutlines = React.memo(() => {
  const colorTheme = useColorTheme()
  const scale = useEditorState((store) => store.editor.canvas.scale, 'ParentOutlines canvas scale')
  const parentFrames = useEditorState((store) => {
    const targetParents = uniqBy(
      stripNulls(store.editor.selectedViews.map((view) => EP.parentPath(view))),
      EP.pathsEqual,
    )
    return mapDropNulls((parentPath) => {
      const parentElement = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        parentPath,
      )
      if (
        MetadataUtils.isFlexLayoutedContainer(parentElement) ||
        MetadataUtils.isGridLayoutedContainer(parentElement)
      ) {
        return MetadataUtils.getFrameInCanvasCoords(parentPath, store.editor.jsxMetadata)
      } else {
        return null
      }
    }, targetParents)
  }, 'ParentOutlines frames')

  return (
    <>
      {parentFrames.map((frame, i) => {
        return (
          <CanvasOffsetWrapper key={`parent-outline-${i}`}>
            <div
              style={{
                position: 'absolute',
                left: frame.x,
                top: frame.y,
                width: frame.width,
                height: frame.height,
                outlineStyle: 'dotted',
                outlineColor: colorTheme.primary.value,
                outlineWidth: 1 / scale,
              }}
            />
          </CanvasOffsetWrapper>
        )
      })}
    </>
  )
})
