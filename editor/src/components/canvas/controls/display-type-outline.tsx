import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasFrameAndTarget } from '../canvas-types'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const DisplayTypeOutline = React.memo(() => {
  const colorTheme = useColorTheme()
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'DisplayTypeOutline canvas scale',
  )

  const siblingFramesWithSameDisplayType: CanvasFrameAndTarget[] = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const path = store.editor.selectedViews[0]
      const targetDisplayType = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        path,
      )?.specialSizeMeasurements.display
      const siblings = MetadataUtils.getSiblings(store.editor.jsxMetadata, path)
      const frames = mapDropNulls((sibling) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(
          sibling.elementPath,
          store.editor.jsxMetadata,
        )
        if (sibling.specialSizeMeasurements.display === targetDisplayType && frame != null) {
          return {
            frame: frame,
            target: sibling.elementPath,
          }
        } else {
          return null
        }
      }, siblings)
      return frames
    } else {
      return []
    }
  }, 'sibling frames')

  const lineColor = colorTheme.darkPrimary.value
  const lineWidth = 2 / scale

  return siblingFramesWithSameDisplayType.length === 0 ? null : (
    <CanvasOffsetWrapper>
      {siblingFramesWithSameDisplayType.map((frameAndTarget, i) => {
        return (
          <div
            key={EP.toString(frameAndTarget.target)}
            style={{
              position: 'absolute',
              boxSizing: 'border-box',
              left: frameAndTarget.frame.x,
              top: frameAndTarget.frame.y,
              width: frameAndTarget.frame.width,
              height: frameAndTarget.frame.height,
              pointerEvents: 'none',
              outlineStyle: 'dashed',
              outlineColor: lineColor,
              outlineWidth: lineWidth,
              borderRadius: 2,
            }}
          ></div>
        )
      })}
    </CanvasOffsetWrapper>
  )
})
