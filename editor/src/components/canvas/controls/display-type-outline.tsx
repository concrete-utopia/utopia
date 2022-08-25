import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasFrameAndTarget } from '../canvas-types'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
export const DisplayTypeOutline = () => {
  const colorTheme = useColorTheme()
  const outlineColor = colorTheme.primary.value

  const siblingFramesWithSameDisplayType: CanvasFrameAndTarget[] = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const path = store.editor.selectedViews[0]
      const targetDisplayType = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        path,
      )?.specialSizeMeasurements.display
      const siblings = MetadataUtils.getSiblings(store.editor.jsxMetadata, path).filter(
        (element) => !EP.pathsEqual(element.elementPath, path),
      )
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
              backgroundImage: `linear-gradient(135deg, ${outlineColor} 2.5%, rgba(255,255,255,0) 2.5%, rgba(255,255,255,0) 50%, ${outlineColor} 50%, ${outlineColor} 52%, rgba(255,255,255,0) 52%, rgba(255,255,255,0) 100%)`,
              backgroundSize: `20px 20px`,
            }}
          ></div>
        )
      })}
    </CanvasOffsetWrapper>
  )
}
