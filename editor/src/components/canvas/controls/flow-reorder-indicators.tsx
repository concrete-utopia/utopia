import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { arrayEquals } from '../../../core/shared/utils'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { getMultiselectBounds } from '../canvas-strategies/shared-absolute-move-strategy-helpers'
import { CanvasFrameAndTarget } from '../canvas-types'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

const useColorForDisplayType = (colorTheme: any) => {
  return useEditorState((store) => {
    if (store.editor.selectedViews.length > 0) {
      const metadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )
      return metadata?.specialSizeMeasurements.display === 'block'
        ? colorTheme.canvasDragOutlineBlock.value
        : colorTheme.canvasDragOutlineInline.value
    } else {
      return colorTheme.canvasSelectionPrimaryOutline.value
    }
  }, 'FlowReorderDragOutline color')
}

export const FlowReorderDragOutline = React.memo(() => {
  const scale = useEditorState((store) => store.editor.canvas.scale, 'FlowReorderDragOutline scale')
  const frame = useEditorState((store) => {
    return getMultiselectBounds(store.editor.jsxMetadata, store.editor.selectedViews)
  }, 'FlowReorderDragOutline frame')
  const dragVector = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      return store.editor.canvas.interactionSession.interactionData.drag
    } else {
      return null
    }
  }, 'FlowReorderDragOutline dragVector')

  const colorTheme = useColorTheme()
  const color = useColorForDisplayType(colorTheme)

  if (frame == null || dragVector == null) {
    return null
  } else {
    return (
      <CanvasOffsetWrapper>
        <div
          style={{
            position: 'absolute',
            top: frame.y + dragVector.y,
            left: frame.x + dragVector.x,
            width: frame.width,
            height: frame.height,
            boxSizing: 'border-box',
            boxShadow: `0px 0px 0px ${1 / scale}px  ${color}`,
            opacity: '50%',
          }}
        />
      </CanvasOffsetWrapper>
    )
  }
})

export const FlowReorderAreaIndicator = React.memo(() => {
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'DisplayTypeOutline canvas scale',
  )
  const colorTheme = useColorTheme()
  const outlineColor = useColorForDisplayType(colorTheme)

  const siblingFramesWithSameDisplayType: CanvasFrameAndTarget[] = useEditorState(
    (store) => {
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
    },
    'sibling frames',
    (oldResult: CanvasFrameAndTarget[], newResult: CanvasFrameAndTarget[]) => {
      return arrayEquals(oldResult, newResult, (l, r) => {
        return EP.pathsEqual(l.target, r.target) && shallowEqual(l.frame, r.frame)
      })
    },
  )

  return siblingFramesWithSameDisplayType.length === 0 ? null : (
    <CanvasOffsetWrapper>
      {siblingFramesWithSameDisplayType.map((frameAndTarget) => {
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
              backgroundImage: `linear-gradient(135deg, ${outlineColor} 2.5%, rgba(255,255,255,0) 2.5%, rgba(255,255,255,0) 50%, ${outlineColor} 50%, ${outlineColor} 52%, rgba(255,255,255,0) 52%, rgba(255,255,255,0) 100%)`,
              backgroundSize: `${20 / scale}px ${20 / scale}px`,
              pointerEvents: 'none',
            }}
          ></div>
        )
      })}
    </CanvasOffsetWrapper>
  )
})
