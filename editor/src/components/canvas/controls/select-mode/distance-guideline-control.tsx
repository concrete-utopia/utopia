import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { flatMapArray, mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { CanvasRectangle } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { fastForEach } from '../../../../core/shared/utils'
import { useEditorState } from '../../../editor/store/store-hook'
import { getMultiselectBounds } from '../../canvas-strategies/shared-absolute-move-strategy-helpers'
import { Guideline, Guidelines } from '../../guideline'
import { DistanceGuideline } from '../distance-guideline'

function getDistanceGuidelines(
  highlightedView: ElementPath,
  componentMetadata: ElementInstanceMetadataMap,
): Array<Guideline> {
  const frame = MetadataUtils.getFrameInCanvasCoords(highlightedView, componentMetadata)
  if (frame == null) {
    return []
  } else {
    return Guidelines.guidelinesForFrame(frame, false)
  }
}

export const DistanceGuidelineControl = React.memo(() => {
  const isInteractionActive = useEditorState(
    (store) => store.editor.canvas.interactionSession != null,
    'DistanceGuidelineControl isInteractionActive',
  )
  const altKeyPressed = useEditorState(
    (store) => store.editor.keysPressed['alt'],
    'DistanceGuidelineControl altKeyPressed',
  )
  const selectedElements = useEditorState(
    (store) => store.editor.selectedViews,
    'DistanceGuidelineControl selectedElements',
  )
  if (selectedElements.length > 0 && !isInteractionActive && altKeyPressed) {
    return <DistanceGuidelineControlInner />
  } else {
    return null
  }
})

const DistanceGuidelineControlInner = React.memo(() => {
  const highlightedViews = useEditorState(
    (store) => store.editor.highlightedViews,
    'DistanceGuidelineControl highlightedViews',
  )
  const selectedElements = useEditorState(
    (store) => store.editor.selectedViews,
    'DistanceGuidelineControl selectedElements',
  )

  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'DistanceGuidelineControl scale',
  )
  const canvasOffset = useEditorState(
    (store) => store.editor.canvas.realCanvasOffset,
    'DistanceGuidelineControl canvasOffset',
  )
  const jsxMetadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'DistanceGuidelineControl jsxMetadata',
  )
  const boundingBoxes = useEditorState((store) => {
    if (EP.areAllElementsInSameInstance(store.editor.selectedViews)) {
      const multiSelectBounds = getMultiselectBounds(
        store.editor.jsxMetadata,
        store.editor.selectedViews,
      )
      if (multiSelectBounds != null) {
        return [multiSelectBounds]
      } else {
        return []
      }
    } else {
      return mapDropNulls((element) => {
        return MetadataUtils.getFrameInCanvasCoords(element, store.editor.jsxMetadata)
      }, store.editor.selectedViews)
    }
  }, 'DistanceGuidelineControl boundingBoxes')

  const distanceGuidelines = useEditorState((store) => {
    let guidelineInfo: Array<{ guidelines: Array<Guideline>; boundingBox: CanvasRectangle }> = []
    fastForEach(boundingBoxes, (boundingBox, index) => {
      if (highlightedViews.length !== 0) {
        const guidelinesForHighlightedViews = flatMapArray((highlightedView) => {
          const highlightedViewIsSelected = selectedElements.some((selectedElement) =>
            EP.pathsEqual(selectedElement, highlightedView),
          )
          if (highlightedViewIsSelected) {
            return []
          } else {
            if (EP.isFromSameInstanceAs(highlightedView, selectedElements[index])) {
              return getDistanceGuidelines(highlightedView, jsxMetadata)
            } else {
              return []
            }
          }
        }, highlightedViews)
        guidelineInfo.push({
          guidelines: guidelinesForHighlightedViews,
          boundingBox: boundingBox,
        })
      } else {
        const parentPath = EP.parentPath(selectedElements[0])
        if (parentPath != null) {
          if (EP.isFromSameInstanceAs(parentPath, selectedElements[index])) {
            const guidelinesForParent = getDistanceGuidelines(parentPath, jsxMetadata)
            guidelineInfo.push({
              guidelines: guidelinesForParent,
              boundingBox: boundingBox,
            })
          }
        }
      }
    })
    return guidelineInfo
  }, 'DistanceGuidelineControl distanceGuidelines')

  if (boundingBoxes.length !== 0) {
    return (
      <>
        {distanceGuidelines.map((guidelineInfo, index) => (
          <DistanceGuideline
            key={`${EP.toComponentId(selectedElements[index])}-distance-guidelines`}
            canvasOffset={canvasOffset}
            scale={scale}
            guidelines={guidelineInfo.guidelines}
            selectedViews={selectedElements}
            highlightedViews={highlightedViews}
            boundingBox={guidelineInfo.boundingBox}
          />
        ))}
      </>
    )
  } else {
    return null
  }
})
