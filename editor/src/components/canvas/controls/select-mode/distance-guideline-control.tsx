import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { flatMapArray, mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import { isInfinityRectangle, nullIfInfinity } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { fastForEach } from '../../../../core/shared/utils'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { isDragInteractionData } from '../../canvas-strategies/interaction-state'
import { getMultiselectBounds } from '../../canvas-strategies/strategies/shared-move-strategies-helpers'
import type { Guideline } from '../../guideline'
import { Guidelines } from '../../guideline'
import { DistanceGuideline } from '../distance-guideline'

function getDistanceGuidelines(
  highlightedView: ElementPath,
  componentMetadata: ElementInstanceMetadataMap,
): Array<Guideline> {
  const frame = MetadataUtils.getFrameInCanvasCoords(highlightedView, componentMetadata)
  if (frame == null || isInfinityRectangle(frame)) {
    return []
  } else {
    return Guidelines.guidelinesForFrame(frame, false)
  }
}

export const DistanceGuidelineControl = React.memo(() => {
  const isDisallowedInteractionActive = useEditorState(
    Substores.canvas,
    (store) => {
      return (
        store.editor.canvas.interactionSession != null &&
        isDragInteractionData(store.editor.canvas.interactionSession.interactionData)
      )
    },
    'DistanceGuidelineControl isInteractionActive',
  )
  const altKeyPressed = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.keysPressed['alt'],
    'DistanceGuidelineControl altKeyPressed',
  )
  const selectedElements = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'DistanceGuidelineControl selectedElements',
  )
  if (selectedElements.length > 0 && !isDisallowedInteractionActive && altKeyPressed) {
    return <DistanceGuidelineControlInner />
  } else {
    return null
  }
})

const DistanceGuidelineControlInner = React.memo(() => {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'DistanceGuidelineControl scale',
  )
  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.realCanvasOffset,
    'DistanceGuidelineControl canvasOffset',
  )
  const boundingBoxes = useEditorState(
    Substores.metadata,
    (store) => {
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
          return nullIfInfinity(
            MetadataUtils.getFrameInCanvasCoords(element, store.editor.jsxMetadata),
          )
        }, store.editor.selectedViews)
      }
    },
    'DistanceGuidelineControl boundingBoxes',
  )

  const distanceGuidelines = useEditorState(
    Substores.fullStore,
    (store) => {
      let guidelineInfo: Array<{ guidelines: Array<Guideline>; boundingBox: CanvasRectangle }> = []
      fastForEach(boundingBoxes, (boundingBox, index) => {
        if (store.editor.highlightedViews.length !== 0) {
          const guidelinesForHighlightedViews = flatMapArray((highlightedView) => {
            const highlightedViewIsSelected = store.editor.selectedViews.some((selectedElement) =>
              EP.pathsEqual(selectedElement, highlightedView),
            )
            if (highlightedViewIsSelected) {
              return []
            } else {
              if (EP.isFromSameInstanceAs(highlightedView, store.editor.selectedViews[index])) {
                return getDistanceGuidelines(highlightedView, store.editor.jsxMetadata)
              } else {
                return []
              }
            }
          }, store.editor.highlightedViews)
          guidelineInfo.push({
            guidelines: guidelinesForHighlightedViews,
            boundingBox: boundingBox,
          })
        } else {
          const parentPath = EP.parentPath(store.editor.selectedViews[0])
          if (parentPath != null) {
            if (EP.isFromSameInstanceAs(parentPath, store.editor.selectedViews[index])) {
              const guidelinesForParent = getDistanceGuidelines(
                parentPath,
                store.editor.jsxMetadata,
              )
              guidelineInfo.push({
                guidelines: guidelinesForParent,
                boundingBox: boundingBox,
              })
            }
          }
        }
      })
      return guidelineInfo
    },
    'DistanceGuidelineControl distanceGuidelines',
  )

  if (boundingBoxes.length !== 0) {
    return (
      <>
        {distanceGuidelines.map((guidelineInfo, index) => (
          <DistanceGuideline
            key={`${index}-distance-guidelines`}
            canvasOffset={canvasOffset}
            scale={scale}
            guidelines={guidelineInfo.guidelines}
            boundingBox={guidelineInfo.boundingBox}
          />
        ))}
      </>
    )
  } else {
    return null
  }
})
