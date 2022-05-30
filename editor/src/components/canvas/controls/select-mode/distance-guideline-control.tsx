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
  const selectedElements = useEditorState(
    (store) => store.editor.selectedViews,
    'DistanceGuidelineControl selectedElements',
  )
  const isInteractionActive = useEditorState(
    (store) => store.editor.canvas.interactionSession != null,
    'DistanceGuidelineControl isInteractionActive',
  )

  const altKeyPressed = useEditorState(
    (store) => store.editor.keysPressed['alt'],
    'DistanceGuidelineControl altKeyPressed',
  )
  const highlightedViews = useEditorState(
    (store) => store.editor.highlightedViews,
    'DistanceGuidelineControl highlightedViews',
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

  if (selectedElements.length > 0 && !isInteractionActive && altKeyPressed) {
    let boundingBoxes: CanvasRectangle[] = []
    if (EP.areAllElementsInSameInstance(selectedElements)) {
      const multiSelectBounds = getMultiselectBounds(jsxMetadata, selectedElements)
      if (multiSelectBounds != null) {
        boundingBoxes = [multiSelectBounds]
      }
    } else {
      boundingBoxes = mapDropNulls((element) => {
        return MetadataUtils.getFrameInCanvasCoords(element, jsxMetadata)
      }, selectedElements)
    }

    if (boundingBoxes.length === 0) {
      return null
    }

    let guideLineElements: Array<JSX.Element> = []
    fastForEach(boundingBoxes, (boundingBox, index) => {
      let distanceGuidelines: Array<Guideline> = []
      if (highlightedViews.length !== 0) {
        distanceGuidelines = flatMapArray((highlightedView) => {
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
      } else {
        const parentPath = EP.parentPath(selectedElements[0])
        if (parentPath != null) {
          if (EP.isFromSameInstanceAs(parentPath, selectedElements[index])) {
            distanceGuidelines = getDistanceGuidelines(parentPath, jsxMetadata)
          }
        }
      }
      guideLineElements.push(
        <DistanceGuideline
          key={`${EP.toComponentId(selectedElements[index])}-distance-guidelines`}
          canvasOffset={canvasOffset}
          scale={scale}
          guidelines={distanceGuidelines}
          selectedViews={selectedElements}
          highlightedViews={highlightedViews}
          boundingBox={boundingBox}
        />,
      )
    })

    return <>{guideLineElements}</>
  } else {
    return null
  }
})
