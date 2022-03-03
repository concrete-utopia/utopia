import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import {
  boundingRectangleArray,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import {
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getMetadata } from '../../editor/store/editor-state'
import { Guideline, Guidelines } from '../guideline'
import * as EP from '../../../core/shared/element-path'
import { flatMapArray } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'

interface NotNullRefObject<T> {
  readonly current: T
}

function getDistanceGuidelines(
  target: ElementPath,
  componentMetadata: ElementInstanceMetadataMap,
): Array<Guideline> {
  const frame = MetadataUtils.getFrameInCanvasCoords(target, componentMetadata)
  if (frame == null) {
    return []
  } else {
    return Guidelines.guidelinesForFrame(frame, false)
  }
}

function boundingBoxFromSelection(
  selectedElements: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): CanvasRectangle | null {
  let frames: Array<CanvasRectangle> = []
  fastForEach(selectedElements, (view) => {
    const frame = MetadataUtils.getFrameInCanvasCoords(view, metadata)
    if (frame != null) {
      frames.push(frame)
    }
  })

  return boundingRectangleArray(frames)
}

export function useClosestDistanceGuideline<T = HTMLDivElement>(
  selectedElements: Array<ElementPath>,
  type: Guideline['type'],
  onChangeCallback: (
    ref: NotNullRefObject<T>,
    boundingBox: CanvasRectangle | null,
    distance: number | null,
  ) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)
  const guidelineCallback = React.useCallback(
    (guidelineFrame: CanvasRectangle | null, distance: number | null) => {
      if (controlRef.current != null) {
        onChangeCallback(controlRef as NotNullRefObject<T>, guidelineFrame, distance)
      }
    },
    [onChangeCallback],
  )

  const metadataRef = useRefEditorState((store) => getMetadata(store.editor))
  const highlightedViews = useEditorState(
    (store) => store.editor.highlightedViews,
    'guideline highlighted view',
  )

  const guidelineCallbackRef = React.useRef(guidelineCallback)
  guidelineCallbackRef.current = guidelineCallback

  const innerCallback = React.useCallback(() => {
    const boundingBox = boundingBoxFromSelection(selectedElements, metadataRef.current)
    if (boundingBox != null) {
      let distanceGuidelines: Array<Guideline> = []
      if (highlightedViews.length !== 0) {
        distanceGuidelines = flatMapArray((highlightedView) => {
          if (
            selectedElements.every((target) => EP.isFromSameInstanceAs(highlightedView, target))
          ) {
            return getDistanceGuidelines(highlightedView, metadataRef.current)
          } else {
            return []
          }
        }, highlightedViews)
      } else if (selectedElements.length > 0) {
        const parentPath = EP.parentPath(selectedElements[0])
        if (parentPath != null) {
          distanceGuidelines = getDistanceGuidelines(parentPath, metadataRef.current)
        }
      }

      const selectedAxisGuidelines = distanceGuidelines.filter(
        (guideline) => guideline.type === type,
      )

      if (selectedAxisGuidelines.length > 0) {
        const distanceResults = selectedAxisGuidelines
          .map((guideline) =>
            Guidelines.distanceFromFrameToGuideline(boundingBox, guideline, false),
          )
          .sort((a, b) => a.distance - b.distance)
        const result = distanceResults[0]
        const topLeft = {
          x: Math.min(result.from.x, result.to.x),
          y: Math.min(result.from.y, result.to.y),
        } as CanvasPoint
        const width = Math.abs(result.to.x - result.from.x)
        const height = Math.abs(result.to.y - result.from.y)

        const frame = canvasRectangle({ x: topLeft.x, y: topLeft.y, width: width, height: height })
        guidelineCallbackRef.current(frame, result.distance)
      } else {
        guidelineCallbackRef.current(null, null)
      }
    }
  }, [selectedElements, metadataRef, highlightedViews, type])

  useSelectorWithCallback(
    (store) => getMetadata(store.editor),
    (newMetadata) => {
      innerCallback()
    },
  )

  React.useEffect(innerCallback, [innerCallback, selectedElements])

  return controlRef
}
