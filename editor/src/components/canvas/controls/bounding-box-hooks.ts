import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import {
  boundingRectangleArray,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
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

export function useDistanceGuidelineX<T = HTMLDivElement>(
  selectedElements: Array<ElementPath>,
  onChangeCallback: (
    ref: NotNullRefObject<T>,
    boundingBox: CanvasRectangle,
    distance: number,
  ) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)
  const boundingBoxCallback = React.useCallback(
    (boundingBox: CanvasRectangle | null, distance: number) => {
      if (boundingBox != null && controlRef.current != null) {
        onChangeCallback(controlRef as NotNullRefObject<T>, boundingBox, distance)
      }
    },
    [onChangeCallback],
  )

  const metadataRef = useRefEditorState((store) => getMetadata(store.editor))
  const highlightedViews = useEditorState(
    (store) => store.editor.highlightedViews,
    'guideline highlighted view',
  )

  const boundingBoxCallbackRef = React.useRef(boundingBoxCallback)
  boundingBoxCallbackRef.current = boundingBoxCallback

  const innerCallback = React.useCallback(() => {
    let frames: Array<CanvasRectangle> = []
    fastForEach(selectedElements, (view) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(view, metadataRef.current)
      if (frame != null) {
        frames.push(frame)
      }
    })

    const boundingBox = boundingRectangleArray(frames)
    if (boundingBox != null) {
      let distanceGuidelines: Array<Guideline> = []
      if (highlightedViews.length !== 0) {
        distanceGuidelines = flatMapArray((highlightedView) => {
          return getDistanceGuidelines(highlightedView, metadataRef.current)
        }, highlightedViews)
      } else if (selectedElements.length > 0) {
        const parentPath = EP.parentPath(selectedElements[0])
        if (parentPath != null) {
          distanceGuidelines = getDistanceGuidelines(parentPath, metadataRef.current)
        }
      }

      const xAxisGuidelines = distanceGuidelines.filter(
        (guideline) => guideline.type === 'XAxisGuideline',
      )
      const yAxisGuidelines = distanceGuidelines.filter(
        (guideline) => guideline.type === 'YAxisGuideline',
      )

      if (xAxisGuidelines.length > 0) {
        const distanceResults = xAxisGuidelines
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

        const frame = canvasRectangle({ x: topLeft.x - 1, y: topLeft.y, width: width, height: 0 })

        boundingBoxCallbackRef.current(frame, result.distance)
      }
    }
  }, [selectedElements, metadataRef, highlightedViews])

  useSelectorWithCallback(
    (store) => getMetadata(store.editor),
    (newMetadata) => {
      innerCallback()
    },
  )

  React.useEffect(innerCallback, [innerCallback, selectedElements])

  return controlRef
}

export function useDistanceGuidelineY<T = HTMLDivElement>(
  selectedElements: Array<ElementPath>,
  onChangeCallback: (
    ref: NotNullRefObject<T>,
    boundingBox: CanvasRectangle,
    distance: number,
  ) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)
  const boundingBoxCallback = React.useCallback(
    (boundingBox: CanvasRectangle | null, distance: number) => {
      if (boundingBox != null && controlRef.current != null) {
        onChangeCallback(controlRef as NotNullRefObject<T>, boundingBox, distance)
      }
    },
    [onChangeCallback],
  )

  const metadataRef = useRefEditorState((store) => getMetadata(store.editor))
  const highlightedViews = useEditorState(
    (store) => store.editor.highlightedViews,
    'guideline highlighted view',
  )

  const boundingBoxCallbackRef = React.useRef(boundingBoxCallback)
  boundingBoxCallbackRef.current = boundingBoxCallback

  const innerCallback = React.useCallback(() => {
    let frames: Array<CanvasRectangle> = []
    fastForEach(selectedElements, (view) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(view, metadataRef.current)
      if (frame != null) {
        frames.push(frame)
      }
    })

    const boundingBox = boundingRectangleArray(frames)
    if (boundingBox != null) {
      let distanceGuidelines: Array<Guideline> = []
      if (highlightedViews.length !== 0) {
        distanceGuidelines = flatMapArray((highlightedView) => {
          return getDistanceGuidelines(highlightedView, metadataRef.current)
        }, highlightedViews)
      } else if (selectedElements.length > 0) {
        const parentPath = EP.parentPath(selectedElements[0])
        if (parentPath != null) {
          distanceGuidelines = getDistanceGuidelines(parentPath, metadataRef.current)
        }
      }
      const yAxisGuidelines = distanceGuidelines.filter(
        (guideline) => guideline.type === 'YAxisGuideline',
      )

      if (yAxisGuidelines.length > 0) {
        const distanceResults = yAxisGuidelines
          .map((guideline) =>
            Guidelines.distanceFromFrameToGuideline(boundingBox, guideline, false),
          )
          .sort((a, b) => a.distance - b.distance)
        const result = distanceResults[0]
        const topLeft = {
          x: Math.min(result.from.x, result.to.x),
          y: Math.min(result.from.y, result.to.y),
        } as CanvasPoint
        const height = Math.abs(result.to.y - result.from.y)

        const frame = canvasRectangle({ x: topLeft.x, y: topLeft.y - 1, width: 0, height: height })
        boundingBoxCallbackRef.current(frame, result.distance)
      }
    }
  }, [selectedElements, metadataRef, highlightedViews])

  useSelectorWithCallback(
    (store) => getMetadata(store.editor),
    (newMetadata) => {
      innerCallback()
    },
  )

  React.useEffect(innerCallback, [innerCallback, selectedElements])

  return controlRef
}

export function useBoundingBox<T = HTMLDivElement>(
  selectedElements: Array<ElementPath>,
  onChangeCallback: (ref: NotNullRefObject<T>, boundingBox: CanvasRectangle) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)
  const boundingBoxCallback = React.useCallback(
    (boundingBox: CanvasRectangle | null) => {
      if (boundingBox != null && controlRef.current != null) {
        onChangeCallback(controlRef as NotNullRefObject<T>, boundingBox)
      }
    },
    [onChangeCallback],
  )

  useBoundingBoxFromMetadataRef(selectedElements, boundingBoxCallback)
  return controlRef
}

function useBoundingBoxFromMetadataRef(
  selectedElements: Array<ElementPath>,
  boundingBoxCallback: (boundingRectangle: CanvasRectangle | null) => void,
): void {
  const metadataRef = useRefEditorState((store) => getMetadata(store.editor))

  const boundingBoxCallbackRef = React.useRef(boundingBoxCallback)
  boundingBoxCallbackRef.current = boundingBoxCallback

  const innerCallback = React.useCallback(() => {
    let frames: Array<CanvasRectangle> = []
    fastForEach(selectedElements, (view) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(view, metadataRef.current)
      if (frame != null) {
        frames.push(frame)
      }
    })

    const boundingBox = boundingRectangleArray(frames)

    boundingBoxCallbackRef.current(boundingBox)
  }, [selectedElements, metadataRef])

  useSelectorWithCallback(
    (store) => getMetadata(store.editor),
    (newMetadata) => {
      innerCallback()
    },
  )

  React.useEffect(innerCallback, [innerCallback, selectedElements])
}
