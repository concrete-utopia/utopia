import React from 'react'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangle,
  isFiniteRectangle,
  zeroRectIfNullOrInfinity,
} from '../../../core/shared/math-utils'
import {
  Substores,
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getMetadata } from '../../editor/store/editor-state'
import { elementHasOnlyTextChildren } from '../canvas-utils'
import { isFixedHugFillModeApplied } from '../../inspector/inspector-common'
import * as EP from '../../../core/shared/element-path'

interface NotNullRefObject<T> {
  readonly current: T
}

export function useBoundingBox<T = HTMLDivElement>(
  selectedElements: ReadonlyArray<ElementPath>,
  onChangeCallback: (
    ref: NotNullRefObject<T>,
    safeGappedBoundingBox: CanvasRectangle,
    realBoundingBox: CanvasRectangle,
    scale: number,
  ) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)
  const boundingBoxCallback = React.useCallback(
    (
      safeGappedBoundingBox: CanvasRectangle | null,
      realBoundingBox: CanvasRectangle | null,
      scale: number,
    ) => {
      const maybeZeroSafeGappedBoundingBox = zeroRectIfNullOrInfinity(safeGappedBoundingBox)
      const maybeZeroRealBoundingBox = zeroRectIfNullOrInfinity(realBoundingBox)
      if (controlRef.current != null) {
        onChangeCallback(
          controlRef as NotNullRefObject<T>,
          maybeZeroSafeGappedBoundingBox,
          maybeZeroRealBoundingBox,
          scale,
        )
      }
    },
    [onChangeCallback],
  )

  useBoundingBoxFromMetadataRef(selectedElements, boundingBoxCallback)
  return controlRef
}

export const SmallElementSize = 20
export const SafeGapSmallElementSize = 12
export const RESIZE_CONTROL_SAFE_GAP = 4 // safe gap applied when the dimension of an element is smaller than SmallElementSize

function useBoundingBoxFromMetadataRef(
  selectedElements: ReadonlyArray<ElementPath>,
  boundingBoxCallback: (
    safeGappedBoundingRectangle: CanvasRectangle | null,
    realBoundingRectangle: CanvasRectangle | null,
    scale: number,
  ) => void,
): void {
  const metadataRef = useRefEditorState((store) => getMetadata(store.editor))
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  const isInteractionDraggingPastThreshold = useEditorState(
    Substores.canvas,
    (store) => {
      return (
        store.editor.canvas.interactionSession?.interactionData.type === 'DRAG' &&
        store.editor.canvas.interactionSession?.interactionData.drag != null
      )
    },
    'useBoundingBoxFromMetadataRef isInteractionDraggingPastThreshold',
  )

  const shouldApplySafeGap = React.useCallback(
    (dimension: number, scale: number): boolean => {
      if (selectedElements.length === 1) {
        // For text elements only show the safe gaps if they have been resized past the initial max-content dimensions
        const meta = MetadataUtils.findElementByElementPath(
          metadataRef.current,
          selectedElements[0],
        )
        if (
          meta != null &&
          elementHasOnlyTextChildren(meta) &&
          isFixedHugFillModeApplied(metadataRef.current, selectedElements[0], 'hug')
        ) {
          return false
        }
      }

      return !isInteractionDraggingPastThreshold && dimension <= SmallElementSize / scale
    },
    [isInteractionDraggingPastThreshold, metadataRef, selectedElements],
  )

  const boundingBoxCallbackRef = React.useRef(boundingBoxCallback)
  boundingBoxCallbackRef.current = boundingBoxCallback

  const innerCallback = React.useCallback(() => {
    let frames: Array<CanvasRectangle> = []
    fastForEach(selectedElements, (view) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(view, metadataRef.current)
      if (frame != null && isFiniteRectangle(frame)) {
        frames.push(frame)
      }
    })

    function getAdjustedBoundingBox(boundingBox: CanvasRectangle | null) {
      if (boundingBox == null) {
        return boundingBox
      }

      let adjustedBoundingBox = {
        x: boundingBox.x,
        y: boundingBox.y,
        width: boundingBox.width,
        height: boundingBox.height,
      }
      const scaledSafeGap = RESIZE_CONTROL_SAFE_GAP / scaleRef.current
      if (shouldApplySafeGap(boundingBox.width, scaleRef.current)) {
        adjustedBoundingBox.x -= scaledSafeGap
        adjustedBoundingBox.width += scaledSafeGap * 2
      }
      if (shouldApplySafeGap(boundingBox.height, scaleRef.current)) {
        adjustedBoundingBox.y -= scaledSafeGap
        adjustedBoundingBox.height += scaledSafeGap * 2
      }
      return canvasRectangle(adjustedBoundingBox)
    }
    const realBoundingBox = boundingRectangleArray(frames)
    const safeGappedBoundingBox = getAdjustedBoundingBox(realBoundingBox)

    boundingBoxCallbackRef.current(safeGappedBoundingBox, realBoundingBox, scaleRef.current)
  }, [selectedElements, metadataRef, scaleRef, shouldApplySafeGap])

  useSelectorWithCallback(
    Substores.metadata,
    (store) => getMetadata(store.editor),
    (newMetadata) => {
      innerCallback()
    },
    'useBoundingBoxFromMetadataRef metadata',
  )
  useSelectorWithCallback(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    (newScale) => {
      innerCallback()
    },
    'useBoundingBoxFromMetadataRef canvas.scale',
  )

  React.useEffect(innerCallback, [innerCallback, selectedElements])
}
