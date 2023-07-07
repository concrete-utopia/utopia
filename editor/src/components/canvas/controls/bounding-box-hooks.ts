import React from 'react'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  isFiniteRectangle,
  zeroRectIfNullOrInfinity,
} from '../../../core/shared/math-utils'
import {
  Substores,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getMetadata } from '../../editor/store/editor-state'

interface NotNullRefObject<T> {
  readonly current: T
}

export function useBoundingBox<T = HTMLDivElement>(
  selectedElements: Array<ElementPath>,
  onChangeCallback: (ref: NotNullRefObject<T>, boundingBox: CanvasRectangle, scale: number) => void,
): React.RefObject<T> {
  const controlRef = React.useRef<T>(null)
  const boundingBoxCallback = React.useCallback(
    (boundingBox: CanvasRectangle | null, scale: number) => {
      const maybeZeroBoundingBox = zeroRectIfNullOrInfinity(boundingBox)
      if (controlRef.current != null) {
        onChangeCallback(controlRef as NotNullRefObject<T>, maybeZeroBoundingBox, scale)
      }
    },
    [onChangeCallback],
  )

  useBoundingBoxFromMetadataRef(selectedElements, boundingBoxCallback)
  return controlRef
}

function useBoundingBoxFromMetadataRef(
  selectedElements: Array<ElementPath>,
  boundingBoxCallback: (boundingRectangle: CanvasRectangle | null, scale: number) => void,
): void {
  const metadataRef = useRefEditorState((store) => getMetadata(store.editor))
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

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

    const boundingBox = boundingRectangleArray(frames)

    boundingBoxCallbackRef.current(boundingBox, scaleRef.current)
  }, [selectedElements, metadataRef, scaleRef])

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
