import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import { boundingRectangleArray, CanvasRectangle } from '../../../core/shared/math-utils'
import { useRefEditorMetadata, useSelectorWithCallback } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getMetadata } from '../../editor/store/editor-state'

interface NotNullRefObject<T> {
  readonly current: T
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
  const metadataRef = useRefEditorMetadata()

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
    React.useCallback((store) => getMetadata(store.editor), []),
    React.useCallback(
      (newMetadata) => {
        innerCallback()
      },
      [innerCallback],
    ),
  )

  React.useEffect(innerCallback, [innerCallback, selectedElements])
}
