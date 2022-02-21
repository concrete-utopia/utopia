import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import { boundingRectangleArray, CanvasRectangle } from '../../../core/shared/math-utils'
import { useRefEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getMetadata } from '../../editor/store/editor-state'

export function useBoundingBox(
  selectedElements: Array<ElementPath>,
  onChangeCallback: (ref: any, boundingBox: any) => void,
): React.RefObject<HTMLDivElement> {
  const controlRef = React.useRef<HTMLDivElement>(null)
  const boundingBoxCallback = React.useCallback(
    (boundingBox: CanvasRectangle | null) => {
      if (boundingBox != null) {
        onChangeCallback(controlRef, boundingBox)
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
    true,
  )

  React.useEffect(innerCallback, [innerCallback, selectedElements])
}
