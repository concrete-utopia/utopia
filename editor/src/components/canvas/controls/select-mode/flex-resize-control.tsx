import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { aperture, mapDropNulls, safeIndex } from '../../../../core/shared/array-utils'
import {
  canvasRectangle,
  CanvasRectangle,
  offsetRect,
  point,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { createInteractionViaMouse } from '../../../../interactions_proposal'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { DirectionHorizontal } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'

const FlexResizeControls_ = () => {
  const canvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'FlexResizeControls canvasOffset',
  )

  const targetedElement = useEditorState(
    (store) => safeIndex(store.editor.selectedViews, 0) ?? null,
    'FlexResizeControls targetedElement',
  )

  const jsxMetadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'FlexResizeControls jsxMetadata',
  )

  const dispatch = useEditorState((store) => store.dispatch, 'FlexResizeControls dispatch')

  const canvasScale = useEditorState(
    (store) => store.editor.canvas.scale,
    'FlexResizeControls scale',
  )

  const roundedCanvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'FlexResizeControls roundedCanvasOffset',
  )

  const startDragging = React.useCallback(
    (event: React.MouseEvent) => {
      const startPoint = windowToCanvasCoordinates(
        canvasScale,
        roundedCanvasOffset,
        windowPoint(point(event.clientX, event.clientY)),
      ).canvasPositionRounded

      dispatch(
        [
          CanvasActions.createInteractionState(
            createInteractionViaMouse(startPoint, Modifier.modifiersForEvent(event), {
              type: 'RESIZE_HANDLE',
              edgePosition: DirectionHorizontal,
            }),
          ),
        ],
        'canvas',
      )
      event.preventDefault()
      event.stopPropagation()
    },
    [dispatch, canvasScale, roundedCanvasOffset],
  )

  const completeDrag = React.useCallback(
    (event: React.MouseEvent) => {
      // TODO Complete Interaction Session here
      dispatch([CanvasActions.clearInteractionState(true)], 'canvas')
    },
    [dispatch],
  )

  const elementMetadata = React.useMemo(() => {
    return MetadataUtils.findElementByElementPath(jsxMetadata, targetedElement)
  }, [jsxMetadata, targetedElement])

  const edgeControlFrame: CanvasRectangle | null = React.useMemo(() => {
    if (elementMetadata?.globalFrame == null) {
      return null
    } else {
      const elementFrame = offsetRect(elementMetadata.globalFrame, canvasOffset)
      const frameLeft = elementFrame.x + elementFrame.width - 20
      const frameTop = elementFrame.y + 40
      return canvasRectangle({
        x: frameLeft,
        y: frameTop,
        width: 40,
        height: 40,
      })
    }
  }, [elementMetadata, canvasOffset])

  if (targetedElement == null || edgeControlFrame == null) {
    return null
  } else {
    return (
      <>
        <div
          key={`flex-resize-rect-mouse-catcher`}
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            width: '100%',
            height: '100%',
          }}
          onMouseUp={completeDrag}
        />
        ,
        <div
          key={`flex-resize-rect-${edgeControlFrame.x}-${edgeControlFrame.y}-${edgeControlFrame.width}-${edgeControlFrame.height}`}
          style={{
            position: 'absolute',
            left: edgeControlFrame.x,
            top: edgeControlFrame.y,
            width: edgeControlFrame.width,
            height: edgeControlFrame.height,
            backgroundColor: 'purple',
          }}
          onMouseDown={startDragging}
        />
      </>
    )
  }
}
FlexResizeControls_.displayName = 'FlexResizeControls'
export const FlexResizeControls = React.memo(FlexResizeControls_)
