import * as React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { aperture, mapDropNulls } from '../../../../core/shared/array-utils'
import { point, windowPoint } from '../../../../core/shared/math-utils'
import { createInteractionViaMouse } from '../../../../interactions_proposal'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { windowToCanvasCoordinates } from '../../dom-lookup'

const FlexGapControls_ = () => {
  const canvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'FlexGapControls canvasOffset',
  )

  const targetedElement = useEditorState(
    (store) => store.editor.selectedViews[0],
    'FlexGapControls targetedElement',
  )

  const jsxMetadata = useEditorState(
    (store) => store.editor.jsxMetadata,
    'FlexGapControls jsxMetadata',
  )

  const dispatch = useEditorState((store) => store.dispatch, 'FlexGapControls dispatch')

  const canvasScale = useEditorState((store) => store.editor.canvas.scale, 'FlexGapControls scale')

  const roundedCanvasOffset = useEditorState(
    (store) => store.editor.canvas.roundedCanvasOffset,
    'FlexGapControls roundedCanvasOffset',
  )

  const startDragging = React.useCallback(
    (event: React.MouseEvent) => {
      function completeDrag() {
        window.removeEventListener('mouseup', completeDrag)
        dispatch([CanvasActions.clearInteractionState(true)], 'canvas')
      }
      window.addEventListener('mouseup', completeDrag)

      const startPoint = windowToCanvasCoordinates(
        canvasScale,
        roundedCanvasOffset,
        windowPoint(point(event.clientX, event.clientY)),
      ).canvasPositionRounded

      dispatch(
        [
          CanvasActions.createInteractionState(
            createInteractionViaMouse(startPoint, Modifier.modifiersForEvent(event), {
              type: 'FLEX_GAP_HANDLE',
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

  const flexGapRects = React.useMemo(() => {
    const targetParent = MetadataUtils.getParent(jsxMetadata, targetedElement)
    const isFlexLayouted = MetadataUtils.isFlexLayoutedContainer(targetParent)
    const siblings = MetadataUtils.getSiblings(jsxMetadata, targetedElement)
    const siblingPairs = aperture(2, siblings)
    if (isFlexLayouted) {
      return mapDropNulls(([first, second]) => {
        if (first?.globalFrame == null || second?.globalFrame == null) {
          return null
        } else {
          return {
            x: first.globalFrame.x + first.globalFrame.width,
            y: first.globalFrame.y + first.globalFrame.height / 2,
            width: second.globalFrame.x - first.globalFrame.x - first.globalFrame.width,
            height: 10,
          }
        }
      }, siblingPairs)
    } else {
      return []
    }
  }, [targetedElement, jsxMetadata])

  return (
    <>
      {flexGapRects.map((rect) => {
        return (
          <div
            key={`flex-gap-rect-${rect.x}-${rect.y}-${rect.width}-${rect.height}`}
            style={{
              position: 'absolute',
              left: canvasOffset.x + rect.x,
              top: canvasOffset.y + rect.y,
              width: rect.width,
              height: rect.height,
              backgroundColor: 'purple',
            }}
            onMouseDown={startDragging}
          />
        )
      })}
    </>
  )
}
FlexGapControls_.displayName = 'FlexGapControls'
export const FlexGapControls = React.memo(FlexGapControls_)
