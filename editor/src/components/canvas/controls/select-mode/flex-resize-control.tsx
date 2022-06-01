import React from 'react'
import { EditorStorePatched } from '../../../../components/editor/store/editor-state'
import { Modifier } from '../../../../utils/modifiers'
import { LayoutTargetableProp } from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasPoint,
  CanvasVector,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { setResizeOptionsTargetOptions } from '../../../editor/actions/action-creators'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import {
  CSSCursor,
  DirectionHorizontal,
  DirectionVertical,
  EdgePosition,
  resizeDragState,
  updateResizeDragState,
} from '../../canvas-types'
import { getOriginalFrames } from '../../canvas-utils'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

const selectedElementsSelector = (store: EditorStorePatched) => store.editor.selectedViews
export const FlexResizeControl = React.memo(() => {
  const selectedElements = useEditorState(
    selectedElementsSelector,
    'FlexResizeControl selectedElements',
  )

  const controlRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.x + 'px'
    ref.current.style.top = boundingBox.y + 'px'
    ref.current.style.width = boundingBox.width + 'px'
    ref.current.style.height = boundingBox.height + 'px'
  })

  const rightRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width + 'px'
    ref.current.style.top = boundingBox.height / 2 + 'px'
  })

  const bottomRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
    ref.current.style.left = boundingBox.width / 2 + 'px'
    ref.current.style.top = boundingBox.height + 'px'
  })

  return (
    <CanvasOffsetWrapper>
      <div
        ref={controlRef}
        style={{
          position: 'absolute',
        }}
      >
        <ResizeEdge
          ref={rightRef}
          position={{ x: 1, y: 0.5 }}
          cursor={CSSCursor.ResizeEW}
          direction='vertical'
        />
        <ResizeEdge
          ref={bottomRef}
          position={{ x: 0.5, y: 1 }}
          cursor={CSSCursor.ResizeNS}
          direction='horizontal'
        />
      </div>
    </CanvasOffsetWrapper>
  )
})

interface ResizeEdgeProps {
  cursor: CSSCursor
  position: EdgePosition
  direction: 'horizontal' | 'vertical'
}

const ResizeEdgeMouseAreaSize = 12
const ResizeEdgeMouseAreaOffset = ResizeEdgeMouseAreaSize / 2
const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
    const LineSVGComponent =
      props.position.y === 0.5 ? DimensionableControlVertical : DimensionableControlHorizontal
    const dispatch = useEditorState((store) => store.dispatch, 'ResizeEdge dispatch')
    const scale = useEditorState((store) => store.editor.canvas.scale, 'ResizeEdge scale')
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

    const onEdgeMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        startResizeInteraction(event, dispatch, props.position, canvasOffsetRef.current, scale)
      },
      [dispatch, props.position, canvasOffsetRef, scale],
    )

    return (
      <div ref={ref} style={{ position: 'absolute' }} onMouseDown={onEdgeMouseDown}>
        <LineSVGComponent />
        <div
          style={{
            position: 'absolute',
            pointerEvents: 'initial',
            width: ResizeEdgeMouseAreaSize / scale,
            height: ResizeEdgeMouseAreaSize / scale,
            top: -ResizeEdgeMouseAreaOffset / scale,
            left: -ResizeEdgeMouseAreaOffset / scale,
            backgroundColor: 'transparent',
            cursor: props.cursor,
          }}
        />
      </div>
    )
  }),
)

function startResizeInteraction(
  event: React.MouseEvent<HTMLDivElement>,
  dispatch: EditorDispatch,
  position: EdgePosition,
  canvasOffset: CanvasVector,
  scale: number,
) {
  event.stopPropagation()
  if (event.buttons === 1 && event.button !== 2) {
    const canvasPositions = windowToCanvasCoordinates(
      scale,
      canvasOffset,
      windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
    )
    dispatch([
      CanvasActions.createInteractionSession(
        createInteractionViaMouse(
          canvasPositions.canvasPositionRaw,
          Modifier.modifiersForEvent(event),
          {
            type: 'RESIZE_HANDLE',
            edgePosition: position,
          },
        ),
      ),
    ])
  }
}

const ControlSideShort = 3
const ControlSideLong = 15
const DimensionableControlVertical = React.memo(() => {
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'DimensionableControlVertical scale',
  )
  const colorTheme = useColorTheme()
  const controlLength = ControlSideLong
  const controlWidth = ControlSideShort
  const controlOffset = controlLength / 2

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'relative',
        backgroundColor: colorTheme.primary.shade(10).value,
        borderRadius: 5 / scale,
        boxShadow: `0px 0px 0px ${0.3 / scale}px ${colorTheme.primary.value}, 0px ${1 / scale}px ${
          3 / scale
        }px rgba(140,140,140,.9)`,
        height: controlLength / scale,
        width: controlWidth / scale,
        left: -1 / scale,
        top: -controlOffset / scale,
      }}
    />
  )
})

const DimensionableControlHorizontal = React.memo(() => {
  const scale = useEditorState(
    (store) => store.editor.canvas.scale,
    'DimensionableControlHorizontal scale',
  )
  const colorTheme = useColorTheme()
  const controlLength = ControlSideShort
  const controlWidth = ControlSideLong
  const controlOffset = controlWidth / 2

  return (
    <div
      className='label-dimensionableControlVertical'
      style={{
        position: 'relative',
        backgroundColor: colorTheme.primary.shade(10).value,
        borderRadius: 5 / scale,
        boxShadow: `0px 0px 0px ${0.3 / scale}px ${colorTheme.primary.value}, 0px ${1 / scale}px ${
          3 / scale
        }px rgba(140,140,140,.9)`,
        height: controlLength / scale,
        width: controlWidth / scale,
        left: -controlOffset / scale,
        top: -1 / scale,
      }}
    />
  )
})
