import React from 'react'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { Modifier } from '../../../../utils/modifiers'
import type { EditorDispatch } from '../../../editor/action-types'
import { applyCommandsAction } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { invert, resizeToFitCommands } from '../../../inspector/inspector-common'
import { setPropHugStrategies } from '../../../inspector/inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from '../../../inspector/inspector-strategies/inspector-strategy'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import type { EdgePosition } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { ResizeControl } from '../resize-control'
import { useMaybeHighlightElement } from './select-mode-hooks'

export const AbsoluteResizeControlTestId = (targets: Array<ElementPath>): string =>
  `${targets.map(EP.toString).sort()}-absolute-resize-control`

interface AbsoluteResizeControlProps {
  targets: Array<ElementPath>
  pathsWereReplaced: boolean
}

export const AbsoluteResizeControl = controlForStrategyMemoized(
  ({ targets, pathsWereReplaced }: AbsoluteResizeControlProps) => {
    const dispatch = useDispatch()

    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)
    const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
    const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

    const controlRef = useBoundingBox(targets, (ref, safeGappedBoundingBox, realBoundingBox) => {
      if (isZeroSizedElement(realBoundingBox)) {
        ref.current.style.display = 'none'
      } else {
        ref.current.style.display = 'block'
        ref.current.style.left = safeGappedBoundingBox.x + 'px'
        ref.current.style.top = safeGappedBoundingBox.y + 'px'
        ref.current.style.width = safeGappedBoundingBox.width + 'px'
        ref.current.style.height = safeGappedBoundingBox.height + 'px'
      }
    })

    const scale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'AbsoluteResizeControl scale',
    )

    const onCornerMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>, position: EdgePosition) => {
        startResizeInteraction(event, dispatch, position, canvasOffsetRef.current, scale)
      },
      [dispatch, canvasOffsetRef, scale],
    )

    const onCornerDoubleClick = React.useCallback(() => {
      dispatch([
        applyCommandsAction(
          resizeToFitCommands(
            metadataRef.current,
            selectedElementsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
          ),
        ),
      ])
    }, [allElementPropsRef, dispatch, metadataRef, elementPathTreeRef, selectedElementsRef])

    const onEdgeMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>, position: EdgePosition) => {
        startResizeInteraction(event, dispatch, position, canvasOffsetRef.current, scale)
      },
      [dispatch, canvasOffsetRef, scale],
    )

    const onEdgeMouseMove = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        maybeClearHighlightsOnHoverEnd()
        event.stopPropagation()
      },
      [maybeClearHighlightsOnHoverEnd],
    )

    const onEdgeDoubleClick = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>, direction: 'horizontal' | 'vertical') => {
        executeFirstApplicableStrategy(
          dispatch,
          setPropHugStrategies(
            metadataRef.current,
            selectedElementsRef.current,
            elementPathTreeRef.current,
            invert(direction),
          ),
        )
      },
      [dispatch, metadataRef, elementPathTreeRef, selectedElementsRef],
    )

    return (
      <CanvasOffsetWrapper>
        <ResizeControl
          data-testid={AbsoluteResizeControlTestId(targets)}
          ref={controlRef}
          position='absolute'
          expandToFill={'do-not-expand'}
          targets={targets}
          onCornerMouseDown={onCornerMouseDown}
          onCornerDoubleClick={onCornerDoubleClick}
          onEdgeDoubleClick={onEdgeDoubleClick}
          onEdgeMouseMove={onEdgeMouseMove}
          onEdgeMouseDown={onEdgeMouseDown}
        />
      </CanvasOffsetWrapper>
    )
  },
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
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}
