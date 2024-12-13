import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import { windowPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import type { EditorDispatch } from '../../../editor/action-types'
import { applyCommandsAction } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import {
  invert,
  isFillOrStretchModeAppliedOnAnySide,
  isFillOrStretchModeAppliedOnSpecificSide,
  resizeToFitCommands,
} from '../../../inspector/inspector-common'
import { setPropHugStrategies } from '../../../inspector/inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from '../../../inspector/inspector-strategies/inspector-strategy'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import type { EdgePosition } from '../../canvas-types'
import { CSSCursor } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useMaybeHighlightElement } from './select-mode-hooks'
import { isEdgePositionEqualTo } from '../../canvas-utils'
import { useResizeEdges } from './use-resize-edges'

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

    const topLeftRef = useBoundingBox(targets, NO_OP)
    const topRightRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.left = boundingBox.width + 'px'
    })
    const bottomLeftRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.top = boundingBox.height + 'px'
    })
    const bottomRightRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.left = boundingBox.width + 'px'
      ref.current.style.top = boundingBox.height + 'px'
    })

    const scale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'AbsoluteResizeControl scale',
    )
    const onEdgeMouseDown = React.useCallback(
      (position: EdgePosition) => (event: React.MouseEvent<HTMLDivElement>) => {
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
      (direction: 'horizontal' | 'vertical') => () => {
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

    const resizeEdges = useResizeEdges(targets, {
      onEdgeMouseDown,
      onEdgeMouseMove,
      onEdgeDoubleClick,
    })

    const canResize = useEditorState(
      Substores.metadata,
      (store) => {
        const metadata = store.editor.jsxMetadata

        let horizontally = true
        let vertically = true
        let diagonally = true

        for (const element of selectedElementsRef.current) {
          if (MetadataUtils.isGridItem(metadata, element)) {
            if (isFillOrStretchModeAppliedOnAnySide(metadata, element)) {
              diagonally = false
            }
            if (isFillOrStretchModeAppliedOnSpecificSide(metadata, element, 'horizontal')) {
              horizontally = false
            }
            if (isFillOrStretchModeAppliedOnSpecificSide(metadata, element, 'vertical')) {
              vertically = false
            }
          }
        }

        return {
          horizontally: horizontally,
          vertically: vertically,
          diagonally: diagonally,
        }
      },
      'AbsoluteResizeControl canResize',
    )

    return (
      <CanvasOffsetWrapper>
        <div
          data-testid={AbsoluteResizeControlTestId(targets)}
          ref={controlRef}
          style={{
            position: 'absolute',
            pointerEvents: 'none',
          }}
        >
          {when(
            canResize.vertically,
            <React.Fragment>
              {resizeEdges.top}
              {resizeEdges.bottom}
            </React.Fragment>,
          )}
          {when(
            canResize.horizontally,
            <React.Fragment>
              {resizeEdges.left}
              {resizeEdges.right}
            </React.Fragment>,
          )}
          {when(
            canResize.diagonally,
            <React.Fragment>
              <ResizePoint
                ref={topLeftRef}
                position={{ x: 0, y: 0 }}
                cursor={CSSCursor.ResizeNWSE}
              />
              <ResizePoint
                ref={topRightRef}
                position={{ x: 1, y: 0 }}
                cursor={CSSCursor.ResizeNESW}
              />
              <ResizePoint
                ref={bottomLeftRef}
                position={{ x: 0, y: 1 }}
                cursor={CSSCursor.ResizeNESW}
              />
              <ResizePoint
                ref={bottomRightRef}
                position={{ x: 1, y: 1 }}
                cursor={CSSCursor.ResizeNWSE}
              />
            </React.Fragment>,
          )}
        </div>
      </CanvasOffsetWrapper>
    )
  },
)

interface ResizePointProps {
  cursor: CSSCursor
  position: EdgePosition
}

export const ResizePointTestId = (position: EdgePosition): string =>
  `resize-control-${position.x}-${position.y}`
const ResizePointMouseAreaSize = 12
const ResizePointMouseAreaOffset = ResizePointMouseAreaSize / 2
const ResizePointSize = 6
const ResizePointOffset = ResizePointSize / 2
const ResizePoint = React.memo(
  React.forwardRef<HTMLDivElement, ResizePointProps>((props, ref) => {
    const colorTheme = useColorTheme()
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'ResizeEdge scale',
    )
    const dispatch = useDispatch()
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)

    const onPointMouseDown = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        startResizeInteraction(event, dispatch, props.position, canvasOffsetRef.current, scale)
      },
      [dispatch, props.position, canvasOffsetRef, scale],
    )

    const onMouseMove = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        maybeClearHighlightsOnHoverEnd()
        event.stopPropagation()
      },
      [maybeClearHighlightsOnHoverEnd],
    )

    const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)
    const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
    const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

    const onEdgeDblClick = React.useCallback(() => {
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

    const hiddenDuringInteraction = useEditorState(
      Substores.canvas,
      (store) =>
        store.editor.canvas.interactionSession != null &&
        store.editor.canvas.interactionSession.activeControl.type === 'RESIZE_HANDLE' &&
        !isEdgePositionEqualTo(
          props.position,
          store.editor.canvas.interactionSession.activeControl.edgePosition,
        ),
      'ResizePoint hiddenDuringInteraction',
    )

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          visibility: hiddenDuringInteraction ? 'hidden' : 'visible',
        }}
      >
        <div
          style={{
            position: 'relative',
            width: ResizePointSize / scale,
            height: ResizePointSize / scale,
            top: -ResizePointOffset / scale,
            left: -ResizePointOffset / scale,
            boxSizing: 'border-box',
            borderWidth: 1 / scale,
            backgroundColor: colorTheme.canvasControlsSizeBoxBackground.value,
            borderRadius: '10%',
            borderStyle: 'none',
            borderColor: 'transparent',
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor50.value} 0px 0px
              ${1 / scale}px, ${colorTheme.canvasControlsSizeBoxShadowColor20.value} 0px ${
              1 / scale
            }px ${2 / scale}px ${1 / scale}px`,
          }}
        />
        <div
          onDoubleClick={onEdgeDblClick}
          style={{
            position: 'relative',
            width: ResizePointMouseAreaSize / scale,
            height: ResizePointMouseAreaSize / scale,
            top: -ResizePointMouseAreaSize / scale,
            left: -ResizePointMouseAreaOffset / scale,
            backgroundColor: 'transparent',
            pointerEvents: 'initial',
            cursor: props.cursor,
          }}
          onMouseDown={onPointMouseDown}
          onMouseMove={onMouseMove}
          data-testid={ResizePointTestId(props.position)}
        />
      </div>
    )
  }),
)
ResizePoint.displayName = 'ResizePoint'

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
