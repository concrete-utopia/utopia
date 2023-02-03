import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  CanvasVector,
  nullIfInfinity,
  windowPoint,
} from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import { EditorDispatch } from '../../../editor/action-types'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { getMetadata } from '../../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { detectFillHugFixedState, FixedHugFill, invert } from '../../../inspector/inspector-common'
import { setPropHugStrategies } from '../../../inspector/inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from '../../../inspector/inspector-strategies/inspector-strategy'
import CanvasActions from '../../canvas-actions'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import { CSSCursor, EdgePosition } from '../../canvas-types'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useMaybeHighlightElement } from './select-mode-hooks'

interface AbsoluteResizeControlProps {
  targets: Array<ElementPath>
}

export const AbsoluteResizeControl = controlForStrategyMemoized(
  ({ targets }: AbsoluteResizeControlProps) => {
    const controlRef = useBoundingBox(targets, (ref, boundingBox) => {
      if (isZeroSizedElement(boundingBox)) {
        ref.current.style.display = 'none'
      } else {
        ref.current.style.display = 'block'
        ref.current.style.left = boundingBox.x + 'px'
        ref.current.style.top = boundingBox.y + 'px'
        ref.current.style.width = boundingBox.width + 'px'
        ref.current.style.height = boundingBox.height + 'px'
      }
    })

    const leftRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.height = boundingBox.height + 'px'
    })
    const topRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.width = boundingBox.width + 'px'
    })
    const rightRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.left = boundingBox.width + 'px'
      ref.current.style.height = boundingBox.height + 'px'
    })

    const bottomRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.top = boundingBox.height + 'px'
      ref.current.style.width = boundingBox.width + 'px'
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

    const resizeRef = useBoundingBox(targets, (ref, boundingBox) => {
      ref.current.style.top = boundingBox.height + 'px'
      ref.current.style.left = 0 + 'px'
      ref.current.style.width = boundingBox.width + 'px'
    })

    return (
      <CanvasOffsetWrapper>
        <div
          ref={controlRef}
          style={{
            position: 'absolute',
            pointerEvents: 'none',
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
          <ResizeEdge
            ref={leftRef}
            position={{ x: 0, y: 0.5 }}
            cursor={CSSCursor.ResizeEW}
            direction='vertical'
          />
          <ResizeEdge
            ref={topRef}
            position={{ x: 0.5, y: 0 }}
            cursor={CSSCursor.ResizeNS}
            direction='horizontal'
          />
          <ResizePoint ref={topLeftRef} position={{ x: 0, y: 0 }} cursor={CSSCursor.ResizeNWSE} />
          <ResizePoint ref={topRightRef} position={{ x: 1, y: 0 }} cursor={CSSCursor.ResizeNESW} />
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
          <SizeLabel ref={resizeRef} targets={targets} />
        </div>
      </CanvasOffsetWrapper>
    )
  },
)

interface ResizePointProps {
  cursor: CSSCursor
  position: EdgePosition
}

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

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
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
              ${1 / scale}px, ${colorTheme.canvasControlsSizeBoxShadowColor21.value} 0px ${
              1 / scale
            }px ${2 / scale}px ${1 / scale}px`,
          }}
        />
        <div
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
          data-testid={`resize-control-${props.position.x}-${props.position.y}`}
        />
      </div>
    )
  }),
)

interface ResizeEdgeProps {
  cursor: CSSCursor
  direction: 'horizontal' | 'vertical'
  position: EdgePosition
}

const ResizeMouseAreaSize = 10
const ResizeEdge = React.memo(
  React.forwardRef<HTMLDivElement, ResizeEdgeProps>((props, ref) => {
    const scale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'ResizeEdge scale',
    )
    const dispatch = useDispatch()
    const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
    const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const selectedElementsRef = useRefEditorState((store) => store.editor.selectedViews)
    const { maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

    const onEdgeMouseDown = React.useCallback(
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

    const onEdgeDblClick = React.useCallback(
      (event: React.MouseEvent<HTMLDivElement>) => {
        if (event.detail != 2) {
          return
        }

        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedElementsRef.current,
          setPropHugStrategies(invert(props.direction)),
        )
      },
      [dispatch, metadataRef, props.direction, selectedElementsRef],
    )

    const lineSize = ResizeMouseAreaSize / scale
    const width = props.direction === 'horizontal' ? undefined : lineSize
    const height = props.direction === 'vertical' ? undefined : lineSize
    const offsetLeft = props.direction === 'horizontal' ? `0px` : `${-lineSize / 2}px`
    const offsetTop = props.direction === 'vertical' ? `0px` : `${-lineSize / 2}px`
    return (
      <div
        onClick={onEdgeDblClick}
        ref={ref}
        style={{
          position: 'absolute',
          width: width,
          height: height,
          backgroundColor: 'transparent',
          cursor: props.cursor,
          pointerEvents: 'initial',
          transform: `translate(${offsetLeft}, ${offsetTop})`,
        }}
        onMouseDown={onEdgeMouseDown}
        onMouseMove={onMouseMove}
        data-testid={`resize-control-${props.position.x}-${props.position.y}`}
      />
    )
  }),
)

const sizeLabel = (state: FixedHugFill['type'], actualSize: number): string => {
  switch (state) {
    case 'fill':
      return 'Fill'
    case 'fixed':
      return `${actualSize}`
    case 'hug':
      return 'Hug'
  }
}

function sizeLabelContents(
  metadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
): [string, string] | null {
  if (selectedElements.length === 0) {
    return null
  }

  if (selectedElements.length === 1) {
    const element = MetadataUtils.findElementByElementPath(metadata, selectedElements[0])
    if (element == null) {
      return null
    }

    const horizontal =
      detectFillHugFixedState('horizontal', metadata, selectedElements[0])?.type ?? 'fixed'
    const vertical =
      detectFillHugFixedState('vertical', metadata, selectedElements[0])?.type ?? 'fixed'
    return [
      sizeLabel(horizontal, element.specialSizeMeasurements.clientWidth),
      sizeLabel(vertical, element.specialSizeMeasurements.clientHeight),
    ]
  }

  const boundingBox = boundingRectangleArray(
    selectedElements.map((t) => nullIfInfinity(MetadataUtils.getFrameInCanvasCoords(t, metadata))),
  )
  if (boundingBox != null) {
    return [`${boundingBox.width}`, `${boundingBox.height}`]
  }

  return null
}

interface SizeLabelProps {
  targets: Array<ElementPath>
}

const SizeLabel = React.memo(
  React.forwardRef<HTMLDivElement, SizeLabelProps>(({ targets }, ref) => {
    const scale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'Resizelabel scale',
    )
    const colorTheme = useColorTheme()
    const metadata = useEditorState(
      Substores.metadata,
      (store) => getMetadata(store.editor),
      'ResizeLabel metadata',
    )

    const label = sizeLabelContents(metadata, targets)

    return (
      <div
        ref={ref}
        style={{
          position: 'absolute',
          pointerEvents: 'none',
          display: 'flex',
          justifyContent: 'center',
        }}
        data-testid='parent-resize-label'
      >
        {when(
          label != null,
          <div
            style={{
              marginTop: 8 / scale,
              padding: `0px ${2 / scale}px`,
              borderRadius: 2 / scale,
              color: colorTheme.white.value,
              backgroundColor: colorTheme.secondaryBlue.value,
              fontSize: 11 / scale,
            }}
          >
            {`${label![0]} x ${label![1]}`}
          </div>,
        )}
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
          'zero-drag-not-permitted',
        ),
      ),
    ])
  }
}
