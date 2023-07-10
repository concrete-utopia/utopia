import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  isInfinityRectangle,
  nullIfInfinity,
  windowPoint,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever, NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { when } from '../../../../utils/react-conditionals'
import { useColorTheme } from '../../../../uuiui'
import type { EditorDispatch } from '../../../editor/action-types'
import { applyCommandsAction } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { getMetadata } from '../../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import type { FixedHugFill } from '../../../inspector/inspector-common'
import {
  detectFillHugFixedState,
  invert,
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

export const AbsoluteResizeControlTestId = (targets: Array<ElementPath>): string =>
  `${targets.map(EP.toString).sort()}-absolute-resize-control`

interface AbsoluteResizeControlProps {
  targets: Array<ElementPath>
}

export const SizeLabelTestId = 'SizeLabelTestId'
export const SmallElementSize = 20

function shouldUseSmallElementResizeControl(size: number, scale: number): boolean {
  return size <= SmallElementSize / scale
}

export const AbsoluteResizeControl = controlForStrategyMemoized(
  ({ targets }: AbsoluteResizeControlProps) => {
    const scale = useEditorState(
      Substores.canvasOffset,
      (store) => store.editor.canvas.scale,
      'AbsoluteResizeControl scale',
    )

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
      const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.width, scale)
      const lineSize = ResizeMouseAreaSize / scale
      const width = isSmallElement ? lineSize / 2 : lineSize
      const offsetLeft = `${-lineSize / 2}px`
      const offsetTop = `0px`

      ref.current.style.width = `${width}px`
      ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
      ref.current.style.height = boundingBox.height + 'px'
    })
    const topRef = useBoundingBox(targets, (ref, boundingBox) => {
      const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.height, scale)
      const lineSize = ResizeMouseAreaSize / scale
      const height = isSmallElement ? lineSize / 2 : lineSize
      const offsetLeft = `0px`
      const offsetTop = `${-lineSize / 2}px`

      ref.current.style.width = boundingBox.width + 'px'
      ref.current.style.height = height + 'px'
      ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
    })
    const rightRef = useBoundingBox(targets, (ref, boundingBox) => {
      const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.width, scale)
      const lineSize = ResizeMouseAreaSize / scale
      const width = isSmallElement ? lineSize / 2 : lineSize
      const offsetLeft = isSmallElement ? `0px` : `${-lineSize / 2}px`
      const offsetTop = `0px`

      ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
      ref.current.style.left = boundingBox.width + 'px'
      ref.current.style.width = width + 'px'
      ref.current.style.height = boundingBox.height + 'px'
    })

    const bottomRef = useBoundingBox(targets, (ref, boundingBox) => {
      const isSmallElement = shouldUseSmallElementResizeControl(boundingBox.height, scale)
      const lineSize = ResizeMouseAreaSize / scale
      const height = isSmallElement ? lineSize / 2 : lineSize
      const offsetLeft = `0px`
      const offsetTop = isSmallElement ? `0px` : `${-lineSize / 2}px`

      ref.current.style.transform = `translate(${offsetLeft}, ${offsetTop})`
      ref.current.style.top = boundingBox.height + 'px'
      ref.current.style.width = boundingBox.width + 'px'
      ref.current.style.height = height + 'px'
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
          data-testid={AbsoluteResizeControlTestId(targets)}
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
    const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
    const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

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

    const onEdgeDblClick = React.useCallback(() => {
      executeFirstApplicableStrategy(
        dispatch,
        metadataRef.current,
        selectedElementsRef.current,
        elementPathTreeRef.current,
        allElementPropsRef.current,
        setPropHugStrategies(invert(props.direction)),
      )
    }, [
      allElementPropsRef,
      dispatch,
      metadataRef,
      props.direction,
      elementPathTreeRef,
      selectedElementsRef,
    ])

    return (
      <div
        onDoubleClick={onEdgeDblClick}
        ref={ref}
        style={{
          position: 'absolute',
          backgroundColor: 'transparent',
          cursor: props.cursor,
          pointerEvents: 'initial',
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
    case 'hug':
      return 'Hug'
    case 'fixed':
    case 'detected':
    case 'computed':
      return `${actualSize}`
    default:
      assertNever(state)
  }
}

function sizeLabelContents(
  metadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
): { h: string; v: string } | null {
  if (selectedElements.length === 0) {
    return null
  }

  if (selectedElements.length === 1) {
    const globalFrame = MetadataUtils.findElementByElementPath(
      metadata,
      selectedElements[0],
    )?.globalFrame
    if (globalFrame == null || isInfinityRectangle(globalFrame)) {
      return null
    }

    const horizontal =
      detectFillHugFixedState('horizontal', metadata, selectedElements[0]).fixedHugFill?.type ??
      'fixed'
    const vertical =
      detectFillHugFixedState('vertical', metadata, selectedElements[0]).fixedHugFill?.type ??
      'fixed'
    return {
      h: sizeLabel(horizontal, globalFrame.width),
      v: sizeLabel(vertical, globalFrame.height),
    }
  }

  const boundingBox = boundingRectangleArray(
    selectedElements.map((t) => nullIfInfinity(MetadataUtils.getFrameInCanvasCoords(t, metadata))),
  )
  if (boundingBox != null) {
    return { h: `${boundingBox.width}`, v: `${boundingBox.height}` }
  }

  return null
}

interface SizeLabelProps {
  targets: Array<ElementPath>
}

const FontSize = 11
const PaddingV = 0
const PaddingH = 2
const ExplicitHeightHacked = 20
const BorderRadius = 2
const SizeLabelMarginTop = 8

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

    const labelText = label == null ? null : `${label.h} x ${label.v}`

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
          labelText != null,
          <div
            data-testid={SizeLabelTestId}
            style={{
              display: 'flex',
              alignItems: 'center',
              marginTop: SizeLabelMarginTop / scale,
              padding: `${PaddingV}px ${PaddingH / scale}px`,
              borderRadius: BorderRadius / scale,
              color: colorTheme.white.value,
              backgroundColor: colorTheme.primary.value,
              fontSize: FontSize / scale,
              height: ExplicitHeightHacked / scale,
            }}
          >
            {labelText}
          </div>,
        )}
      </div>
    )
  }),
)
SizeLabel.displayName = 'SizeLabel'

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
