import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  CanvasPoint,
  getRectCenter,
  mod,
  point,
  rectanglesEqual,
  windowPoint,
  zeroCanvasRect,
} from '../../../core/shared/math-utils'
import { arrayEquals } from '../../../core/shared/utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { Icons, useColorTheme } from '../../../uuiui'
import { CSSCursor } from '../../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import CanvasActions from '../canvas-actions'
import { createInteractionViaMouse, flowSlider } from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { ElementPath } from '../../../core/shared/project-file-types'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { findNewIndex } from '../canvas-strategies/strategies/flow-reorder-helpers'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'

export const IconSize = 16
const IndicatorSize = (scale: number) => IconSize / scale
const MenuHeight = (scale: number) => 22 / scale
const IndicatorOffset = (scale: number) => 2 / scale
const ControlSize = (scale: number) => 6 / scale

interface FlowSliderControlProps {
  target: ElementPath | null
}

export const FlowSliderControl = controlForStrategyMemoized(
  ({ target }: FlowSliderControlProps) => {
    const scale = useEditorState((store) => store.editor.canvas.scale, 'FlowSliderControl scale')
    const colorTheme = useColorTheme()
    const isDragging = useEditorState(
      (store) =>
        store.editor.canvas.interactionSession != null &&
        store.editor.canvas.interactionSession.activeControl.type === 'FLOW_SLIDER',
      'FlowSliderControl isDragging',
    )

    const { siblings, latestIndex, startingIndex, startingFrame } = useEditorState(
      (store) => {
        if (target != null) {
          const siblingPaths = MetadataUtils.getSiblingsProjectContentsOrdered(
            store.editor.canvas.interactionSession?.latestMetadata ?? store.editor.jsxMetadata,
            target,
          ).map((sibling) => sibling.elementPath)
          const targetIndex = siblingPaths.findIndex((sibling) => EP.pathsEqual(sibling, target))
          const latestFrame =
            MetadataUtils.getFrameInCanvasCoords(target, store.editor.jsxMetadata) ?? zeroCanvasRect

          if (isDragging) {
            const startingSiblingsMetadata = MetadataUtils.getSiblings(
              store.strategyState.startingMetadata,
              target,
            ).map((sibling) => sibling.elementPath)
            return {
              siblings: siblingPaths,
              latestIndex: targetIndex,
              startingIndex: startingSiblingsMetadata.findIndex((sibling) =>
                EP.pathsEqual(sibling, target),
              ),
              startingFrame:
                MetadataUtils.getFrameInCanvasCoords(
                  target,
                  store.strategyState.startingMetadata,
                ) ?? zeroCanvasRect,
            }
          } else {
            return {
              siblings: siblingPaths,
              latestIndex: targetIndex,
              startingIndex: targetIndex,
              startingFrame: latestFrame,
            }
          }
        } else {
          return { siblings: [], latestIndex: -1, startingIndex: -1, startingFrame: zeroCanvasRect }
        }
      },
      'FlowSliderControl',
      (oldValue, newValue) =>
        arrayEquals(oldValue.siblings, newValue.siblings, EP.pathsEqual) &&
        oldValue.latestIndex === newValue.latestIndex &&
        oldValue.startingIndex === newValue.startingIndex &&
        rectanglesEqual(oldValue.startingFrame, newValue.startingFrame),
    )

    const controlAreaTopLeft = React.useMemo(() => {
      const centerPoint = getRectCenter(startingFrame ?? zeroCanvasRect)
      return {
        x: centerPoint.x - IndicatorSize(scale) / 2 - startingIndex * IndicatorSize(scale),
        y: centerPoint.y - MenuHeight(scale) / 2,
      } as CanvasPoint
    }, [startingFrame, startingIndex, scale])

    const controlTopLeft = React.useMemo(() => {
      return {
        x: controlAreaTopLeft.x + latestIndex * IndicatorSize(scale) + IndicatorSize(scale) / 4,
        y: getRectCenter(startingFrame ?? zeroCanvasRect).y - ControlSize(scale) / 2,
      } as CanvasPoint
    }, [startingFrame, controlAreaTopLeft, latestIndex, scale])

    if (siblings.length > 1) {
      return (
        <CanvasOffsetWrapper>
          {when(
            isDragging,
            <div
              style={{
                position: 'absolute',
                top: controlAreaTopLeft.y,
                left: controlAreaTopLeft.x,
                width: siblings.length * IndicatorSize(scale),
                height: MenuHeight(scale),
                borderRadius: 4 / scale,
                opacity: '60%',
                background: colorTheme.bg0.value,
                boxShadow: `inset 0px 0px 0px ${0.5 / scale}px ${colorTheme.border3.value} , 0px ${
                  2 / scale
                }px ${4 / scale}px 0px ${colorTheme.fg6.o(50).value}`,
                cursor: CSSCursor.ResizeEW,
                display: 'flex',
                alignItems: 'center',
                boxSizing: 'content-box',
                overflow: 'hidden',
              }}
            >
              <ReorderIndicators startingIndex={startingIndex} siblings={siblings} />
              {siblings.map((s) => {
                return (
                  <div key={EP.toString(s)} style={{ zoom: 1 / scale }}>
                    <Icons.Dot width={IconSize} height={IconSize} />
                  </div>
                )
              })}
            </div>,
          )}
          <FlowReorderControl controlPosition={controlTopLeft} />
        </CanvasOffsetWrapper>
      )
    } else {
      return null
    }
  },
)

interface ReorderIndicatorProps {
  startingIndex: number
  siblings: Array<ElementPath>
}

const ReorderIndicators = React.memo((props: ReorderIndicatorProps) => {
  const scale = useEditorState((store) => store.editor.canvas.scale, 'ReorderIndicator scale')
  const { startingIndex, siblings } = props
  const indicatorOffset = useEditorState((store) => {
    if (
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'FLOW_SLIDER' &&
      store.editor.canvas.interactionSession.interactionData.type === 'DRAG' &&
      store.editor.canvas.interactionSession.interactionData.drag != null
    ) {
      const dragDelta = store.editor.canvas.interactionSession.interactionData.drag
      return findNewIndex(startingIndex, dragDelta, siblings, 'raw-value')
    } else {
      return startingIndex
    }
  }, 'FlowSliderControl indicatorOffset')

  // when reaching the end of the slider it will restart from the beginning, a second indicator is shown
  if (indicatorOffset > siblings.length - 1) {
    return (
      <>
        <ReorderIndicator style={{ left: indicatorOffset * IndicatorSize(scale) }} />
        <ReorderIndicator
          style={{ left: (indicatorOffset - siblings.length) * IndicatorSize(scale) }}
        />
      </>
    )
  } else {
    return <ReorderIndicator style={{ left: indicatorOffset * IndicatorSize(scale) }} />
  }
})

const ReorderIndicator = React.memo(({ style }: { style: React.CSSProperties }) => {
  const colorTheme = useColorTheme()
  const scale = useEditorState((store) => store.editor.canvas.scale, 'ReorderIndicator scale')
  return (
    <div
      style={{
        position: 'absolute',
        top: IndicatorOffset(scale),
        width: IndicatorSize(scale) - IndicatorOffset(scale),
        height: MenuHeight(scale) - IndicatorOffset(scale) * 2,
        borderRadius: 4 / scale,
        background: colorTheme.primary.value,
        ...style,
      }}
    />
  )
})

const FlowReorderControl = React.memo(({ controlPosition }: { controlPosition: CanvasPoint }) => {
  const colorTheme = useColorTheme()
  const scale = useEditorState((store) => store.editor.canvas.scale, 'FlowReorderControl scale')
  const ref = React.useRef<HTMLDivElement>(null)
  const ClickAreaSize = ControlSize(scale) + 6 / scale

  const dispatch = useRefEditorState((store) => store.dispatch)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.realCanvasOffset)

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      stopPropagation(event)
      const startPoint = windowToCanvasCoordinates(
        scaleRef.current,
        canvasOffsetRef.current,
        windowPoint(point(event.clientX, event.clientY)),
      ).canvasPositionRounded
      if (ref.current != null && !IS_TEST_ENVIRONMENT) {
        // real user input is needed for enabling pointerLock
        ref.current.requestPointerLock()
      }
      if (event.button !== 2) {
        dispatch.current(
          [
            CanvasActions.createInteractionSession(
              createInteractionViaMouse(
                startPoint,
                Modifier.modifiersForEvent(event),
                flowSlider(),
              ),
            ),
          ],
          'everyone',
        )
      }
    },
    [dispatch, canvasOffsetRef, scaleRef],
  )

  return (
    <React.Fragment>
      <div
        style={{
          position: 'absolute',
          top: controlPosition.y,
          left: controlPosition.x,
          width: ControlSize(scale),
          height: ControlSize(scale),
          borderRadius: '50%',
          background: colorTheme.bg0.value,
          boxShadow: `0px ${1 / scale}px ${2 / scale}px 0px rgb(52 52 52 / 35%), 0px 0px 0px ${
            0.5 / scale
          }px rgb(166 166 166 / 82%)`,
        }}
      ></div>
      <div
        ref={ref}
        data-testid='flow-reorder-slider-control'
        style={{
          position: 'absolute',
          top: controlPosition.y - (ClickAreaSize - ControlSize(scale)) / 2,
          left: controlPosition.x - (ClickAreaSize - ControlSize(scale)) / 2,
          width: ClickAreaSize,
          height: ClickAreaSize,
          cursor: CSSCursor.ResizeEW,
        }}
        onMouseDown={onMouseDown}
      ></div>
    </React.Fragment>
  )
})
