import React from 'react'
import { useSpring, animated } from 'react-spring'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import {
  easeOutCubic,
  easeOutQuint,
  mod,
  point,
  windowPoint,
} from '../../../core/shared/math-utils'
import { arrayEquals } from '../../../core/shared/utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { Icons, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { CSSCursor } from '../../../uuiui-deps'
import { usePrevious } from '../../editor/hook-utils'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import CanvasActions from '../canvas-actions'
import { createInteractionViaMouse, flowSlider } from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const FlowSliderControl = React.memo(() => {
  const colorTheme = useColorTheme()
  const ref = React.useRef<HTMLDivElement>(null)
  const dispatch = useRefEditorState((store) => store.dispatch)
  const isTargetFlowWithSiblings = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const target = store.editor.selectedViews[0]
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        target,
      )
      if (MetadataUtils.isPositionedByFlow(elementMetadata)) {
        const siblings = MetadataUtils.getSiblings(store.editor.jsxMetadata, target)
        if (
          siblings.length > 1 &&
          siblings.every((sibling) => MetadataUtils.isPositionedByFlow(sibling))
        ) {
          return true
        } else {
          return false
        }
      } else {
        return false
      }
    } else {
      return false
    }
  }, 'isTargetFlowWithSiblings')

  const { siblings, currentIndex } = useEditorState(
    (store) => {
      if (store.editor.selectedViews.length === 1) {
        const target = store.editor.selectedViews[0]
        const siblingsMetadata = MetadataUtils.getSiblings(
          store.editor.canvas.interactionSession?.latestMetadata ?? store.editor.jsxMetadata,
          target,
        ).map((sibling) => sibling.elementPath)
        return {
          siblings: siblingsMetadata,
          currentIndex: siblingsMetadata.findIndex((sibling) => EP.pathsEqual(sibling, target)),
        }
      } else {
        return { siblings: [], currentIndex: -1 }
      }
    },
    'current index',
    (o, n) =>
      arrayEquals(o.siblings, n.siblings, EP.pathsEqual) && o.currentIndex === n.currentIndex,
  )
  const prevCurrentIndex = usePrevious(currentIndex)

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
      if (ref.current != null) {
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

  const isDragging = useEditorState(
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'FLOW_SLIDER',
    'isDragging',
  )

  const frame = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const target = store.editor.selectedViews[0]
      if (isDragging) {
        return MetadataUtils.getFrameInCanvasCoords(target, store.strategyState.startingMetadata)
      } else {
        return MetadataUtils.getFrameInCanvasCoords(target, store.editor.jsxMetadata)
      }
    } else {
      return null
    }
  }, 'frame')

  const startingIndex = useEditorState((store) => {
    if (store.editor.selectedViews.length === 1) {
      const target = store.editor.selectedViews[0]
      if (isDragging) {
        const siblingsMetadata = MetadataUtils.getSiblings(
          store.strategyState.startingMetadata,
          target,
        ).map((sibling) => sibling.elementPath)
        return siblingsMetadata.findIndex((sibling) => EP.pathsEqual(sibling, target))
      } else {
        const siblingsMetadata = MetadataUtils.getSiblings(
          store.editor.canvas.interactionSession?.latestMetadata ?? store.editor.jsxMetadata,
          target,
        ).map((sibling) => sibling.elementPath)
        return siblingsMetadata.findIndex((sibling) => EP.pathsEqual(sibling, target))
      }
    } else {
      return -1
    }
  }, 'starting index')

  const possibleNewIndex = useEditorState((store) => {
    if (
      store.editor.selectedViews.length === 1 &&
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.interactionData.type === 'DRAG' &&
      store.editor.canvas.interactionSession.interactionData.drag != null
    ) {
      const target = store.editor.selectedViews[0]
      const siblingsOfTarget = MetadataUtils.getSiblings(
        store.strategyState.startingMetadata,
        target,
      ).map((sibling) => sibling.elementPath)
      const originalIndex = siblingsOfTarget.findIndex((sibling) => EP.pathsEqual(sibling, target))
      const indexOffset = store.editor.canvas.interactionSession.interactionData.drag.x / 40

      return mod(originalIndex + indexOffset, siblingsOfTarget.length)
    } else {
      return -1
    }
  }, 'possibleNewIndex')

  // the strategy uses Math.round, it switches at 0.5, the diff is always between -0.5 and 0.5
  // easing fns work with values between 0 and 1
  const diff = (possibleNewIndex - currentIndex) * 2

  const offset = possibleNewIndex === -1 ? 0 : Math.sign(diff) * easeOutCubic(Math.abs(diff))
  const offsetWithReset = prevCurrentIndex !== currentIndex ? 0 : offset

  const left = (frame?.x ?? 0) + (frame?.width ?? 0) / 2 - 8 - startingIndex * 16
  const styles = useSpring({
    left: left + currentIndex * 16 + offsetWithReset * 5,
    config: { mass: 5, tension: 1500, friction: 80 },
  })

  if (isTargetFlowWithSiblings && siblings.length > 1 && frame != null) {
    // icon size: 16
    return (
      <CanvasOffsetWrapper>
        {when(
          isDragging,
          <div
            style={{
              position: 'absolute',
              top: frame.y + frame.height / 2 - 11,
              left: left,
              width: siblings.length * 16,
              height: 22,
              borderRadius: 4,
              opacity: '50%',
              background: isDragging ? colorTheme.bg0.value : 'transparent',
              boxShadow: isDragging ? UtopiaStyles.popup.boxShadow : 'none',
              cursor: isDragging ? CSSCursor.ResizeEW : 'default',
              display: 'flex',
              alignItems: 'center',
            }}
          >
            {siblings.map((s, i) => {
              return <Icons.Dot key={EP.toString(s)} />
            })}
          </div>,
        )}
        <animated.div
          style={{
            position: 'absolute',
            top: frame.y + frame.height / 2 - 9,
            left: styles.left,
            width: 14,
            height: 22 - 4,
            borderRadius: 4,
            background: colorTheme.primary.value,
            display: possibleNewIndex !== -1 ? 'block' : 'none',
            // eslint-disable-next-line @typescript-eslint/ban-ts-comment
            // @ts-ignore
            opacity: 0.6,
          }}
        ></animated.div>
        <div
          ref={ref}
          style={{
            position: 'absolute',
            top: frame.y + frame.height / 2 - 5,
            left: left + currentIndex * 16 + 2,
            width: 10, // TODO increase click area
            height: 10,
            borderRadius: '50%',
            background: colorTheme.bg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            cursor: CSSCursor.ResizeEW,
          }}
          onMouseDown={onMouseDown}
        ></div>
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})
