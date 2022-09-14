import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { point, windowPoint } from '../../../core/shared/math-utils'
import { Modifier } from '../../../utils/modifiers'
import { when } from '../../../utils/react-conditionals'
import { Icons, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { CSSCursor } from '../../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { stopPropagation } from '../../inspector/common/inspector-utils'
import CanvasActions from '../canvas-actions'
import { createInteractionViaMouse, flowSlider } from '../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

export const FlowSliderControl = React.memo(() => {
  const colorTheme = useColorTheme()
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

  const { siblings, currentIndex } = useEditorState((store) => {
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
  }, 'current index')

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

  if (isTargetFlowWithSiblings && siblings.length > 1 && frame != null) {
    // icon size: 16
    const left = (frame?.x ?? 0) + (frame?.width ?? 0) / 2 - 8 - startingIndex * 16
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
              background: colorTheme.bg0.value,
              boxShadow: UtopiaStyles.popup.boxShadow,
              cursor: CSSCursor.ResizeEW,
              display: 'flex',
              alignItems: 'center',
            }}
          >
            {siblings.map((s, i) => (
              <Icons.Dot key={EP.toString(s)} />
            ))}
          </div>,
        )}
        <div
          style={{
            position: 'absolute',
            top: frame.y + frame.height / 2 - 5,
            left: left + currentIndex * 16 + 1.5,
            width: 10,
            height: 10,
            borderRadius: '50%',
            background: colorTheme.bg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            cursor: CSSCursor.ResizeEW,
            transition: isDragging ? 'left .2s ease-in-out' : undefined,
          }}
          onMouseDown={onMouseDown}
          onMouseUp={stopPropagation}
        ></div>
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})
