import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { isFiniteRectangle, windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme } from '../../../../uuiui'
import { clearHighlightedViews, selectComponents } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { boundingArea, createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isSelectModeWithArea } from '../../../editor/editor-modes'

interface SceneLabelControlProps {
  maybeHighlightOnHover: (target: ElementPath) => void
  maybeClearHighlightsOnHoverEnd: () => void
}

interface SceneLabelProps extends SceneLabelControlProps {
  target: ElementPath
}

export const SceneLabelTestID = 'scene-label'

export const SceneLabelControl = React.memo<SceneLabelControlProps>((props) => {
  const sceneTargets = useEditorState(
    Substores.metadata,
    (store) =>
      Object.values(store.editor.jsxMetadata).filter((element) =>
        MetadataUtils.isProbablyScene(store.editor.jsxMetadata, element.elementPath),
      ),
    'SceneLabelControl',
  )
  return (
    <>
      {sceneTargets.map((element) => (
        <SceneLabel
          key={EP.toString(element.elementPath)}
          target={element.elementPath}
          maybeHighlightOnHover={props.maybeHighlightOnHover}
          maybeClearHighlightsOnHoverEnd={props.maybeClearHighlightsOnHoverEnd}
        />
      ))}
    </>
  )
})

const SceneLabel = React.memo<SceneLabelProps>((props) => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const labelSelectable = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.keysPressed['z'],
    'SceneLabel Z key pressed',
  )
  const label = useEditorState(
    Substores.metadata,
    (store) =>
      MetadataUtils.getElementLabel(
        store.editor.allElementProps,
        props.target,
        store.editor.elementPathTree,
        store.editor.jsxMetadata,
      ),
    'SceneLabel label',
  )
  const frame = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.getFrameInCanvasCoords(props.target, store.editor.jsxMetadata),
    'SceneLabel frame',
  )

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.realCanvasOffset,
    'SceneLabel canvasOffset',
  )
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'SceneLabel scale',
  )
  const baseFontSize = 9
  const scaledFontSize = baseFontSize / scale
  const scaledLineHeight = 17 / scale
  const paddingY = scaledFontSize / 9
  const offsetY = scaledFontSize
  const offsetX = scaledFontSize
  const borderRadius = 3 / scale

  const storeRef = useRefEditorState((store) => {
    return {
      mode: store.editor.mode,
    }
  })

  const isSelected = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews.some((view) => EP.pathsEqual(props.target, view)),
    'SceneLabel isSelected',
  )
  const isHighlighted = useEditorState(
    Substores.highlightedHoveredViews,
    (store) => store.editor.highlightedViews.some((view) => EP.pathsEqual(props.target, view)),
    'SceneLabel isHighlighted',
  )

  const onMouseMove = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      const isSelectingArea = isSelectModeWithArea(storeRef.current.mode)
      if (!isSelectingArea) {
        event.stopPropagation()
      }
    },
    [storeRef],
  )
  const onMouseOver = React.useCallback(() => {
    if (!isHighlighted) {
      if (isSelected) {
        props.maybeClearHighlightsOnHoverEnd()
      } else {
        props.maybeHighlightOnHover(props.target)
      }
    }
  }, [isHighlighted, isSelected, props])

  const onMouseLeave = React.useCallback(() => {
    if (isHighlighted) {
      dispatch([clearHighlightedViews()], 'canvas')
    }
  }, [dispatch, isHighlighted])

  const onMouseUp = React.useCallback(
    (event: MouseEvent) => {
      event.stopPropagation()
      window.removeEventListener('mouseup', onMouseUp, true)
      dispatch([CanvasActions.clearInteractionSession(true)], 'canvas')
    },
    [dispatch],
  )

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      window.addEventListener('mouseup', onMouseUp, true)
      if (event.buttons === 1 && event.button !== 2) {
        event.stopPropagation()

        const isMultiselect = event.shiftKey
        const selectAction = selectComponents([props.target], isMultiselect)
        const canvasPositions = windowToCanvasCoordinates(
          scale,
          canvasOffset,
          windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
        )

        const dragAction = CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            canvasPositions.canvasPositionRaw,
            Modifier.modifiersForEvent(event),
            boundingArea(),
            'zero-drag-not-permitted',
          ),
        )
        dispatch([selectAction, dragAction], 'canvas')
      }
    },
    [dispatch, scale, canvasOffset, props.target, onMouseUp],
  )

  React.useEffect(() => {
    return () => {
      window.removeEventListener('mouseup', onMouseUp, true)
    }
  }, [onMouseUp])

  const highlightColor = colorTheme.fg9.value
  const selectedColor = colorTheme.verySubduedForeground.value
  const backgroundColor = isSelected ? selectedColor : highlightColor
  const boxShadowWidth = 1.5 / scale
  const boxShadow = `0px 0px 0px ${boxShadowWidth}px ${backgroundColor}`

  if (frame != null && isFiniteRectangle(frame)) {
    return (
      <CanvasOffsetWrapper>
        <div
          onMouseOver={labelSelectable ? onMouseOver : NO_OP}
          onMouseOut={labelSelectable ? onMouseLeave : NO_OP}
          onMouseDown={labelSelectable ? onMouseDown : NO_OP}
          onMouseMove={labelSelectable ? onMouseMove : NO_OP}
          data-testid={SceneLabelTestID}
          className='roleComponentName'
          style={{
            pointerEvents: labelSelectable ? 'initial' : 'none',
            color: colorTheme.subduedForeground.value,
            position: 'absolute',
            fontWeight: 600,
            left: frame.x,
            bottom: -frame.y + offsetY,
            width: frame.width,
            paddingLeft: offsetX,
            paddingBottom: paddingY,
            fontFamily:
              '-apple-system, BlinkMacSystemFont, Helvetica, "Segoe UI", Roboto,  Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"',
            fontSize: scaledFontSize,
            lineHeight: `${scaledLineHeight}px`,
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            boxShadow: boxShadow,
            borderRadius: borderRadius,
            backgroundColor: backgroundColor,
          }}
        >
          {label}
        </div>
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})
