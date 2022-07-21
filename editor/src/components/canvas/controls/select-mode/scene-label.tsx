import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { windowPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { useColorTheme } from '../../../../uuiui'
import { clearHighlightedViews, selectComponents } from '../../../editor/actions/action-creators'
import { useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { boundingArea, createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

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
  const dispatch = useEditorState((store) => store.dispatch, 'SceneLabel dispatch')

  const labelSelectable = useEditorState(
    (store) => !store.editor.keysPressed['z'],
    'SceneLabel Z key pressed',
  )
  const label = useEditorState(
    (store) =>
      MetadataUtils.getElementLabel(
        store.editor.allElementProps,
        props.target,
        store.editor.jsxMetadata,
      ),
    'SceneLabel label',
  )
  const frame = useEditorState(
    (store) => MetadataUtils.getFrameInCanvasCoords(props.target, store.editor.jsxMetadata),
    'SceneLabel frame',
  )

  const canvasOffset = useEditorState(
    (store) => store.editor.canvas.realCanvasOffset,
    'SceneLabel canvasOffset',
  )
  const scale = useEditorState((store) => store.editor.canvas.scale, 'SceneLabel scale')
  const baseFontSize = 9
  const scaledFontSize = baseFontSize / scale
  const paddingY = scaledFontSize / 9
  const offsetY = scaledFontSize
  const offsetX = scaledFontSize
  const borderRadius = 3 / scale

  const isSelected = useEditorState(
    (store) => store.editor.selectedViews.some((view) => EP.pathsEqual(props.target, view)),
    'SceneLabel isSelected',
  )
  const isHighlighted = useEditorState(
    (store) => store.editor.highlightedViews.some((view) => EP.pathsEqual(props.target, view)),
    'SceneLabel isHighlighted',
  )

  const onMouseMove = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => event.stopPropagation(),
    [],
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

  const onMouseDown = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
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
            boundingArea(props.target),
          ),
        )
        dispatch([selectAction, dragAction], 'canvas')
      }
    },
    [dispatch, scale, canvasOffset, props.target],
  )

  const onMouseUp = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.stopPropagation()
      dispatch([CanvasActions.clearInteractionSession(true)], 'canvas')
    },
    [dispatch],
  )

  const highlightColor = colorTheme.fg9.value
  const selectedColor = colorTheme.verySubduedForeground.value
  const backgroundColor = isSelected ? selectedColor : highlightColor
  const boxShadowWidth = 1.5 / scale
  const boxShadow = `0px 0px 0px ${boxShadowWidth}px ${backgroundColor}`

  if (frame != null) {
    return (
      <CanvasOffsetWrapper>
        <div
          onMouseOver={labelSelectable ? onMouseOver : NO_OP}
          onMouseOut={labelSelectable ? onMouseLeave : NO_OP}
          onMouseDown={labelSelectable ? onMouseDown : NO_OP}
          onMouseUp={labelSelectable ? onMouseUp : NO_OP}
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
