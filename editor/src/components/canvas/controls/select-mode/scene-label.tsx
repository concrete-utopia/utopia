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
import { ControlFontSize } from '../../canvas-controls-frame'
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
    (store) => MetadataUtils.getElementLabel(props.target, store.editor.jsxMetadata),
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
  const scaledFontSize = ControlFontSize / scale
  const offsetY = -(scaledFontSize * 1.5)
  const offsetX = 3 / scale

  const isSelected = useEditorState(
    (store) => store.editor.selectedViews.some((view) => EP.pathsEqual(props.target, view)),
    'SceneLabel isSelected',
  )
  const isHighlighted = useEditorState(
    (store) => store.editor.selectedViews.some((view) => EP.pathsEqual(props.target, view)),
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

  if (frame != null) {
    return (
      <CanvasOffsetWrapper>
        <div
          onMouseOver={labelSelectable ? onMouseOver : NO_OP}
          onMouseOut={labelSelectable ? onMouseLeave : NO_OP}
          onMouseDown={labelSelectable ? onMouseDown : NO_OP}
          onMouseMove={labelSelectable ? onMouseMove : NO_OP}
          className='roleComponentName'
          style={{
            pointerEvents: labelSelectable ? 'initial' : 'none',
            color: colorTheme.subduedForeground.value,
            position: 'absolute',
            fontWeight: 500,
            left: frame.x + offsetX,
            top: frame.y + offsetY,
            maxWidth: frame.width,
            paddingBottom: '0px',
            fontFamily:
              '-apple-system, BlinkMacSystemFont, Helvetica, "Segoe UI", Roboto,  Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"',
            fontSize: scaledFontSize,
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            textDecoration: isSelected || isHighlighted ? 'underline' : undefined,
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
