import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  isFiniteRectangle,
  isNotNullFiniteRectangle,
  windowPoint,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { FlexRow, useColorTheme } from '../../../../uuiui'
import { clearHighlightedViews, selectComponents } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { boundingArea, createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isCommentMode, isSelectModeWithArea } from '../../../editor/editor-modes'
import { getElementPathTreeChildren, getSubTree } from '../../../../core/shared/element-path-tree'

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

  const sceneHasSingleChild = useEditorState(
    Substores.metadata,
    (store) => {
      const subTree = getSubTree(store.editor.elementPathTree, props.target)
      if (subTree == null) {
        return false
      } else {
        return getElementPathTreeChildren(subTree).length === 1
      }
    },
    'SceneLabel sceneHasSingleChild',
  )

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

  const sceneSize = useEditorState(
    Substores.metadata,
    (store) => {
      const element = store.editor.jsxMetadata[EP.toString(props.target)]
      if (element == null || !isNotNullFiniteRectangle(element.globalFrame)) {
        return ''
      }
      return `${element.globalFrame.width} × ${element.globalFrame.height}`
    },
    'SceneLabel sceneSize',
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
  const baseFontSize = 10
  const scaledFontSize = baseFontSize / scale
  const scaledLineHeight = 23 / scale
  const paddingY = scaledFontSize / 4
  const paddingX = paddingY * 2
  const offsetY = scaledFontSize / 1.5
  const offsetX = scaledFontSize / 2
  const borderRadius = 5 / scale

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
      const shouldStopPropagation = !isSelectingArea && !isCommentMode(storeRef.current.mode)
      if (shouldStopPropagation) {
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

  if (frame != null && isFiniteRectangle(frame)) {
    return (
      <CanvasOffsetWrapper>
        <FlexRow
          onMouseOver={labelSelectable ? onMouseOver : NO_OP}
          onMouseOut={labelSelectable ? onMouseLeave : NO_OP}
          onMouseDown={labelSelectable ? onMouseDown : NO_OP}
          onMouseMove={labelSelectable ? onMouseMove : NO_OP}
          data-testid={SceneLabelTestID}
          id={SceneLabelTestID}
          className='roleComponentName'
          style={{
            pointerEvents: labelSelectable ? 'initial' : 'none',
            color: sceneHasSingleChild ? colorTheme.primary.value : colorTheme.fg6.value,
            backgroundColor: colorTheme.bg1subdued.value,
            position: 'absolute',
            fontWeight: 600,
            left: frame.x,
            bottom: -frame.y + offsetY,
            width: frame.width,
            padding: `${paddingY}px ${paddingX}px`,
            fontFamily: 'Utopian-Inter',
            fontSize: scaledFontSize,
            lineHeight: `${scaledLineHeight}px`,
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            borderRadius: borderRadius,
            textOverflow: 'ellipsis',
            gap: 20,
          }}
        >
          <div
            style={{
              fontWeight: 600,
            }}
          >
            {label}
          </div>
          <div
            style={{
              fontWeight: 400,
            }}
          >
            {sceneSize}
          </div>
        </FlexRow>
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})
