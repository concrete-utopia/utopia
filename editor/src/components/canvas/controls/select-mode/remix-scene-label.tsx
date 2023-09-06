import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { isFiniteRectangle, windowPoint } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { Modifier } from '../../../../utils/modifiers'
import { FlexRow, Icn, Tooltip, useColorTheme } from '../../../../uuiui'
import { clearHighlightedViews, selectComponents } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { boundingArea, createInteractionViaMouse } from '../../canvas-strategies/interaction-state'
import { windowToCanvasCoordinates } from '../../dom-lookup'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isSelectModeWithArea } from '../../../editor/editor-modes'
import { getSubTree } from '../../../../core/shared/element-path-tree'
import { useAtom } from 'jotai'
import { RemixNavigationAtom } from '../../remix/utopia-remix-root-component'

interface RemixSceneLabelControlProps {
  maybeHighlightOnHover: (target: ElementPath) => void
  maybeClearHighlightsOnHoverEnd: () => void
}

interface RemixSceneLabelProps extends RemixSceneLabelControlProps {
  target: ElementPath
}

export const RemixSceneLabelTestID = 'remix-scene-label'

export const RemixSceneLabelControl = React.memo<RemixSceneLabelControlProps>((props) => {
  const sceneTargets = useEditorState(
    Substores.metadata,
    (store) =>
      Object.values(store.editor.jsxMetadata).filter((element) =>
        MetadataUtils.isProbablyRemixScene(store.editor.jsxMetadata, element.elementPath),
      ),
    'RemixSceneLabelControl',
  )
  return (
    <>
      {sceneTargets.map((element) => (
        <RemixSceneLabel
          key={EP.toString(element.elementPath)}
          target={element.elementPath}
          maybeHighlightOnHover={props.maybeHighlightOnHover}
          maybeClearHighlightsOnHoverEnd={props.maybeClearHighlightsOnHoverEnd}
        />
      ))}
    </>
  )
})

const RemixSceneLabel = React.memo<RemixSceneLabelProps>((props) => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const [navigationData] = useAtom(RemixNavigationAtom)

  const currentPath = (navigationData[EP.toString(props.target)] ?? null)?.location.pathname
  const isIndexRoute = currentPath === '/'

  const label = isIndexRoute ? '(home)' : currentPath

  const forward = React.useCallback(
    () => navigationData[EP.toString(props.target)]?.forward(),
    [navigationData, props.target],
  )
  const back = React.useCallback(
    () => navigationData[EP.toString(props.target)]?.back(),
    [navigationData, props.target],
  )
  const home = React.useCallback(
    () => navigationData[EP.toString(props.target)]?.home(),
    [navigationData, props.target],
  )

  const labelSelectable = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.keysPressed['z'],
    'RemixSceneLabel Z key pressed',
  )

  const frame = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.getFrameInCanvasCoords(props.target, store.editor.jsxMetadata),
    'RemixSceneLabel frame',
  )

  const canvasOffset = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.realCanvasOffset,
    'RemixSceneLabel canvasOffset',
  )
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'RemixSceneLabel scale',
  )
  const baseFontSize = 9
  const scaledFontSize = baseFontSize / scale
  const scaledLineHeight = 17 / scale
  const paddingY = scaledFontSize / 2
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
    'RemixSceneLabel isSelected',
  )
  const isHighlighted = useEditorState(
    Substores.highlightedHoveredViews,
    (store) => store.editor.highlightedViews.some((view) => EP.pathsEqual(props.target, view)),
    'RemixSceneLabel isHighlighted',
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

  // TODO: icon scaling
  const highlightColor = colorTheme.fg9.value
  const backgroundColor = colorTheme.white.value
  const boxShadowWidth = 1.5 / scale
  const boxShadow = `0px 0px 0px ${boxShadowWidth}px ${highlightColor}`

  if (frame != null && isFiniteRectangle(frame)) {
    return (
      <CanvasOffsetWrapper>
        <FlexRow
          onMouseOver={labelSelectable ? onMouseOver : NO_OP}
          onMouseOut={labelSelectable ? onMouseLeave : NO_OP}
          onMouseDown={labelSelectable ? onMouseDown : NO_OP}
          onMouseMove={labelSelectable ? onMouseMove : NO_OP}
          data-testid={RemixSceneLabelTestID}
          className='roleComponentName'
          style={{
            pointerEvents: labelSelectable ? 'initial' : 'none',
            color: isIndexRoute ? colorTheme.subduedForeground.value : colorTheme.textColor.value,
            position: 'absolute',
            left: frame.x,
            bottom: -frame.y + offsetY,
            width: frame.width,
            paddingLeft: offsetX,
            paddingTop: paddingY,
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
            gap: 12 / scale,
          }}
        >
          <Tooltip title={'Back'}>
            <span style={{ cursor: 'pointer', fontSize: 16 / scale }} onMouseDown={back}>
              〱
            </span>
          </Tooltip>
          <Tooltip title={'Forward'}>
            <span
              style={{ cursor: 'pointer', fontSize: 16 / scale, transform: 'scale(-1, 1)' }}
              onMouseDown={forward}
            >
              〱
            </span>
          </Tooltip>
          <Tooltip title={'Home'}>
            <span style={{ cursor: 'pointer', fontSize: 16 / scale }} onMouseDown={home}>
              ／
            </span>
          </Tooltip>
          <div
            style={{
              backgroundColor: '#f2f3f4',
              borderRadius: 10 / scale,
              padding: `${4 / scale}px ${12 / scale}px`,
              fontSize: 14 / scale,
            }}
          >
            {label}
          </div>
        </FlexRow>
      </CanvasOffsetWrapper>
    )
  } else {
    return null
  }
})
