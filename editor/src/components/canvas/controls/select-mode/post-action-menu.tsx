/** @jsxRuntime classic */
/** @jsx jsx */
/* @jsxFrag */
import React from 'react'
import { css, jsx } from '@emotion/react'
import { FlexColumn, FlexRow, UtopiaStyles, colorTheme } from '../../../../uuiui'
import {
  Substores,
  useEditorState,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../../editor/store/store-hook'
import { stopPropagation } from '../../../inspector/common/inspector-utils'
import {
  PostActionChoice,
  generatePostactionChoices as generatePostActionChoices,
} from '../../canvas-strategies/post-action-options/post-action-options'
import { useDispatch } from '../../../editor/store/dispatch-context'
import {
  clearPostActionData,
  executePostActionMenuChoice,
} from '../../../editor/actions/action-creators'
import {
  CanvasVector,
  boundingRectangleArray,
  isInfinityRectangle,
  mod,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { when } from '../../../../utils/react-conditionals'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../../../core/shared/atom-with-pub-sub'
import { NavigatorWidthAtom } from '../../../editor/store/editor-state'

const isPostActionMenuActive = (postActionSessionChoices: PostActionChoice[]) =>
  postActionSessionChoices.length > 0

export const PostActionMenu = React.memo(() => {
  const [open, setOpen] = React.useState<boolean>(false)

  const postActionSessionChoices = useEditorState(
    Substores.postActionInteractionSession,
    (store) =>
      store.postActionInteractionSession == null
        ? []
        : generatePostActionChoices(store.postActionInteractionSession.postActionMenuData),
    'PostActionMenu postActionSessionChoices',
  )

  const activePostActionChoice = useEditorState(
    Substores.postActionInteractionSession,
    (store) => store.postActionInteractionSession?.activeChoiceId,
    'PostActionMenu activePostActionChoice',
  )

  const postActionSessionInProgressRef = useRefEditorState(
    (store) => store.postActionInteractionSession != null,
  )

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'PostActionMenu scale',
  )

  const dispatch = useDispatch()

  const onSetPostActionChoice = React.useCallback(
    (index: number) => {
      const nextChoiceIndex = mod(index, postActionSessionChoices.length)
      const choice = postActionSessionChoices.at(nextChoiceIndex)
      if (choice == null) {
        return
      }

      dispatch([executePostActionMenuChoice(choice)])
    },
    [dispatch, postActionSessionChoices],
  )

  React.useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {
      const keyIntValue = Number.parseInt(event.key)
      const isStrategySwitchingKey = !isNaN(keyIntValue) || event.key === 'Tab'
      const isDismissKey = event.key === 'Enter' || event.key === 'Escape'

      if (
        isStrategySwitchingKey &&
        postActionSessionInProgressRef.current &&
        isPostActionMenuActive(postActionSessionChoices)
      ) {
        event.preventDefault()
        event.stopPropagation()
        event.stopImmediatePropagation()

        if (event.key === 'Tab') {
          const activeStrategyIndex = postActionSessionChoices.findIndex(
            (choice) => choice.id === activePostActionChoice,
          )

          const newStrategyIndex = event.shiftKey
            ? activeStrategyIndex - 1
            : activeStrategyIndex + 1

          onSetPostActionChoice(newStrategyIndex)
          return
        }
        if (!isNaN(keyIntValue)) {
          const index = keyIntValue - 1
          onSetPostActionChoice(index)
        }
      } else if (isDismissKey) {
        event.preventDefault()
        event.stopPropagation()
        event.stopImmediatePropagation()

        alert('dispatch([clearPostActionData()])')

        dispatch([clearPostActionData()])
      } else {
        dispatch([clearPostActionData()])
      }
    }

    if (isPostActionMenuActive(postActionSessionChoices)) {
      window.addEventListener('keydown', handleKeyDown, true)
    }

    return function cleanup() {
      window.removeEventListener('keydown', handleKeyDown, true)
    }
  }, [
    activePostActionChoice,
    dispatch,
    onSetPostActionChoice,
    postActionSessionChoices,
    postActionSessionInProgressRef,
  ])

  const positioningProps: React.CSSProperties = useEditorState(
    Substores.metadata,
    (store) => {
      if (store.editor.selectedViews.length === 0) {
        return {
          top: 4,
          right: 4,
        }
      }

      const aabbs = mapDropNulls((path) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)
        return frame == null || isInfinityRectangle(frame) ? null : frame
      }, store.editor.selectedViews)

      const selectedElementBounds = boundingRectangleArray(aabbs) ?? zeroCanvasRect
      return {
        top: selectedElementBounds.y + selectedElementBounds.height + 12 / scale,
        left: selectedElementBounds.x + selectedElementBounds.width + 12 / scale,
      }
    },
    'PostActionMenu positioningProps',
  )

  const openIfClosed = React.useCallback(
    (e: React.MouseEvent) => {
      stopPropagation(e)
      if (!open) {
        setOpen(true)
      }
    },
    [open],
  )

  React.useEffect(() => {
    if (!isPostActionMenuActive(postActionSessionChoices)) {
      setOpen(false)
    }
  }, [postActionSessionChoices])

  if (!isPostActionMenuActive(postActionSessionChoices)) {
    return null
  }

  return (
    <PostActionMenuOffsetWrapper>
      <div
        style={{
          display: 'block',
          pointerEvents: 'initial',
          position: 'absolute',
          fontSize: 9,
          ...positioningProps,
        }}
        onMouseDown={stopPropagation}
        onClick={openIfClosed}
      >
        <FlexColumn
          style={{
            minHeight: open ? 84 : 30 / scale,
            minWidth: open ? 100 : 30 / scale,
            display: 'flex',
            alignItems: 'stretch',
            padding: 4 / scale,
            borderRadius: 4 / scale,
            border: `1px solid ${colorTheme.navigatorResizeHintBorder.value}`,
            background: open ? colorTheme.bg0.value : colorTheme.primary.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            cursor: open ? undefined : 'pointer',
          }}
        >
          {when(
            open,
            <>
              Paste options
              {postActionSessionChoices.map((choice, index) => {
                const isActive = choice.id === activePostActionChoice
                return (
                  <FlexRow
                    key={choice.id}
                    // eslint-disable-next-line react/jsx-no-bind
                    onClick={() => onSetPostActionChoice(index)}
                    style={{
                      fontSize: '10px',
                      height: 19,
                      paddingLeft: 4,
                      paddingRight: 4,
                      color: colorTheme.textColor.value,
                      cursor: 'pointer',
                    }}
                    css={{
                      '&:hover': {
                        backgroundColor: colorTheme.bg5.value,
                      },
                    }}
                  >
                    <KeyIndicator keyNumber={index + 1} isActive={isActive} />
                    <span>{choice.name}</span>
                  </FlexRow>
                )
              })}
              <div
                style={{
                  alignSelf: 'center',
                  marginTop: 'auto',
                  color: colorTheme.fg5.value,
                }}
              >
                Press{' '}
                <span
                  style={{
                    padding: 2,
                    borderRadius: 2,
                    border: `1px solid ${colorTheme.fg8.value}`,
                  }}
                >
                  Tab
                </span>{' '}
                to switch
              </div>
            </>,
          )}
        </FlexColumn>
      </div>
    </PostActionMenuOffsetWrapper>
  )
})
PostActionMenu.displayName = 'PostActionMenu'

const KeyIndicator = ({ keyNumber, isActive }: { keyNumber: number; isActive: boolean }) => {
  const height = 14
  const width = 14
  return (
    <div
      style={{
        width: width,
        height: height,
        marginRight: 5,
        borderRadius: 3,
        display: 'flex',
        flexDirection: 'row',
        justifyContent: 'center',
        alignItems: 'center',
        border: `1px solid ${isActive ? colorTheme.primary.value : colorTheme.fg4.value}`,
        color: isActive ? colorTheme.white.value : undefined,
        backgroundColor: isActive ? colorTheme.primary.value : undefined,
      }}
    >
      <span
        style={{
          fontWeight: 700,
          fontSize: '8px',
        }}
      >
        {keyNumber}
      </span>
    </div>
  )
}

export const PostActionMenuOffsetWrapper = React.memo((props: { children?: React.ReactNode }) => {
  const elementRef = useApplyCanvasOffsetToStyle()

  return (
    <div ref={elementRef} style={{ position: 'absolute' }}>
      {props.children}
    </div>
  )
})

export function useApplyCanvasOffsetToStyle(): React.RefObject<HTMLDivElement> {
  const elementRef = React.useRef<HTMLDivElement>(null)
  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

  const isNavigatorOverCanvas = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.navigator.minimised,
    'ErrorOverlayComponent isOverlappingWithNavigator',
  )

  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)

  const applyCanvasOffset = React.useCallback(
    (roundedCanvasOffset: CanvasVector) => {
      if (selectedViewsRef.current.length === 0) {
        return
      }

      const navigatorWidthOffset = isNavigatorOverCanvas ? navigatorWidth : 0

      if (elementRef.current != null) {
        elementRef.current.style.setProperty(
          'transform',
          `scale(${scaleRef.current}) translate3d(${
            roundedCanvasOffset.x - navigatorWidthOffset
          }px, ${roundedCanvasOffset.y}px, 0)`,
        )
      }
    },
    [isNavigatorOverCanvas, navigatorWidth, scaleRef, selectedViewsRef],
  )

  useSelectorWithCallback(
    Substores.canvasOffset,
    (store) => store.editor.canvas.roundedCanvasOffset,
    applyCanvasOffset,
    'useApplyCanvasOffsetToStyle',
  )

  const applyCanvasOffsetEffect = React.useCallback(() => {
    applyCanvasOffset(canvasOffsetRef.current)
  }, [applyCanvasOffset, canvasOffsetRef])
  React.useLayoutEffect(applyCanvasOffsetEffect, [applyCanvasOffsetEffect])

  return elementRef
}
