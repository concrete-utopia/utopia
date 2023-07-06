/** @jsxRuntime classic */
/** @jsx jsx */
/* @jsxFrag */
import React from 'react'
import { jsx } from '@emotion/react'
import {
  FlexColumn,
  FlexRow,
  Icn,
  InspectorSubsectionHeader,
  UtopiaStyles,
  colorTheme,
} from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { stopPropagation } from '../../../inspector/common/inspector-utils'
import {
  PostActionChoice,
  generatePostactionChoices as generatePostActionChoices,
} from '../../canvas-strategies/post-action-options/post-action-options'
import { useDispatch } from '../../../editor/store/dispatch-context'
import {
  clearPostActionData,
  executePostActionMenuChoice,
  undo,
} from '../../../editor/actions/action-creators'
import {
  boundingRectangleArray,
  isInfinityRectangle,
  mod,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { createSelector } from 'reselect'
import { PostActionInteractionSessionSubstate } from '../../../editor/store/store-hook-substore-types'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

const PostActionChoicesSelector = createSelector(
  (store: PostActionInteractionSessionSubstate) => store.postActionInteractionSession,
  (session) => (session == null ? [] : generatePostActionChoices(session.postActionMenuData)),
)

const isPostActionMenuActive = (postActionSessionChoices: PostActionChoice[]) =>
  postActionSessionChoices.length > 0

export const PostActionMenu = React.memo(
  ({ postActionSessionChoices }: { postActionSessionChoices: PostActionChoice[] }) => {
    const activePostActionChoice = useEditorState(
      Substores.postActionInteractionSession,
      (store) => store.postActionInteractionSession?.activeChoiceId,
      'PostActionMenu activePostActionChoice',
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

        if (isStrategySwitchingKey && isPostActionMenuActive(postActionSessionChoices)) {
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
    }, [activePostActionChoice, dispatch, onSetPostActionChoice, postActionSessionChoices])

    const undoOption = React.useCallback(
      () => dispatch([clearPostActionData(), undo()]),
      [dispatch],
    )
    const runPostActionOption = React.useCallback(
      (index: number) => () => onSetPostActionChoice(index),
      [onSetPostActionChoice],
    )

    return (
      <>
        {postActionSessionChoices.map((choice, index) => {
          const isActive = choice.id === activePostActionChoice
          return (
            <FlexRow
              key={choice.id}
              onClick={runPostActionOption(index)}
              style={{
                paddingTop: 4,
                paddingBottom: 4,
                paddingLeft: 8,
                paddingRight: 8,
                borderRadius: 4,
                color: colorTheme.textColor.value,
                cursor: 'pointer',
                justifyContent: 'space-between',
                gap: 12,
              }}
              css={{
                '&:hover': {
                  backgroundColor: colorTheme.bg5.value,
                },
              }}
            >
              <FlexRow>
                <div
                  style={{
                    width: '8px',
                    marginRight: 8,
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'center',
                  }}
                >
                  {isActive ? '✓' : ' '}
                </div>
                <span>{choice.name}</span>
              </FlexRow>
              <ShortcutIndicator label={`${index + 1}`} />
            </FlexRow>
          )
        })}

        <div
          style={{
            height: 12,
            display: 'flex',
            flexDirection: 'column',
            justifyContent: 'center',
          }}
        >
          <div style={{ height: 1, width: '100%', background: colorTheme.border3.value }} />
        </div>

        <FlexRow
          key={'undo-option'}
          onClick={undoOption}
          style={{
            paddingTop: 4,
            paddingBottom: 4,
            paddingLeft: 8,
            paddingRight: 8,
            borderRadius: 4,
            color: colorTheme.textColor.value,
            cursor: 'pointer',
            justifyContent: 'space-between',
            gap: 12,
          }}
          css={{
            '&:hover': {
              backgroundColor: colorTheme.bg5.value,
            },
          }}
        >
          <FlexRow>
            <div
              style={{
                width: '8px',
                marginRight: 8,
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
              }}
            />
            <span>{'Undo'}</span>
          </FlexRow>
          <ShortcutIndicator label={`⌘Z`} />
        </FlexRow>
      </>
    )
  },
)
PostActionMenu.displayName = 'PostActionMenu'

const ShortcutIndicator = ({ label }: { label: string }) => {
  return (
    <div
      style={{
        marginRight: 5,
        borderRadius: 3,
        display: 'flex',
        flexDirection: 'row',
        justifyContent: 'center',
        alignItems: 'center',
      }}
    >
      {label}
    </div>
  )
}

export const FloatingPostActionMenu = React.memo(() => {
  const [open, setOpen] = React.useState<boolean>(false)

  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'PostActionMenu scale',
  )
  const positioningProps: React.CSSProperties = useEditorState(
    Substores.metadata,
    (store) => {
      const aabbs = mapDropNulls((path) => {
        const frame = MetadataUtils.getFrameInCanvasCoords(path, store.editor.jsxMetadata)
        return frame == null || isInfinityRectangle(frame) ? null : frame
      }, store.editor.selectedViews)

      const selectedElementBounds = boundingRectangleArray(aabbs) ?? zeroCanvasRect
      return {
        top: selectedElementBounds.y,
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

  const postActionSessionChoices = useEditorState(
    Substores.postActionInteractionSession,
    PostActionChoicesSelector,
    'PostActionMenu postActionSessionChoices',
  )

  React.useEffect(() => {
    if (!isPostActionMenuActive(postActionSessionChoices)) {
      setOpen(false)
    }
  }, [postActionSessionChoices])

  const dispatch = useDispatch()

  React.useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {
      const isDismissKey = event.key === 'Enter' || event.key === 'Escape'

      if (isDismissKey && isPostActionMenuActive(postActionSessionChoices)) {
        event.preventDefault()
        event.stopPropagation()
        event.stopImmediatePropagation()

        dispatch([clearPostActionData()])
      }
    }

    if (isPostActionMenuActive(postActionSessionChoices)) {
      window.addEventListener('keydown', handleKeyDown, true)
    }

    return function cleanup() {
      window.removeEventListener('keydown', handleKeyDown, true)
    }
  }, [dispatch, postActionSessionChoices])

  if (!isPostActionMenuActive(postActionSessionChoices)) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
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
            zoom: 1 / scale,
            display: 'flex',
            alignItems: 'stretch',
            padding: 4,
            borderRadius: 4,
            background: colorTheme.bg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            cursor: open ? undefined : 'pointer',
            fontSize: 10,
          }}
        >
          {open ? (
            <PostActionMenu postActionSessionChoices={postActionSessionChoices} />
          ) : (
            <Icn category='semantic' type='clipboard' color={'main'} width={18} height={18} />
          )}
        </FlexColumn>
      </div>
    </CanvasOffsetWrapper>
  )
})
FloatingPostActionMenu.displayName = 'FloatingPostActionMenu'

export const InspectorPostActionMenu = React.memo(() => {
  const postActionSessionChoices = useEditorState(
    Substores.postActionInteractionSession,
    PostActionChoicesSelector,
    'PostActionMenu postActionSessionChoices',
  )

  if (!isPostActionMenuActive(postActionSessionChoices)) {
    return null
  }

  return (
    <div
      style={{
        marginTop: 12,
        marginBottom: 12,
      }}
    >
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
          }}
        >
          <span>Paste options</span>
        </FlexRow>
      </InspectorSubsectionHeader>
      <PostActionMenu postActionSessionChoices={postActionSessionChoices} />
    </div>
  )
})
InspectorPostActionMenu.displayName = 'FloatingPostActionMenu'
