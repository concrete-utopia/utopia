/** @jsxRuntime classic */
/** @jsx jsx */
/* @jsxFrag */
import React from 'react'
import { jsx } from '@emotion/react'
import { FlexColumn, FlexRow, colorTheme } from '../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import {
  PostActionChoice,
  generatePostactionChoices as generatePostActionChoices,
} from '../../canvas/canvas-strategies/post-action-options/post-action-options'
import { useDispatch } from '../../editor/store/dispatch-context'
import {
  clearPostActionData,
  executePostActionMenuChoice,
  undo,
} from '../../editor/actions/action-creators'
import { mod } from '../../../core/shared/math-utils'

const isPostActionMenuActive = (postActionSessionChoices: PostActionChoice[]) =>
  postActionSessionChoices.length > 0

export const PostActionMenu = React.memo(() => {
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

  const undoOption = React.useCallback(() => dispatch([clearPostActionData(), undo()]), [dispatch])
  const runPostActionOption = React.useCallback(
    (index: number) => () => onSetPostActionChoice(index),
    [onSetPostActionChoice],
  )

  if (!isPostActionMenuActive(postActionSessionChoices)) {
    return null
  }

  return (
    <FlexColumn
      style={{
        display: 'flex',
        alignItems: 'stretch',
        padding: 4,
        background: colorTheme.bg0.value,
        fontSize: 10,
        marginBottom: 4,
      }}
    >
      <div style={{ fontSize: 12, padding: '8px 4px', fontWeight: 800 }}>Paste options</div>
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
              color: isActive ? colorTheme.white.value : colorTheme.textColor.value,
              backgroundColor: isActive ? colorTheme.primary.value : undefined,
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
            <span>{choice.name}</span>
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
        <span>{'Undo'}</span>
        <ShortcutIndicator label={`⌘Z`} />
      </FlexRow>
      <div
        style={{
          alignSelf: 'center',
          marginTop: 6,
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
    </FlexColumn>
  )
})
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
