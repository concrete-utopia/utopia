import * as React from 'react'
import { FlexColumn, FlexRow, UtopiaStyles, colorTheme } from '../../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
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
import { mod } from '../../../../core/shared/math-utils'

const isPostActionMenuActive = (postActionSessionChoices: PostActionChoice[]) =>
  postActionSessionChoices.length > 1

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
    (choice: PostActionChoice) => {
      dispatch([executePostActionMenuChoice(choice)])
    },
    [dispatch],
  )

  React.useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {
      const keyIntValue = Number.parseInt(event.key)
      const isStrategySwitchingKey = !isNaN(keyIntValue) || event.key === 'Tab'

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

          const nextChoiceIndex = mod(newStrategyIndex, postActionSessionChoices.length)
          const nextChoice = postActionSessionChoices[nextChoiceIndex]

          onSetPostActionChoice(nextChoice)
          return
        }
        if (!isNaN(keyIntValue)) {
          const index = keyIntValue - 1
          const nextPostActionChoice = postActionSessionChoices.at(index)
          if (nextPostActionChoice != null) {
            onSetPostActionChoice(nextPostActionChoice)
          }
        }
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

  if (!isPostActionMenuActive(postActionSessionChoices)) {
    return null
  }

  return (
    <div
      style={{
        pointerEvents: 'initial',
        position: 'absolute',
        top: 4,
        right: 4,
        fontSize: 9,
      }}
      onMouseDown={stopPropagation}
      onClick={stopPropagation}
    >
      <FlexColumn
        style={{
          minHeight: 84,
          display: 'flex',
          alignItems: 'stretch',
          padding: 4,
          gap: 4,
          borderRadius: 4,
          border: `1px solid ${colorTheme.navigatorResizeHintBorder.value}`,
          background: colorTheme.bg0.value,
          boxShadow: UtopiaStyles.popup.boxShadow,
        }}
      >
        {postActionSessionChoices.map((choice, index) => {
          return (
            <FlexRow
              key={choice.id}
              style={{
                height: 19,
                paddingLeft: 4,
                paddingRight: 4,
                backgroundColor:
                  choice.id === activePostActionChoice ? colorTheme.bg5.value : undefined,
                color: colorTheme.textColor.value,
              }}
            >
              <KeyIndicator keyNumber={index + 1} />
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
            style={{ padding: 2, borderRadius: 2, border: `1px solid ${colorTheme.fg8.value}` }}
          >
            Tab
          </span>{' '}
          to switch
        </div>
      </FlexColumn>
    </div>
  )
})
PostActionMenu.displayName = 'PostActionMenu'

const KeyIndicator = ({ keyNumber }: { keyNumber: number }) => {
  const height = 12
  const width = 12
  return (
    <div
      style={{
        width: width,
        height: height,
        marginRight: 5,
        border: `1px solid ${colorTheme.fg4.value}`,
        borderRadius: 3,
        display: 'flex',
        flexDirection: 'row',
        justifyContent: 'center',
        alignItems: 'center',
      }}
    >
      <span
        style={{
          fontWeight: 700,
          color: colorTheme.fg4.value,
          fontSize: '8px',
        }}
      >
        {keyNumber}
      </span>
    </div>
  )
}
