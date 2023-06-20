import * as React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexColumn, FlexRow, UtopiaStyles, colorTheme } from '../../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { stopPropagation } from '../../../inspector/common/inspector-utils'
import {
  PostActionChoice,
  generatePostactionChoices as generatePostActionChoices,
} from '../../canvas-strategies/post-action-options/post-action-options'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { executeCommandsWithPostActionMenu, undo } from '../../../editor/actions/action-creators'

export const PostActionMenu = React.memo(() => {
  const postActionSessionChoices = useEditorState(
    Substores.restOfEditor,
    (store) =>
      store.editor.postActionInteractionData?.type == null
        ? null
        : generatePostActionChoices(store.editor.postActionInteractionData),
    'post action on',
  )

  const editorStateRef = useRefEditorState((store) => store.editor)
  const builtInDependenciesRef = useRefEditorState((store) => store.builtInDependencies)

  const dispatch = useDispatch()

  const postActionInteractionDataRef = useRefEditorState(
    (store) => store.editor.postActionInteractionData,
  )

  const onSetPostActionChoice = React.useCallback(
    (choice: PostActionChoice) => {
      const commands = choice.run(editorStateRef.current, builtInDependenciesRef.current)
      if (commands == null || postActionInteractionDataRef.current == null) {
        return
      }

      dispatch([
        undo(),
        executeCommandsWithPostActionMenu(commands, postActionInteractionDataRef.current),
      ])
    },
    [builtInDependenciesRef, dispatch, editorStateRef, postActionInteractionDataRef],
  )

  React.useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {
      const keyIntValue = Number.parseInt(event.key)
      const isStrategySwitchingKey = !isNaN(keyIntValue) // || event.key === 'Tab'

      if (
        isStrategySwitchingKey &&
        postActionSessionChoices != null &&
        postActionSessionChoices.length > 0
      ) {
        event.preventDefault()
        event.stopPropagation()
        event.stopImmediatePropagation()

        // if (event.key === 'Tab') {
        //   const activeStrategyIndex = allApplicableStrategies.findIndex(
        //     ({ strategy }) => strategy.id === activeStrategy,
        //   )

        //   const newStrategyIndex = event.shiftKey
        //     ? activeStrategyIndex - 1
        //     : activeStrategyIndex + 1

        //   const nextStrategyIndex = mod(newStrategyIndex, allApplicableStrategies.length)
        //   const nextStrategy = allApplicableStrategies[nextStrategyIndex].strategy

        //   onSetPostActionChoice(nextStrategy)
        //   return
        // }
        if (!isNaN(keyIntValue)) {
          const index = keyIntValue - 1
          const nextPostActionChoice = postActionSessionChoices.at(index)
          if (nextPostActionChoice != null) {
            onSetPostActionChoice(nextPostActionChoice)
          }
        }
      }
    }

    window.addEventListener('keydown', handleKeyDown, true)
    return function cleanup() {
      window.removeEventListener('keydown', handleKeyDown, true)
    }
  }, [onSetPostActionChoice, postActionSessionChoices])

  return (
    <>
      {when(
        postActionSessionChoices != null,
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
              background: colorTheme.bg0.value,
              boxShadow: UtopiaStyles.popup.boxShadow,
            }}
          >
            {postActionSessionChoices?.map((choice, index) => {
              return (
                <FlexRow
                  key={choice.name}
                  style={{
                    height: 19,
                    paddingLeft: 4,
                    paddingRight: 4,
                    backgroundColor: undefined,
                    // choice.id === activeStrategy ? colorTheme.bg5.value : undefined,
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
        </div>,
      )}
    </>
  )
})

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
