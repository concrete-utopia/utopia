import * as React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexRow, FlexColumn, useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { useEditorDispatch, useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { CanvasStrategy } from '../../canvas-strategies/canvas-strategy-types'

const strategiesSelector = (store: EditorStorePatched) => ({
  activeStrategy: store.strategyState.currentStrategy,
  otherPossibleStrategies: store.strategyState.sortedApplicableStrategies,
})

export const CanvasStrategyIndicator = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useEditorDispatch('CanvasStrategyIndicator dispatch')
  const { activeStrategy, otherPossibleStrategies } = useEditorState(
    strategiesSelector,
    'CanvasStrategyIndicator strategyState.currentStrategy',
  )

  const onTabPressed = React.useCallback(
    (newStrategy: CanvasStrategy) => {
      dispatch([CanvasActions.setUsersPreferredStrategy(newStrategy.id)])
    },
    [dispatch],
  )

  React.useEffect(() => {
    function handleTabKey(event: KeyboardEvent) {
      if (
        event.key === 'Tab' &&
        activeStrategy != null &&
        otherPossibleStrategies != null &&
        otherPossibleStrategies.length > 0
      ) {
        event.preventDefault()
        event.stopPropagation()
        event.stopImmediatePropagation()

        const activeStrategyIndex = otherPossibleStrategies.findIndex(
          (strategy: CanvasStrategy) => strategy.id === activeStrategy,
        )
        const nextStrategyIndex = (activeStrategyIndex + 1) % otherPossibleStrategies.length
        const nextStrategy = otherPossibleStrategies[nextStrategyIndex]

        onTabPressed(nextStrategy)
      }
    }
    window.addEventListener('keydown', handleTabKey, true)
    return function cleanup() {
      window.removeEventListener('keydown', handleTabKey, true)
    }
  }, [onTabPressed, activeStrategy, otherPossibleStrategies])

  return (
    <>
      {when(
        activeStrategy != null,
        <div
          style={{
            pointerEvents: 'initial',
            position: 'absolute',
            bottom: 4,
            left: 4,
          }}
        >
          <FlexColumn
            style={{
              minHeight: 29,
              display: 'flex',
              alignItems: 'stretch',
              padding: 4,
              gap: 4,
              borderRadius: 4,
              background: colorTheme.bg0.value,
              boxShadow: UtopiaStyles.popup.boxShadow,
            }}
          >
            {otherPossibleStrategies?.map((strategy) => {
              return (
                <FlexRow
                  key={strategy.id}
                  style={{
                    height: 29,
                    paddingLeft: 4,
                    paddingRight: 4,
                    backgroundColor:
                      strategy.id === activeStrategy ? colorTheme.primary.value : undefined,
                    color:
                      strategy.id === activeStrategy
                        ? colorTheme.white.value
                        : colorTheme.textColor.value,
                  }}
                >
                  {strategy.name}
                </FlexRow>
              )
            })}
          </FlexColumn>
        </div>,
      )}
    </>
  )
})
