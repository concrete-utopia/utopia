import * as React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexRow, FlexColumn, useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { useDelayedCurrentStrategy } from '../../canvas-strategies/canvas-strategies'
import { CanvasStrategy } from '../../canvas-strategies/canvas-strategy-types'

export const CanvasStrategyPicker = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useEditorState((store) => store.dispatch, 'CanvasStrategyPicker dispatch')
  const { otherPossibleStrategies } = useEditorState(
    (store) => ({
      otherPossibleStrategies: store.strategyState.sortedApplicableStrategies,
    }),
    'CanvasStrategyPicker strategyState.currentStrategy',
  )
  const activeStrategy = useDelayedCurrentStrategy()

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
            top: 4,
            right: 4,
            fontSize: 9,
          }}
        >
          <FlexColumn
            style={{
              minHeight: 84,
              width: 120,
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
                    height: 19,
                    paddingLeft: 4,
                    paddingRight: 4,
                    backgroundColor:
                      strategy.id === activeStrategy ? colorTheme.bg5.value : undefined,
                    color: colorTheme.textColor.value,
                  }}
                >
                  {strategy.name}
                </FlexRow>
              )
            })}
            <div
              style={{
                alignSelf: 'center',
                marginTop: 'auto',
                color: colorTheme.fg8.value,
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
