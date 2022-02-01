import * as React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexRow, FlexColumn, useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { useGetApplicableStrategiesOrderedByFitness } from '../../canvas-strategies/canvas-strategies'

export const CanvasStrategyIndicator = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useEditorState((store) => store.dispatch, 'CanvasStrategyIndicator dispatch')
  const activeStrategy = useEditorState(
    (store) => store.sessionStateState.currentStrategy,
    'CanvasStrategyIndicator sessionStateState.currentStrategy',
  )
  const otherPossibleStrategies = useGetApplicableStrategiesOrderedByFitness()

  const onTabPressed = React.useCallback(
    (newStrategyName: string) => {
      dispatch([CanvasActions.setUsersPreferredStrategy(newStrategyName)])
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
        event.stopImmediatePropagation()
        event.stopPropagation()
        event.preventDefault()

        const activeStrategyIndex = otherPossibleStrategies.findIndex(
          (strategyName: string) => strategyName === activeStrategy,
        )
        const nextStrategyIndex = (activeStrategyIndex + 1) % otherPossibleStrategies.length
        const nextStrategyName = otherPossibleStrategies[nextStrategyIndex]

        onTabPressed(nextStrategyName)
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
                  key={strategy}
                  style={{
                    height: 29,
                    paddingLeft: 4,
                    paddingRight: 4,
                    backgroundColor:
                      strategy === activeStrategy ? colorTheme.primary.value : undefined,
                    color:
                      strategy === activeStrategy
                        ? colorTheme.white.value
                        : colorTheme.textColor.value,
                  }}
                >
                  {strategy}
                </FlexRow>
              )
            })}
          </FlexColumn>
        </div>,
      )}
    </>
  )
})
