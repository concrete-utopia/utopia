import * as React from 'react'
import { mod } from '../../../../core/shared/math-utils'
import { when } from '../../../../utils/react-conditionals'
import { FlexRow, FlexColumn, UtopiaStyles, colorTheme, UtopiaTheme } from '../../../../uuiui'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { stopPropagation } from '../../../inspector/common/inspector-utils'
import CanvasActions from '../../canvas-actions'
import { useDelayedCurrentStrategy } from '../../canvas-strategies/canvas-strategies'
import type { CanvasStrategy } from '../../canvas-strategies/canvas-strategy-types'

export const CanvasStrategyPicker = React.memo(() => {
  const dispatch = useDispatch()
  const { allApplicableStrategies } = useEditorState(
    Substores.restOfStore,
    (store) => ({
      allApplicableStrategies: store.strategyState.sortedApplicableStrategies,
    }),
    'CanvasStrategyPicker strategyState.currentStrategy',
  )
  const activeStrategy = useDelayedCurrentStrategy()
  const isStrategyFailure = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState?.status === 'failure',
    'Strategy failure',
  )

  const onSetStrategy = React.useCallback(
    (newStrategy: CanvasStrategy) => {
      dispatch([CanvasActions.setUsersPreferredStrategy(newStrategy.id)])
    },
    [dispatch],
  )

  React.useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {
      const keyIntValue = Number.parseInt(event.key)
      const isStrategySwitchingKey = event.key === 'Tab' || !isNaN(keyIntValue)
      if (
        isStrategySwitchingKey &&
        activeStrategy != null &&
        allApplicableStrategies != null &&
        allApplicableStrategies.length > 0
      ) {
        event.preventDefault()
        event.stopPropagation()
        event.stopImmediatePropagation()

        if (event.key === 'Tab') {
          const activeStrategyIndex = allApplicableStrategies.findIndex(
            ({ strategy }) => strategy.id === activeStrategy,
          )

          const newStrategyIndex = event.shiftKey
            ? activeStrategyIndex - 1
            : activeStrategyIndex + 1

          const nextStrategyIndex = mod(newStrategyIndex, allApplicableStrategies.length)
          const nextStrategy = allApplicableStrategies[nextStrategyIndex].strategy

          onSetStrategy(nextStrategy)
        } else if (!isNaN(keyIntValue)) {
          const index = keyIntValue - 1
          const nextStrategy = allApplicableStrategies[index]
          if (nextStrategy != null) {
            onSetStrategy(nextStrategy.strategy)
          }
        }
      }
    }
    window.addEventListener('keydown', handleKeyDown, true)
    return function cleanup() {
      window.removeEventListener('keydown', handleKeyDown, true)
    }
  }, [onSetStrategy, activeStrategy, allApplicableStrategies])

  return (
    <>
      {when(
        activeStrategy != null,
        <div
          style={{
            pointerEvents: 'initial',
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
              background: colorTheme.bg1.value,
              borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
              boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
            }}
          >
            {allApplicableStrategies?.map(({ strategy, name }, index) => {
              return (
                <FlexRow
                  key={strategy.id}
                  style={{
                    height: 19,
                    paddingLeft: 4,
                    paddingRight: 4,
                    borderRadius: 6,
                    backgroundColor:
                      strategy.id === activeStrategy ? colorTheme.bg3.value : undefined,
                    color: colorTheme.textColor.value,
                    opacity: isStrategyFailure && strategy.id === activeStrategy ? 0.5 : 1,
                  }}
                >
                  <KeyIndicator keyNumber={index + 1} />
                  <span
                    data-testid={
                      strategy.id === activeStrategy ? 'strategy-picker-active-row' : undefined
                    }
                  >
                    {name}
                  </span>
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
