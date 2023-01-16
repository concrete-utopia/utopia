import React from 'react'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { when } from '../../../utils/react-conditionals'
import { useColorTheme } from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { isStrategyActive } from './canvas-strategies'

export const CanvasStrategyInspector = React.memo(() => {
  const colorTheme = useColorTheme()
  const activeStrategy = useEditorState(
    Substores.restOfStore,
    (store) => isStrategyActive(store.strategyState),
    'CanvasStrategyInspector activeStrategy',
  )
  const commandsToList = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState.commandDescriptions,
    'CanvasStrategyInspector accumulatedCommands',
  )

  if (activeStrategy && isFeatureEnabled('Canvas Strategies Debug Panel')) {
    return (
      <div
        style={{
          position: 'absolute',
          display: 'flex',
          flexDirection: 'column',
          top: 0,
          width: '100%',
          height: '100%',
          backgroundColor: colorTheme.inspectorBackground.value,
          color: 'black',
          zIndex: 10,
        }}
      >
        The following Commands are being applied:
        {commandsToList.map((c, i) => (
          <span
            key={`${i}-${c.description}`}
            style={{
              whiteSpace: 'normal',
              fontStyle: c.transient ? 'italic' : 'normal',
              fontWeight: c.transient ? undefined : 'bold',
            }}
          >
            • {c.description}
          </span>
        ))}
      </div>
    )
  } else {
    return null
  }
})
