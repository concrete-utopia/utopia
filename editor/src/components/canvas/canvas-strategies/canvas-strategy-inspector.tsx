import React from 'react'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { isStrategyActive } from './canvas-strategies'

export const CanvasStrategyInspector = React.memo(() => {
  const colorTheme = useColorTheme()
  const activeStrategy = useEditorState(
    (store) => isStrategyActive(store.strategyState),
    'CanvasStrategyInspector activeStrategy',
  )
  const commandsToList = useEditorState(
    (store) => store.strategyState.commandDescriptions,
    'CanvasStrategyInspector accumulatedCommands',
  )

  if (!activeStrategy) {
    return null
  } else {
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
  }
})
