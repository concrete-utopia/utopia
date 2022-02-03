import React from 'react'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'

export const CanvasStrategyInspector = React.memo(() => {
  const colorTheme = useColorTheme()
  const accumulatedCommands = useEditorState(
    (store) => store.sessionStateState.accumulatedCommands,
    'CanvasStrategyInspector accumulatedCommands',
  )
  const currentStrategyCommands = useEditorState(
    (store) => store.sessionStateState.currentStrategyCommands,
    'CanvasStrategyInspector currentStrategyCommands',
  )
  const commandsToList = [...accumulatedCommands, ...currentStrategyCommands]
  if (commandsToList.length === 0) {
    return null
  } else {
    return (
      <div
        style={{
          position: 'absolute',
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
          <div key={`${i}-${c.type}`}>{c.type}</div>
        ))}
      </div>
    )
  }
})
