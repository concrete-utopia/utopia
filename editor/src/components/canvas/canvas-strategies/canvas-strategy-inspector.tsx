import React from 'react'
import { when } from '../../../utils/react-conditionals'
import { useColorTheme } from '../../../uuiui'
import { EditorStorePatched } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { isStrategyActive } from './canvas-strategies'

const isStrategyActiveSelector = (store: EditorStorePatched) =>
  isStrategyActive(store.strategyState)
const commandDescriptionsSelector = (store: EditorStorePatched) =>
  store.strategyState.commandDescriptions

export const CanvasStrategyInspector = React.memo(() => {
  const colorTheme = useColorTheme()
  const activeStrategy = useEditorState(
    isStrategyActiveSelector,
    'CanvasStrategyInspector activeStrategy',
  )
  const commandsToList = useEditorState(
    commandDescriptionsSelector,
    'CanvasStrategyInspector accumulatedCommands',
  )

  if (activeStrategy) {
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
