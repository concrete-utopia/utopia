import React from 'react'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'

export const CanvasStrategyInspector = React.memo(() => {
  const colorTheme = useColorTheme()
  const activeInteractionState = useEditorState(
    (store) => store.editor.canvas.interactionState != null,
    'CanvasStrategyInspector activeInteractionState',
  )
  const commandsToList = useEditorState(
    (store) => store.sessionStateState.commandDescriptions,
    'CanvasStrategyInspector accumulatedCommands',
  )

  return null
  if (!activeInteractionState) {
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
