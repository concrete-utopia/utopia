import React from 'react'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { FlexColumn, UtopiaStyles, useColorTheme } from '../../../../uuiui'

export const StaticReparentStrategyPicker = React.memo(() => {
  const isPasteSessionInProgress = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.interactionSession?.interactionData.type === 'STATIC_REPARENT',
    'StaticReparentStrategyPicker isPasteSessionInProgress',
  )

  const colorTheme = useColorTheme()

  if (!isPasteSessionInProgress) {
    return null
  }

  return (
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
          display: 'flex',
          alignItems: 'stretch',
          padding: 4,
          gap: 4,
          borderRadius: 4,
          background: colorTheme.bg0.value,
          boxShadow: UtopiaStyles.popup.boxShadow,
        }}
      >
        Paste session underway!
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
    </div>
  )
})
