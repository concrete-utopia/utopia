import * as React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexRow, useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'

export const CanvasStrategyIndicator = React.memo(() => {
  const colorTheme = useColorTheme()
  const activeStrategy = null // TODO actually use an active strategy here

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
          <FlexRow
            style={{
              height: 29,
              display: 'flex',
              alignItems: 'center',
              padding: 4,
              gap: 4,
              borderRadius: 4,
              background: colorTheme.bg0.value,
              boxShadow: UtopiaStyles.popup.boxShadow,
            }}
          >
            {activeStrategy}
          </FlexRow>
        </div>,
      )}
    </>
  )
})
