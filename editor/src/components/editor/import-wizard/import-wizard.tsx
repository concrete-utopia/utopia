import React from 'react'
import { getProjectID } from '../../../common/env-vars'
import { Button, Icons, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { useDispatch } from '../store/dispatch-context'
import { useEditorState, Substores } from '../store/store-hook'
import { when } from '../../../utils/react-conditionals'
import { hideImportWizard } from './import-wizard-service'

export const ImportWizard = React.memo(() => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const projectId = getProjectID()

  const importWizardOpen: boolean = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.importWizardOpen,
    'ImportWizard importWizardOpen',
  )

  const operations = useEditorState(
    Substores.github,
    (store) => store.editor.importOperations,
    'ImportWizard operations',
  )

  const handleDismiss = React.useCallback(() => {
    hideImportWizard(dispatch)
  }, [dispatch])

  const stopPropagation = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
  }, [])

  if (projectId == null) {
    return null
  }

  return (
    <div
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        bottom: 0,
        right: 0,
        pointerEvents: !importWizardOpen ? 'none' : 'all',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        backgroundColor: !importWizardOpen ? 'transparent' : '#00000033',
      }}
      onClick={handleDismiss}
    >
      {when(
        importWizardOpen,
        <div
          style={{
            background: colorTheme.bg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            borderRadius: 10,
            width: '500px',
            height: '500px',
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            justifyContent: 'center',
          }}
          onClick={stopPropagation}
        >
          <Button
            highlight
            style={{
              position: 'absolute',
              top: 14,
              right: 14,
              width: 22,
              height: 22,
            }}
            onClick={handleDismiss}
          >
            <Icons.Cross />
          </Button>
          <div>{JSON.stringify(operations)}</div>
        </div>,
      )}
    </div>
  )
})
ImportWizard.displayName = 'ImportWizard'
