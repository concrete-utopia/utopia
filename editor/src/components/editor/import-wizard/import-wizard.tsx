/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { getProjectID } from '../../../common/env-vars'
import { Button, FlexRow, H2, Icons, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { useEditorState, Substores } from '../store/store-hook'
import { when } from '../../../utils/react-conditionals'
import { hideImportWizard } from '../../../core/shared/import/import-operation-service'
import { OperationLine } from './components'

export const ImportWizard = React.memo(() => {
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
    hideImportWizard()
  }, [])

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
        pointerEvents: importWizardOpen ? 'all' : 'none',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        backgroundColor: importWizardOpen ? '#00000033' : 'transparent',
      }}
    >
      {when(
        importWizardOpen,
        <div
          style={{
            background: colorTheme.bg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            borderRadius: 10,
            width: 600,
            height: 500,
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            fontSize: '14px',
            lineHeight: 'normal',
            letterSpacing: 'normal',
            padding: 20,
            overflow: 'hidden',
          }}
          onClick={stopPropagation}
        >
          <FlexRow
            className='import-wizard-header'
            css={{
              justifyContent: 'space-between',
              width: '100%',
            }}
          >
            <div css={{ fontSize: 16, fontWeight: 400 }}>Project Import</div>
            <Button
              highlight
              style={{
                width: 22,
                height: 22,
              }}
              onClick={handleDismiss}
            >
              <Icons.Cross />
            </Button>
          </FlexRow>
          <div
            className='import-wizard-body'
            style={{
              display: 'flex',
              flexDirection: 'column',
              gap: 15,
              overflow: 'scroll',
              height: '100%',
              width: '100%',
              // padding: 20,
              marginTop: 20,
            }}
          >
            {operations.map((operation) => (
              <OperationLine key={operation.id ?? operation.type} operation={operation} />
            ))}
          </div>
          <div
            className='import-wizard-footer'
            css={{
              display: 'flex',
              justifyContent: 'flex-end',
              width: '100%',
              marginTop: 20,
            }}
          ></div>
        </div>,
      )}
    </div>
  )
})
ImportWizard.displayName = 'ImportWizard'
