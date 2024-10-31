/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { getProjectID } from '../../../common/env-vars'
import { Button, FlexRow, Icons, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { useEditorState, Substores } from '../store/store-hook'
import { when } from '../../../utils/react-conditionals'
import { hideImportWizard } from '../../../core/shared/import/import-operation-service'
import { OperationLine } from './components'
import { ImportOperationResult } from '../../../core/shared/import/import-operation-types'
import { assertNever } from '../../../core/shared/utils'
import { useDispatch } from '../store/dispatch-context'

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

  const dispatch = useDispatch()

  const handleDismiss = React.useCallback(() => {
    hideImportWizard(dispatch)
  }, [dispatch])

  const stopPropagation = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
  }, [])

  const totalImportResult: ImportOperationResult | null = React.useMemo(() => {
    let result: ImportOperationResult = ImportOperationResult.Success
    for (const operation of operations) {
      // if one of the operations is still running, we don't know the total result yet
      if (operation.timeDone == null || operation.result == null) {
        return null
      }
      // if any operation is an error, the total result is an error
      if (operation.result == ImportOperationResult.Error) {
        return ImportOperationResult.Error
      }
      // if any operation is at least a warn, the total result is a warn,
      // but we also need to check if there are any errors
      if (operation.result == ImportOperationResult.Warn) {
        result = ImportOperationResult.Warn
      }
    }
    return result
  }, [operations])

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
            color: colorTheme.fg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            borderRadius: 10,
            width: 600,
            height: 420,
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
            <div css={{ fontSize: 16, fontWeight: 400 }}>Loading Project</div>
            {when(
              totalImportResult == null,
              <Button
                highlight
                style={{
                  padding: 15,
                  color: colorTheme.fg6.value,
                }}
                onClick={handleDismiss}
              >
                Cancel
              </Button>,
            )}
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
              justifyContent: 'space-between',
              alignItems: 'center',
              width: '100%',
              marginTop: 20,
            }}
          >
            <ActionButtons importResult={totalImportResult} />
          </div>
        </div>,
      )}
    </div>
  )
})
ImportWizard.displayName = 'ImportWizard'

function ActionButtons({ importResult }: { importResult: ImportOperationResult | null }) {
  const colorTheme = useColorTheme()
  const textColor = React.useMemo(() => {
    switch (importResult) {
      case ImportOperationResult.Success:
        return 'green'
      case ImportOperationResult.Warn:
        return 'orange'
      case ImportOperationResult.Error:
        return 'var(--utopitheme-githubIndicatorFailed)'
      case null:
        return 'black'
      default:
        assertNever(importResult)
    }
  }, [importResult])
  const buttonColor = React.useMemo(() => {
    switch (importResult) {
      case ImportOperationResult.Success:
        return 'var(--utopitheme-green)'
      case ImportOperationResult.Warn:
        return 'var(--utopitheme-githubMUDModified)'
      case ImportOperationResult.Error:
        return 'var(--utopitheme-githubIndicatorFailed)'
      case null:
        return 'black'
      default:
        assertNever(importResult)
    }
  }, [importResult])
  const textStyle = {
    color: textColor,
    fontSize: 14,
  }
  const buttonStyle = {
    backgroundColor: colorTheme.buttonBackground.value,
    padding: 20,
    fontSize: 14,
    cursor: 'pointer',
  }
  const dispatch = useDispatch()
  const hideWizard = React.useCallback(() => {
    hideImportWizard(dispatch)
  }, [dispatch])
  if (importResult == ImportOperationResult.Success) {
    return (
      <React.Fragment>
        <div style={textStyle}>Project Imported Successfully</div>
        <Button onClick={hideWizard} style={buttonStyle}>
          Continue To Editor
        </Button>
      </React.Fragment>
    )
  }
  if (importResult == ImportOperationResult.Warn) {
    return (
      <React.Fragment>
        <div style={textStyle}>Project Imported With Warnings</div>
        <Button onClick={hideWizard} style={buttonStyle}>
          Continue
        </Button>
      </React.Fragment>
    )
  }
  if (importResult == ImportOperationResult.Error) {
    return (
      <React.Fragment>
        <div style={textStyle}>Error Importing Project</div>
        <Button style={buttonStyle}>Import A Different Project</Button>
      </React.Fragment>
    )
  }
  return null
}
