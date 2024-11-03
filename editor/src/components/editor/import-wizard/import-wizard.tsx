/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { getProjectID } from '../../../common/env-vars'
import { Button, FlexRow, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { useEditorState, Substores } from '../store/store-hook'
import { unless, when } from '../../../utils/react-conditionals'
import {
  hideImportWizard,
  updateImportOperationResult,
} from '../../../core/shared/import/import-operation-service'
import { OperationLine } from './components'
import type { ImportOperation } from '../../../core/shared/import/import-operation-types'
import {
  ImportOperationAction,
  ImportOperationResult,
} from '../../../core/shared/import/import-operation-types'
import { assertNever } from '../../../core/shared/utils'
import { useDispatch } from '../store/dispatch-context'
import { updateImportOperations } from '../actions/action-creators'

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
    // if any operation is an error, the total result is immediately an error
    for (const operation of operations) {
      if (
        operation.result == ImportOperationResult.CriticalError ||
        operation.result == ImportOperationResult.Error
      ) {
        return operation.result
      }
    }
    // if any operation is still running, we don't show the total result yet
    if (operations.some((op) => op.timeDone == null || op.result == null)) {
      return null
    }
    // if any operation is a warning, the total result is a warning
    if (operations.some((op) => op.result == ImportOperationResult.Warn)) {
      return ImportOperationResult.Warn
    }
    return ImportOperationResult.Success
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
            fontSize: '13px',
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
              gap: 10,
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
  const operations = useEditorState(
    Substores.github,
    (store) => store.editor.importOperations,
    'ImportWizard operations',
  )
  const dispatch = useDispatch()
  const textColor = React.useMemo(() => {
    switch (importResult) {
      case ImportOperationResult.Success:
        return colorTheme.green.value
      case ImportOperationResult.Warn:
        return colorTheme.warningOrange.value
      case ImportOperationResult.Error:
        return colorTheme.error.value
      case ImportOperationResult.CriticalError:
        return colorTheme.error.value
      case null:
        return colorTheme.fg0.value
      default:
        assertNever(importResult)
    }
  }, [colorTheme, importResult])
  const continueAnyway = React.useCallback(() => {
    const operationsToUpdate: ImportOperation[] = operations.map((op) =>
      updateImportOperationResult(op, (operation) =>
        operation.result === ImportOperationResult.Error
          ? ImportOperationResult.Warn
          : operation.result,
      ),
    )
    dispatch([updateImportOperations(operationsToUpdate, ImportOperationAction.Update)])
  }, [dispatch, operations])
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
  if (
    importResult == ImportOperationResult.Error ||
    importResult == ImportOperationResult.CriticalError
  ) {
    return (
      <React.Fragment>
        <div style={textStyle}>Error Importing Project</div>
        <Button style={{ ...buttonStyle, marginLeft: 'auto' }}>Import A Different Project</Button>
        {unless(
          importResult == ImportOperationResult.CriticalError,
          <Button
            style={{
              cursor: 'pointer',
            }}
            onClick={continueAnyway}
          >
            Continue Anyway
          </Button>,
        )}
      </React.Fragment>
    )
  }
  return null
}
