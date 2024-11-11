/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { getProjectID } from '../../../common/env-vars'
import { Button, FlexRow, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { useEditorState, Substores } from '../store/store-hook'
import { unless, when } from '../../../utils/react-conditionals'
import {
  getTotalImportStatusAndResult,
  hideImportWizard,
  updateProjectImportStatus,
} from '../../../core/shared/import/import-operation-service'
import { OperationLine } from './components'
import type { TotalImportResult } from '../../../core/shared/import/import-operation-types'
import { ImportOperationResult } from '../../../core/shared/import/import-operation-types'
import { assertNever } from '../../../core/shared/utils'
import { useDispatch } from '../store/dispatch-context'
import {
  setImportWizardOpen,
  setLeftMenuTab,
  updateGithubSettings,
} from '../actions/action-creators'
import { emptyGithubSettings, LeftMenuTab } from '../store/editor-state'

export const ImportWizard = React.memo(() => {
  const colorTheme = useColorTheme()
  const projectId = getProjectID()

  const importWizardOpen: boolean = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.importWizardOpen,
    'ImportWizard importWizardOpen',
  )

  const importState = useEditorState(
    Substores.github,
    (store) => store.editor.importState,
    'ImportWizard importState',
  )

  const operations = importState.importOperations

  const stopPropagation = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
  }, [])

  const totalImportResult: TotalImportResult = React.useMemo(
    () => getTotalImportStatusAndResult(importState),
    [importState],
  )

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
            height: 480,
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
              height: '30px',
              flex: 'none',
            }}
          >
            <div css={{ fontSize: 16, fontWeight: 400 }}>Cloning Project</div>
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

function ActionButtons({ importResult }: { importResult: TotalImportResult }) {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()
  const result = importResult.result
  const textColor = React.useMemo(() => {
    switch (result) {
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
        assertNever(result)
    }
  }, [colorTheme, result])
  const hideWizard = React.useCallback(() => {
    hideImportWizard(dispatch)
  }, [dispatch])
  const continueAnyway = React.useCallback(() => {
    if (importResult.importStatus.status === 'done') {
      hideWizard()
    }
    if (importResult.importStatus.status === 'paused') {
      updateProjectImportStatus(dispatch, {
        status: 'in-progress',
      })
      importResult.importStatus.onResume()
    }
  }, [dispatch, hideWizard, importResult.importStatus])
  const importADifferentProject = React.useCallback(() => {
    dispatch(
      [
        setImportWizardOpen(false),
        setLeftMenuTab(LeftMenuTab.Github),
        updateGithubSettings(emptyGithubSettings()),
      ],
      'everyone',
    )
  }, [dispatch])
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
  React.useEffect(() => {
    if (
      importResult.importStatus.status == 'done' &&
      importResult.result == ImportOperationResult.Success
    ) {
      hideWizard()
    }
  }, [importResult, hideWizard])
  if (
    importResult.importStatus.status === 'in-progress' ||
    importResult.importStatus.status === 'not-started'
  ) {
    return null
  }
  switch (importResult.result) {
    case ImportOperationResult.Success:
      return (
        <React.Fragment>
          <div style={textStyle}>Project Imported Successfully</div>
          <Button onClick={hideWizard} style={buttonStyle}>
            Continue To Editor
          </Button>
        </React.Fragment>
      )
    case ImportOperationResult.Warn:
      return (
        <React.Fragment>
          <div style={textStyle}>Project Imported With Warnings</div>
          <Button onClick={hideWizard} style={buttonStyle}>
            Continue To Editor
          </Button>
        </React.Fragment>
      )
    case ImportOperationResult.CriticalError:
      return (
        <React.Fragment>
          <div style={textStyle}>Error Importing Project</div>
          <Button style={{ ...buttonStyle, marginLeft: 'auto' }} onClick={importADifferentProject}>
            Cancel
          </Button>
        </React.Fragment>
      )
    case ImportOperationResult.Error:
      return (
        <React.Fragment>
          <div style={textStyle}>
            {importResult.importStatus.status !== 'done'
              ? 'Error While Importing Project'
              : 'Project Imported With Errors'}
          </div>
          {when(
            importResult.importStatus.status !== 'done',
            <Button
              style={{
                cursor: 'pointer',
                marginLeft: 'auto',
              }}
              onClick={continueAnyway}
            >
              Continue Anyway
            </Button>,
          )}
          {importResult.importStatus.status === 'done' ? (
            <Button style={{ ...buttonStyle }} onClick={hideWizard}>
              Continue To Editor
            </Button>
          ) : (
            <Button style={{ ...buttonStyle }} onClick={importADifferentProject}>
              Cancel
            </Button>
          )}
        </React.Fragment>
      )
    default:
      assertNever(importResult.result)
  }
}
