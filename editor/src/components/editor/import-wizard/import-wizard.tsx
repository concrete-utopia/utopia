/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React, { useMemo } from 'react'
import { getProjectID } from '../../../common/env-vars'
import { Button, Icons, useColorTheme, UtopiaStyles } from '../../../uuiui'
import { GithubSpinner } from '../../navigator/left-pane/github-pane/github-spinner'
import { useEditorState, Substores } from '../store/store-hook'
import { when } from '../../../utils/react-conditionals'
import type { ImportOperation } from './import-wizard-service'
import { getImportOperationText, hideImportWizard } from './import-wizard-service'

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
            width: 610,
            minHeight: 500,
            maxHeight: 770,
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            fontSize: '14px',
            lineHeight: 'normal',
            letterSpacing: 'normal',
            padding: 40,
            overflow: 'hidden',
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
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
              gap: 10,
              overflow: 'scroll',
              height: '100%',
              width: '100%',
            }}
          >
            {operations.map((operation, index) => (
              <OperationLine key={index} operation={operation} />
            ))}
          </div>
        </div>,
      )}
    </div>
  )
})
ImportWizard.displayName = 'ImportWizard'

function OperationLine({ operation }: { operation: ImportOperation }) {
  const operationStatus = useMemo(() => {
    if (operation.timeStarted == null) {
      return 'not started'
    }
    if (operation.timeDone == null) {
      return 'running'
    }
    return `done`
  }, [operation.timeStarted, operation.timeDone])
  const textColor = React.useMemo(() => {
    if (operationStatus === 'not started') {
      return 'gray'
    } else if (operationStatus === 'running') {
      return 'black'
    } else {
      return operation.result === 'success' ? 'green' : 'red'
    }
  }, [operationStatus, operation.result])
  return (
    <div
      className='import-wizard-operation-line'
      style={{
        display: 'flex',
        flexDirection: 'column',
        gap: 10,
      }}
      css={{
        '.import-wizard-operation-children > &': {
          paddingLeft: 10,
          fontSize: 12,
        },
      }}
    >
      <div
        className='import-wizard-operation-line-content'
        style={{
          display: 'grid',
          gridTemplateColumns: '15px 1fr 50px',
          gap: 10,
          alignItems: 'center',
          color: textColor,
        }}
      >
        <OperationIcon status={operationStatus} result={operation.result} />
        <div>{getImportOperationText(operation)}</div>
        <div>
          <TimeFromInSeconds startTime={operation.timeStarted} endTime={operation.timeDone} />
        </div>
      </div>
      {operation.children != null && operation.children.length > 0 && (
        <div
          className='import-wizard-operation-children'
          style={{
            display: 'flex',
            flexDirection: 'column',
            gap: 10,
          }}
        >
          {operation.children.map((child, index) => (
            <OperationLine key={index} operation={child} />
          ))}
        </div>
      )}
    </div>
  )
}

function OperationIcon({ status, result }: { status: string; result?: string }) {
  if (status === 'running') {
    return <GithubSpinner />
  } else if (status === 'done' && result === 'success') {
    return <Icons.Checkmark />
  } else if (status === 'not started') {
    return <Icons.Dot />
  } else {
    return <Icons.Cross />
  }
}

function TimeFromInSeconds({ startTime, endTime }: { startTime?: number; endTime?: number }) {
  const [currentTime, setCurrentTime] = React.useState(Date.now())
  React.useEffect(() => {
    const interval = setInterval(() => {
      setCurrentTime(Date.now())
    }, 1000)
    return () => clearInterval(interval)
  }, [])
  const time = useMemo(() => {
    if (startTime == null) {
      return 0
    }
    if (endTime == null) {
      return currentTime - startTime
    }
    return endTime - startTime
  }, [startTime, endTime, currentTime])
  const timeInSeconds =
    endTime != null ? (time / 1000).toFixed(2) : Math.max(Math.floor(time / 1000), 0)
  return startTime == null ? null : <div>{timeInSeconds}s</div>
}
