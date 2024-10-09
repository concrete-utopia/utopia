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
import { hideImportWizard } from './import-wizard-service'
import { assertNever } from '../../../core/shared/utils'

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
            {operations.map((operation) => (
              <OperationLine key={operation.id ?? operation.type} operation={operation} />
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
      return operation.result === 'success'
        ? 'green'
        : operation.result === 'partial'
        ? 'orange'
        : 'red'
    }
  }, [operationStatus, operation.result])
  return (
    <OperationLineWrapper
      className={operationStatus == 'done' ? 'operation-done' : 'operation-pending'}
    >
      <OperationLineContent textColor={textColor}>
        <OperationIcon status={operationStatus} result={operation.result} />
        <div>{getImportOperationText(operation)}</div>
        <div>
          <TimeFromInSeconds operation={operation} />
        </div>
      </OperationLineContent>
      <OperationChildrenList operation={operation} />
    </OperationLineWrapper>
  )
}

function OperationChildrenList({ operation }: { operation: ImportOperation }) {
  if (operation.children == null || operation.children.length === 0) {
    return null
  }
  if (operation.type === 'refreshDependencies') {
    return (
      <div
        className='import-wizard-operation-children'
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 10,
        }}
      >
        <DependenciesStatus
          dependenciesOperations={operation.children}
          parentOperation={operation}
        />
      </div>
    )
  }
  return (
    <div
      className='import-wizard-operation-children'
      style={{
        display: 'flex',
        flexDirection: 'column',
        gap: 10,
      }}
    >
      {operation.children.map((child) => (
        <OperationLine key={child.id ?? child.type} operation={child} />
      ))}
    </div>
  )
}

function DependenciesStatus({
  dependenciesOperations,
}: {
  dependenciesOperations: ImportOperation[]
  parentOperation: ImportOperation
}) {
  const doneDependencies = dependenciesOperations.filter((op) => op.result === 'success')
  const restOfDependencies = dependenciesOperations.filter((op) => op.result !== 'success')
  const doneDependenciesText =
    doneDependencies.length === 0
      ? ''
      : `${doneDependencies.length} dependencies fetched successfully`
  return (
    <React.Fragment>
      <OperationLineWrapper className='operation-done'>
        <OperationLineContent textColor='green'>
          <Icons.Checkmark />
          <div>{doneDependenciesText}</div>
        </OperationLineContent>
      </OperationLineWrapper>
      {restOfDependencies.map((operation) => (
        <OperationLine key={operation.id ?? operation.type} operation={operation} />
      ))}
    </React.Fragment>
  )
}

function OperationIcon({ status, result }: { status: string; result?: string }) {
  if (status === 'running') {
    return <GithubSpinner />
  } else if (status === 'done' && result === 'success') {
    return <Icons.Checkmark />
  } else if (status === 'done' && result === 'partial') {
    return <Icons.WarningTriangle />
  } else if (status === 'not started') {
    return <Icons.Dot />
  } else {
    return <Icons.Cross />
  }
}

function TimeFromInSeconds({ operation }: { operation: ImportOperation }) {
  const [currentTime, setCurrentTime] = React.useState(Date.now())
  React.useEffect(() => {
    const interval = setInterval(() => {
      setCurrentTime(Date.now())
    }, 1000)
    return () => clearInterval(interval)
  }, [])
  const operationTime = useMemo(() => {
    if (operation.timeStarted == null) {
      return 0
    }
    if (operation.timeDone == null) {
      return currentTime - operation.timeStarted
    }
    return operation.timeDone - operation.timeStarted
  }, [operation.timeStarted, operation.timeDone, currentTime])
  const timeInSeconds =
    operation.timeDone != null
      ? (operationTime / 1000).toFixed(2)
      : Math.max(Math.floor(operationTime / 1000), 0)
  return operation.timeStarted == null ? null : (
    <div data-short-time={operationTime < 100}>{timeInSeconds}s</div>
  )
}

function getImportOperationText(operation: ImportOperation): React.ReactNode {
  switch (operation.type) {
    case 'loadBranch':
      return (
        <span>
          Loading branch{' '}
          <strong>
            {operation.githubRepo?.owner}/{operation.githubRepo?.repository}@{operation.branchName}
          </strong>
        </span>
      )
    case 'fetchDependency':
      return `Fetching ${operation.dependencyName}@${operation.dependencyVersion}`
    case 'parseFiles':
      return 'Parsing files'
    case 'createStoryboard':
      return 'Creating storyboard file'
    case 'refreshDependencies':
      return 'Fetching dependencies'
    case 'createPackageJsonEntry':
      return 'Creating package.json entry'
    case 'checkUtopiaRequirements':
      return 'Checking Utopia requirements'
    default:
      assertNever(operation)
  }
}

function OperationLineWrapper({
  children,
  className,
}: {
  children: React.ReactNode
  className: string
}) {
  return (
    <div
      className={`import-wizard-operation-line ${className}`}
      style={{
        display: 'flex',
        flexDirection: 'column',
        gap: 10,
      }}
      css={{
        '.import-wizard-operation-children > &': {
          paddingLeft: 10,
          fontSize: 12,
          img: {
            width: 12,
            height: 12,
          },
        },
        '.import-wizard-operation-children .operation-done [data-short-time=true]': {
          visibility: 'hidden',
        },
      }}
    >
      {children}
    </div>
  )
}

function OperationLineContent({
  children,
  textColor,
}: {
  children: React.ReactNode
  textColor: string
}) {
  return (
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
      {children}
    </div>
  )
}
