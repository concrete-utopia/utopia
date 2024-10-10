/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type { ImportOperation, ImportOperationResult } from './import-wizard-service'
import { assertNever } from '../../../core/shared/utils'
import { Icons } from '../../../uuiui'
import { GithubSpinner } from '../../../components/navigator/left-pane/github-pane/github-spinner'

export function OperationLine({ operation }: { operation: ImportOperation }) {
  const operationRunningStatus = React.useMemo(() => {
    return operation.timeStarted == null
      ? 'waiting'
      : operation.timeDone == null
      ? 'running'
      : 'done'
  }, [operation.timeStarted, operation.timeDone])

  const textColor = React.useMemo(() => {
    if (operationRunningStatus === 'waiting') {
      return 'gray'
    } else if (operationRunningStatus === 'running') {
      return 'black'
    } else if (
      operation.type === 'checkUtopiaRequirementAndFix' &&
      operation.resolution === 'fixed'
    ) {
      return 'var(--utopitheme-primary)'
    } else if (operation.result === 'success') {
      return 'green'
    } else if (operation.result === 'warn') {
      return 'orange'
    } else {
      return 'var(--utopitheme-errorForeground)'
    }
  }, [operationRunningStatus, operation])
  return (
    <OperationLineWrapper
      className={operationRunningStatus == 'done' ? 'operation-done' : 'operation-pending'}
    >
      <OperationLineContent textColor={textColor}>
        <OperationIcon runningStatus={operationRunningStatus} result={operation.result} />
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
  return (
    <div
      className='import-wizard-operation-children'
      style={{
        display: 'flex',
        flexDirection: 'column',
        gap: 10,
      }}
    >
      {operation.type === 'refreshDependencies' ? (
        <DependenciesStatus
          dependenciesOperations={operation.children}
          parentOperation={operation}
        />
      ) : (
        operation.children.map((child) => (
          <OperationLine key={child.id ?? child.type} operation={child} />
        ))
      )}
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
  return (
    <React.Fragment>
      {doneDependencies.length > 0 ? (
        <OperationLineWrapper className='operation-done'>
          <OperationLineContent textColor='green'>
            <Icons.Checkmark />
            <div>{`${doneDependencies.length} dependencies fetched successfully`}</div>
          </OperationLineContent>
        </OperationLineWrapper>
      ) : null}
      {restOfDependencies.map((operation) => (
        <OperationLine key={operation.id ?? operation.type} operation={operation} />
      ))}
    </React.Fragment>
  )
}

function OperationIcon({
  runningStatus,
  result,
}: {
  runningStatus: 'waiting' | 'running' | 'done'
  result?: ImportOperationResult
}) {
  if (runningStatus === 'running') {
    return <GithubSpinner />
  } else if (runningStatus === 'done' && result === 'success') {
    return <Icons.Checkmark />
  } else if (runningStatus === 'done' && result === 'warn') {
    return <Icons.WarningTriangle />
  } else if (runningStatus === 'waiting') {
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
  const operationTime = React.useMemo(() => {
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

function getImportOperationText(operation: ImportOperation): React.ReactNode {
  if (operation.text != null) {
    return operation.text
  }
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
    case 'refreshDependencies':
      return 'Fetching dependencies'
    case 'checkUtopiaRequirements':
      return 'Checking Utopia requirements'
    case 'checkUtopiaRequirementAndFix':
      return operation.text
    default:
      assertNever(operation)
  }
}
