/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type {
  ImportCheckRequirementAndFix,
  ImportFetchDependency,
  ImportOperation,
} from '../../../core/shared/import/import-operation-types'
import { ImportOperationResult } from '../../../core/shared/import/import-operation-types'
import { assertNever } from '../../../core/shared/utils'
import { Icn, Icons, useColorTheme } from '../../../uuiui'
import { GithubSpinner } from '../../../components/navigator/left-pane/github-pane/github-spinner'
import { RequirementResolutionResult } from '../../../core/shared/import/project-health-check/utopia-requirements-types'

export function OperationLine({ operation }: { operation: ImportOperation }) {
  const operationRunningStatus = React.useMemo(() => {
    return operation.timeStarted == null
      ? 'waiting'
      : operation.timeDone == null
      ? 'running'
      : 'done'
  }, [operation.timeStarted, operation.timeDone])
  const colorTheme = useColorTheme()
  const textColor = operationRunningStatus === 'waiting' ? 'gray' : colorTheme.fg0.value

  const [childrenShown, serChildrenShown] = React.useState(false)
  const shouldShowChildren = React.useMemo(
    () =>
      childrenShown ||
      operation.timeDone == null ||
      operation.result == ImportOperationResult.Error,
    [childrenShown, operation.timeDone, operation.result],
  )
  const hasChildren = React.useMemo(
    () => operation.children != null && operation.children.length > 0,
    [operation.children],
  )
  const toggleShowChildren = React.useCallback(() => {
    if (hasChildren) {
      serChildrenShown((shown) => !shown)
    }
  }, [hasChildren])

  return (
    <OperationLineWrapper
      className={operationRunningStatus == 'done' ? 'operation-done' : 'operation-pending'}
      onClick={toggleShowChildren}
    >
      <OperationLineContent textColor={textColor}>
        <OperationIcon runningStatus={operationRunningStatus} result={operation.result} />
        <div>{getImportOperationText(operation)}</div>
        <div>
          <TimeFromInSeconds operation={operation} runningStatus={operationRunningStatus} />
        </div>
        {hasChildren ? (
          <div>
            {shouldShowChildren ? <Icons.ExpansionArrowDown /> : <Icons.ExpansionArrowRight />}
          </div>
        ) : null}
      </OperationLineContent>
      {shouldShowChildren && hasChildren ? <OperationChildrenList operation={operation} /> : null}
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
        <AggregatedChildrenStatus
          childOperations={operation.children as ImportFetchDependency[]}
          successFn={dependenciesSuccessFn}
          successTextFn={dependenciesSuccessTextFn}
        />
      ) : operation.type === 'checkRequirements' ? (
        operation.children.map((childOperation) => (
          <OperationLine
            key={childOperation.id ?? childOperation.type}
            operation={childOperation}
          />
        ))
      ) : null}
    </div>
  )
}
const dependenciesSuccessFn = (op: ImportFetchDependency) =>
  op.result === ImportOperationResult.Success
const dependenciesSuccessTextFn = (successCount: number) =>
  `${successCount} dependencies fetched successfully`
const requirementsSuccessFn = (op: ImportCheckRequirementAndFix) =>
  op.resolution === RequirementResolutionResult.Passed
const requirementsSuccessTextFn = (successCount: number) => `${successCount} requirements met`

function AggregatedChildrenStatus<T extends ImportOperation>({
  childOperations,
  successFn,
  successTextFn,
}: {
  childOperations: T[]
  successFn: (operation: T) => boolean
  successTextFn: (successCount: number) => string
}) {
  const colorTheme = useColorTheme()
  const doneDependencies = childOperations.filter(successFn)
  const restOfDependencies = childOperations.filter((op) => !successFn(op))
  return (
    <React.Fragment>
      {doneDependencies.length > 0 ? (
        <OperationLineWrapper className='operation-done'>
          <OperationLineContent textColor={colorTheme.fg0.value}>
            <Icn color='green' type='checkmark' />
            <div>{successTextFn(doneDependencies.length)}</div>
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
    return <Icn color='green' type='checkmark' />
  } else if (runningStatus === 'done' && result === 'warn') {
    return <Icn color='component-orange' type='warningtriangle' category='navigator-element' />
  } else if (runningStatus === 'waiting') {
    return <Icons.Dot />
  } else {
    return <Icn color='error' type='cross' />
  }
}

function TimeFromInSeconds({
  operation,
  runningStatus,
}: {
  operation: ImportOperation
  runningStatus: 'waiting' | 'running' | 'done'
}) {
  const colorTheme = useColorTheme()
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
    <div
      data-short-time={operationTime < 100}
      style={{
        color: runningStatus === 'running' ? colorTheme.fg0.value : 'gray',
        fontSize: runningStatus === 'running' ? undefined : 12,
      }}
    >
      {timeInSeconds}s
    </div>
  )
}

function OperationLineWrapper({
  children,
  className,
  onClick,
}: {
  children: React.ReactNode
  className: string
  onClick?: () => void
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
          paddingLeft: 26,
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
      onClick={onClick}
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
        gridTemplateColumns: '15px max-content 1fr 14px',
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
    case 'checkRequirements':
      return 'Checking Utopia requirements'
    case 'checkRequirementAndFix':
      return operation.text
    default:
      assertNever(operation)
  }
}
