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
import { Icons } from '../../../uuiui'
import { GithubSpinner } from '../../../components/navigator/left-pane/github-pane/github-spinner'
import { RequirementResolutionResult } from '../../../core/shared/import/proejct-health-check/utopia-requirements-types'

export function OperationLine({ operation }: { operation: ImportOperation }) {
  const operationRunningStatus = React.useMemo(() => {
    return operation.timeStarted == null
      ? 'waiting'
      : operation.timeDone == null
      ? 'running'
      : 'done'
  }, [operation.timeStarted, operation.timeDone])

  const textColor = React.useMemo(
    () => getTextColor(operationRunningStatus, operation),
    [operationRunningStatus, operation],
  )

  const [childrenShown, serChildrenShown] = React.useState(false)
  const shouldShowChildren = React.useMemo(
    () => childrenShown || operation.timeDone == null,
    [childrenShown, operation.timeDone],
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
  const doneDependencies = childOperations.filter(successFn)
  const restOfDependencies = childOperations.filter((op) => !successFn(op))
  return (
    <React.Fragment>
      {doneDependencies.length > 0 ? (
        <OperationLineWrapper className='operation-done'>
          <OperationLineContent textColor='black'>
            <Icons.Checkmark style={getIconColorStyle(ImportOperationResult.Success)} />
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
  const iconColorStyle = React.useMemo(
    () => (result != null ? getIconColorStyle(result) : {}),
    [result],
  )
  if (runningStatus === 'running') {
    return <GithubSpinner />
  } else if (runningStatus === 'done' && result === 'success') {
    return <Icons.Checkmark style={iconColorStyle} />
  } else if (runningStatus === 'done' && result === 'warn') {
    return <Icons.WarningTriangle style={iconColorStyle} />
  } else if (runningStatus === 'waiting') {
    return <Icons.Dot />
  } else {
    return <Icons.Cross style={iconColorStyle} />
  }
}

function TimeFromInSeconds({
  operation,
  runningStatus,
}: {
  operation: ImportOperation
  runningStatus: 'waiting' | 'running' | 'done'
}) {
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
        color: runningStatus === 'running' ? 'black' : 'gray',
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

function getTextColor(
  operationRunningStatus: 'waiting' | 'running' | 'done',
  operation: ImportOperation,
) {
  if (operationRunningStatus === 'waiting') {
    return 'gray'
  } else {
    return 'black'
  }
}

function getIconColorStyle(result: ImportOperationResult) {
  // temp solution since we currently only have black icons
  // https://codepen.io/sosuke/pen/Pjoqqp
  if (result === ImportOperationResult.Error) {
    return {
      // our error red
      filter:
        'invert(14%) sepia(99%) saturate(4041%) hue-rotate(328deg) brightness(101%) contrast(115%)',
    }
  } else if (result === ImportOperationResult.Warn) {
    return {
      // orange
      filter:
        'invert(72%) sepia(90%) saturate(3088%) hue-rotate(1deg) brightness(105%) contrast(104%)',
    }
  } else if (result === ImportOperationResult.Success) {
    return {
      // green
      filter:
        'invert(72%) sepia(60%) saturate(3628%) hue-rotate(126deg) brightness(104%) contrast(76%)',
    }
  }
  return {}
}
