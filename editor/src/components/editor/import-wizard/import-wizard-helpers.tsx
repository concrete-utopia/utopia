import * as React from 'react'
import {
  ImportOperationResult,
  type ImportOperation,
} from '../../../core/shared/import/import-operation-types'
import { assertNever } from '../../../core/shared/utils'

export function getImportOperationText(operation: ImportOperation): string {
  if (operation.text != null) {
    return operation.text
  }
  switch (operation.type) {
    case 'loadBranch':
      const action =
        operation.result == ImportOperationResult.Error ||
        operation.result == ImportOperationResult.CriticalError
          ? 'Error Fetching'
          : 'Fetching'
      if (operation.branchName != null) {
        return `${action} branch **${operation.githubRepo?.owner}/${operation.githubRepo?.repository}@${operation.branchName}**`
      } else {
        return `${action} repository **${operation.githubRepo?.owner}/${operation.githubRepo?.repository}**`
      }
    case 'fetchDependency':
      return `Fetching ${operation.dependencyName}@${operation.dependencyVersion}`
    case 'parseFiles':
      return 'Parsing files'
    case 'refreshDependencies':
      return 'Fetching dependencies'
    case 'checkRequirementsPreParse':
      return 'Validating code'
    case 'checkRequirementsPostParse':
      return 'Checking Utopia requirements'
    case 'checkRequirementAndFixPreParse':
      return operation.text
    case 'checkRequirementAndFixPostParse':
      return operation.text
    default:
      assertNever(operation)
  }
}

export function getImportOperationTextAsJsx(operation: ImportOperation): React.ReactNode {
  const text = getImportOperationText(operation)
  const nodes = text.split('**').map((part, index) => {
    return index % 2 == 0 ? part : <strong key={part}>{part}</strong>
  })
  return <span>{nodes}</span>
}
