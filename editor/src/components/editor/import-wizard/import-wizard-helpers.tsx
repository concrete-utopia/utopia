import * as React from 'react'
import type { ImportOperation } from '../../../core/shared/import/import-operation-types'
import { assertNever } from '../../../core/shared/utils'

export function getImportOperationText(operation: ImportOperation): React.ReactNode {
  if (operation.text != null) {
    return operation.text
  }
  switch (operation.type) {
    case 'loadBranch':
      if (operation.branchName != null) {
        return (
          <span>
            Fetching branch{' '}
            <strong>
              {operation.githubRepo?.owner}/{operation.githubRepo?.repository}@
              {operation.branchName}
            </strong>
          </span>
        )
      } else {
        return (
          <span>
            Fetching repository{' '}
            <strong>
              {operation.githubRepo?.owner}/{operation.githubRepo?.repository}
            </strong>
          </span>
        )
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
