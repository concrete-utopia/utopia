import type {
  DiscordEndpointPayload,
  DiscordMessage,
  DiscordEndpointSiteImportMessageData,
} from 'utopia-shared/src/types'
import { assertNever, assertNeverApiError } from '../util/assertNever'
import { Status } from '../util/statusCodes'

export function buildDiscordMessage(payload: DiscordEndpointPayload): DiscordMessage {
  switch (payload.webhookType) {
    case 'SITE_IMPORT':
      return buildSiteImportMessage(payload.messageData)
    default:
      assertNeverApiError(
        payload.webhookType,
        `Unknown webhook type: ${payload.webhookType}`,
        Status.NOT_FOUND,
      )
  }
}

function buildSiteImportMessage(message: DiscordEndpointSiteImportMessageData): DiscordMessage {
  const operationsWithErrorsOrWarnings: Record<string, string> = {}
  if (message.criticalErrors.length > 0) {
    operationsWithErrorsOrWarnings['Critical Errors'] = message.criticalErrors
      .map((error) => `- ${error}`)
      .join('\n')
  }
  if (message.errors.length > 0) {
    operationsWithErrorsOrWarnings['Errors'] = message.errors
      .map((error) => `- ${error}`)
      .join('\n')
  }
  if (message.warnings.length > 0) {
    operationsWithErrorsOrWarnings['Warnings'] = message.warnings
      .map((warning) => `- ${warning}`)
      .join('\n')
  }

  const fields = {
    'Project Name': message.projectName,
    'Github URL':
      message.githubRepo != null
        ? `https://github.com/${message.githubRepo.owner}/${message.githubRepo.repository}`
        : '',
    'Github Branch': message.branchName ?? '',
    ...operationsWithErrorsOrWarnings,
  }

  let title: string
  let description: string
  const importResult = message.importResult
  switch (importResult) {
    case 'criticalError':
      title = 'Critical Error'
      description = 'The import process encountered a critical error and was aborted.'
      break
    case 'error':
      title = 'Error'
      description = message.importDone
        ? 'The import process was completed with errors.'
        : 'The import process encountered an error and was aborted by the user.'
      break
    case 'warn':
      title = 'Warning'
      description = 'The import process was completed with warnings.'
      break
    case 'success':
      title = 'Success'
      description = 'The import process was completed successfully.'
      break
    default:
      assertNever(importResult)
  }

  return {
    title: title,
    description: description,
    fields: fields,
  }
}
