export type DiscordMessageType = 'info' | 'success' | 'warning' | 'error'
export type DiscordMessage = {
  title: string
  description: string
  url?: string
  fields?: Record<string, string>
}

export type DiscordWebhookType = 'SITE_IMPORT'

export type DiscordEndpointPayload = DiscordEndpointSiteImport

type DiscordEndpointMetadata = {
  webhookType: DiscordWebhookType
  messageType: DiscordMessageType
  messageData: Record<string, unknown>
}

export type DiscordEndpointSiteImport = DiscordEndpointMetadata & {
  webhookType: 'SITE_IMPORT'
  messageData: DiscordEndpointSiteImportMessageData
}
export type DiscordEndpointSiteImportMessageData = {
  projectName: string
  importResult: 'success' | 'warn' | 'error' | 'criticalError'
  importDone: boolean
  errors: string[]
  warnings: string[]
  criticalErrors: string[]
  githubRepo?: {
    owner: string
    repository: string
  }
  branchName?: string
}
