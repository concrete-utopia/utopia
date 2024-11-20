export type DiscordMessage = {
  title: string
  description: string
  url?: string
  fields?: Record<string, string>
}

export type DiscordWebhookType = 'SITE_IMPORT'
export type DiscordMessageType = 'info' | 'success' | 'warning' | 'error'
export type DiscordWebhookBody = {
  webhookType: DiscordWebhookType
  messageType: DiscordMessageType
  message: DiscordMessage
}
