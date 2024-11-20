import { MessageBuilder, Webhook } from 'discord-webhook-node'
import type {
  DiscordMessage,
  DiscordMessageType,
  DiscordWebhookType,
} from 'utopia-shared/src/types'
import { Status } from './statusCodes'
import { ApiError } from './errors'

const colors: Record<DiscordMessageType, number> = {
  info: 4037805,
  success: 65340,
  warning: 16763904,
  error: 16729149,
}

const webhooksUrls: Record<DiscordWebhookType, string> = {
  SITE_IMPORT:
    'https://discord.com/api/webhooks/1308453828064055326/TIBbcLICvff22v6Qm5RY_UVf8oN0A6Em2akQIqQgZ5KU0GCkNvBeOwARMp4LD_RV-afi',
}

const webhooks: Partial<Record<DiscordWebhookType, Webhook>> = {}

export function sendDiscordMessage(
  type: DiscordWebhookType,
  messageType: DiscordMessageType,
  messageDescriptor: DiscordMessage,
): Promise<void> {
  if (!hasWebhookUrl(type)) {
    throw new ApiError(`Webhook URL for ${type} not found`, Status.NOT_FOUND)
  }
  const webhook = webhooks[type] || createDiscordWebhook(webhooksUrls[type])

  let message = new MessageBuilder()
    .setTitle(messageDescriptor.title)
    .setDescription(messageDescriptor.description)
    .setColor(colors[messageType])
    .setTimestamp()

  if (messageDescriptor.url) {
    message = message.setURL(messageDescriptor.url)
  }

  if (messageDescriptor.fields != null) {
    Object.entries(messageDescriptor.fields).forEach(([key, value]) => {
      message = message.addField(key, value)
    })
  }

  return webhook.send(message)
}

function hasWebhookUrl(type: DiscordWebhookType) {
  return webhooksUrls[type] !== undefined
}

function createDiscordWebhook(url: string) {
  return new Webhook(url)
}
