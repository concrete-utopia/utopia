import { MessageBuilder, Webhook } from 'discord-webhook-node'
import type {
  DiscordMessage,
  DiscordMessageType,
  DiscordWebhookType,
} from 'utopia-shared/src/types'
import { Status } from './statusCodes'
import { ApiError } from './errors'
import type { UserDetails } from 'prisma-client'
import { ServerEnvironment } from '../env.server'
import { assertNever } from './assertNever'

// these need to be decimal colors (for 'discord-webhook-node')
const colors: Record<DiscordMessageType, number> = {
  info: 4037805,
  success: 65340,
  warning: 16763904,
  error: 16729149,
}

const webhooksUrls: Record<DiscordWebhookType, string> = {
  SITE_IMPORT: ServerEnvironment.DISCORD_WEBHOOK_SITE_IMPORT,
}

const webhooks: Partial<Record<DiscordWebhookType, Webhook>> = {}

export function sendDiscordMessage(
  type: DiscordWebhookType,
  messageType: DiscordMessageType,
  messageDescriptor: DiscordMessage,
  user: UserDetails,
): Promise<void> {
  const webhook = getWebhook(type)

  let message = new MessageBuilder()
    .setTitle(messageDescriptor.title)
    .setDescription(messageDescriptor.description)
    .setColor(colors[messageType])
    .setTimestamp()

  if (messageDescriptor.url) {
    message = message.setURL(messageDescriptor.url)
  }

  const fields: Record<string, string> = {
    User: `${user.name ?? 'Unknown User'} (${user.email ?? 'Unknown Email'})`,
    Environment: getEnvironmentText(),
    ...(messageDescriptor.fields ?? {}),
  }

  Object.entries(fields).forEach(([key, value]) => {
    message = message.addField(key, value)
  })

  return webhook.send(message)
}

function hasWebhookUrl(type: DiscordWebhookType) {
  return webhooksUrls[type] != undefined && webhooksUrls[type] != ''
}

function getWebhook(type: DiscordWebhookType) {
  if (!hasWebhookUrl(type)) {
    throw new ApiError(`Webhook URL for ${type} not found`, Status.NOT_FOUND)
  }
  if (webhooks[type] == null) {
    webhooks[type] = createDiscordWebhook(webhooksUrls[type])
  }
  return webhooks[type]
}

function createDiscordWebhook(url: string) {
  return new Webhook(url)
}

function getEnvironmentText() {
  switch (ServerEnvironment.environment) {
    case 'local':
      return 'Local'
    case 'stage':
      return 'Staging'
    case 'prod':
      return 'Production'
    case 'test':
      return 'Test'
    default:
      assertNever(ServerEnvironment.environment)
  }
}
