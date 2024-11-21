import type { LoaderFunctionArgs } from '@remix-run/node'
import { json, type ActionFunctionArgs } from '@remix-run/node'
import { ALLOW } from '../handlers/validators'
import { handle, handleOptions } from '../util/api.server'
import type { DiscordEndpointPayload } from 'utopia-shared/src/types'
import { requireUser } from '../util/api.server'
import { sendDiscordMessage } from '../util/discordWebhookUtils'
import { Status } from '../util/statusCodes'
import { buildDiscordMessage } from '~/handlers/discordMessageBuilder'

export async function loader(args: LoaderFunctionArgs) {
  return handle(args, {
    OPTIONS: handleOptions,
  })
}

export async function action(args: ActionFunctionArgs) {
  return handle(args, {
    POST: {
      validator: ALLOW,
      handler: handleDiscordWebhook,
    },
  })
}

export async function handleDiscordWebhook(req: Request) {
  const user = await requireUser(req)
  const payload = (await req.json()) as DiscordEndpointPayload
  await sendDiscordMessage(
    payload.webhookType,
    payload.messageType,
    buildDiscordMessage(payload),
    user,
  )
  return json({ success: true }, { status: Status.OK })
}
