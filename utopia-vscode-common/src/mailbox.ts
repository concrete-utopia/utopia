import {
  childPaths,
  createDirectory,
  deletePath,
  exists,
  readFileWithEncoding,
  writeFileWithEncoding,
} from './browserfs-utils'
import { parseMessage, UtopiaVSCodeMessage } from './messages'
import { appendToPath } from './path-utils'

type Mailbox = 'VSCODE_MAILBOX' | 'UTOPIA_MAILBOX'
export const VSCodeInbox: Mailbox = 'VSCODE_MAILBOX'
export const UtopiaInbox: Mailbox = 'UTOPIA_MAILBOX'
const MailboxReadyMessageName = 'READY'

// FIXME there are a few issues here:
// 1 - remounting will trigger the mailbox to blind initialise
// 2 - opening the project in another tab will create a race condition around who reads the messages
// Fix both of these by having the mailbox clear itself on init and track the last message number consumed
// rather than deleting them when consuming them

let inbox: Mailbox
let outbox: Mailbox
let onMessageCallback: (message: UtopiaVSCodeMessage) => void
let counter: number = 0
const POLLING_TIMEOUT = 100

function pathForMailbox(mailbox: Mailbox): string {
  return `/${mailbox}`
}

function pathForMessage(messageName: string, mailbox: Mailbox): string {
  return appendToPath(pathForMailbox(mailbox), messageName)
}

const pathForInboxMessage = (messageName: string) => pathForMessage(messageName, inbox)
const pathForOutboxMessage = (messageName: string) => pathForMessage(messageName, outbox)

function generateMessageName(): string {
  return `${counter++}`
}

export async function sendMessage(message: UtopiaVSCodeMessage): Promise<void> {
  if (outbox == null) {
    // TODO Queue messages we're trying to send if the mailbox hasn't been initialised yet
    throw new Error(`Messaging system hasn't been initialised`)
  }
  return sendNamedMessage(generateMessageName(), JSON.stringify(message))
}

async function sendNamedMessage(messageName: string, content: string): Promise<void> {
  return writeFileWithEncoding(pathForOutboxMessage(messageName), content)
}

async function initOutbox(outboxToUse: Mailbox): Promise<void> {
  outbox = outboxToUse
  const outboxPath = pathForMailbox(outbox)
  await deletePath(outboxPath, true)
  await createDirectory(outboxPath)
  await sendNamedMessage(MailboxReadyMessageName, '')
}

async function waitUntilInboxReady(): Promise<void> {
  const readyMessagePath = pathForInboxMessage(MailboxReadyMessageName)
  let isReady = await exists(readyMessagePath)
  if (isReady) {
    await deletePath(readyMessagePath, false)
  } else {
    await new Promise((resolve) => setTimeout(resolve, 100))
    return waitUntilInboxReady()
  }
}

async function receiveMessage(messagePath: string): Promise<UtopiaVSCodeMessage> {
  const content = await readFileWithEncoding(messagePath)
  await deletePath(messagePath, false)
  return parseMessage(content)
}

async function pollInbox(): Promise<void> {
  const messagePaths = await childPaths(pathForMailbox(inbox))
  if (messagePaths.length > 0) {
    const messages = await Promise.all(messagePaths.map(receiveMessage))
    messages.forEach(onMessageCallback)
  }
  setTimeout(pollInbox, POLLING_TIMEOUT)
}

async function initInbox(
  inboxToUse: Mailbox,
  onMessage: (message: UtopiaVSCodeMessage) => void,
): Promise<void> {
  inbox = inboxToUse
  onMessageCallback = onMessage
  await waitUntilInboxReady()
  pollInbox()
}

export async function initMailbox(
  inboxToUse: Mailbox,
  onMessage: (message: UtopiaVSCodeMessage) => void,
): Promise<void> {
  await initOutbox(inboxToUse === VSCodeInbox ? UtopiaInbox : VSCodeInbox)
  await initInbox(inboxToUse, onMessage)
}

// TODO Do we need an unsubscribe feature?
