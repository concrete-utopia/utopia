import {
  childPaths,
  deletePath,
  ensureDirectoryExists,
  readDirectory,
  readFileAsUTF8,
  writeFileAsUTF8,
} from './fs/fs-utils'
import { parseMessage, UtopiaVSCodeMessage } from './messages'
import { appendToPath } from './path-utils'

type Mailbox = 'VSCODE_MAILBOX' | 'UTOPIA_MAILBOX'
export const VSCodeInbox: Mailbox = 'VSCODE_MAILBOX'
export const UtopiaInbox: Mailbox = 'UTOPIA_MAILBOX'

let inbox: Mailbox
let outbox: Mailbox
let onMessageCallback: (message: UtopiaVSCodeMessage) => void
let counter: number = 0
let lastConsumedMessage: number = -1
let queuedMessages: UtopiaVSCodeMessage[] = []
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
    queuedMessages.push(message)
  } else {
    return sendNamedMessage(generateMessageName(), JSON.stringify(message))
  }
}

async function sendNamedMessage(messageName: string, content: string): Promise<void> {
  return writeFileAsUTF8(pathForOutboxMessage(messageName), content)
}

async function initOutbox(outboxToUse: Mailbox): Promise<void> {
  outbox = outboxToUse
  await ensureMailboxExists(outboxToUse)
  if (queuedMessages.length > 0) {
    queuedMessages.forEach(sendMessage)
    queuedMessages = []
  }
}

async function receiveMessage(messageName: string): Promise<UtopiaVSCodeMessage> {
  const messagePath = pathForInboxMessage(messageName)
  const content = await readFileAsUTF8(messagePath)
  return parseMessage(content)
}

async function pollInbox(): Promise<void> {
  const allMessages = await readDirectory(pathForMailbox(inbox))
  const messagesToProcess = allMessages.filter(
    (messageName) => Number.parseInt(messageName) > lastConsumedMessage,
  )
  if (messagesToProcess.length > 0) {
    const messages = await Promise.all(messagesToProcess.map(receiveMessage))
    lastConsumedMessage = Math.max(
      ...messagesToProcess.map((messageName) => Number.parseInt(messageName)),
    )
    messages.forEach(onMessageCallback)
  }
  setTimeout(pollInbox, POLLING_TIMEOUT)
}

async function ensureMailboxExists(mailbox: Mailbox): Promise<void> {
  await ensureDirectoryExists(pathForMailbox(mailbox))
}

async function clearMailbox(mailbox: Mailbox): Promise<void> {
  const messagePaths = await childPaths(pathForMailbox(mailbox))
  await Promise.all(messagePaths.map((messagePath) => deletePath(messagePath, false)))
}

async function initInbox(
  inboxToUse: Mailbox,
  onMessage: (message: UtopiaVSCodeMessage) => void,
): Promise<void> {
  inbox = inboxToUse
  await ensureMailboxExists(inboxToUse)
  await clearMailbox(inboxToUse)
  onMessageCallback = onMessage
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
