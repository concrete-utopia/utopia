import {
  childPaths,
  deletePath,
  ensureDirectoryExists,
  exists,
  readDirectory,
  readFileAsUTF8,
  writeFileAsUTF8,
} from './fs/fs-utils'
import { FromVSCodeMessage, parseToVSCodeMessage, ToVSCodeMessage } from './messages'
import { appendToPath } from './path-utils'

type Mailbox = 'VSCODE_MAILBOX' | 'UTOPIA_MAILBOX'
export const VSCodeInbox: Mailbox = 'VSCODE_MAILBOX'
export const UtopiaInbox: Mailbox = 'UTOPIA_MAILBOX'

let inbox: Mailbox
let outbox: Mailbox
let onMessageCallback: (message: any) => void
let counter: number = 0
let lastConsumedMessage: number = -1
let queuedMessages: Array<ToVSCodeMessage | FromVSCodeMessage> = []
const POLLING_TIMEOUT = 8

function lastConsumedMessageKey(mailbox: Mailbox): string {
  return `/${mailbox}_LAST_CONSUMED`
}

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

export async function sendMessage(message: ToVSCodeMessage | FromVSCodeMessage): Promise<void> {
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

async function receiveMessage<T>(messageName: string, parseMessage: (msg: string) => T): Promise<T> {
  const messagePath = pathForInboxMessage(messageName)
  const content = await readFileAsUTF8(messagePath)
  return parseMessage(content)
}

async function pollInbox<T>(parseMessage: (msg: string) => T): Promise<void> {
  const allMessages = await readDirectory(pathForMailbox(inbox))
  const messagesToProcess = allMessages.filter(
    (messageName) => Number.parseInt(messageName) > lastConsumedMessage,
  )
  if (messagesToProcess.length > 0) {
    const messages = await Promise.all(messagesToProcess.map(m => receiveMessage(m, parseMessage)))
    lastConsumedMessage = Math.max(
      ...messagesToProcess.map((messageName) => Number.parseInt(messageName)),
    )
    await updateLastConsumedMessageFile(inbox, lastConsumedMessage)
    messages.forEach(onMessageCallback)
  }
  setTimeout(() => pollInbox(parseMessage), POLLING_TIMEOUT)
}


async function initInbox<T>(
  inboxToUse: Mailbox,
  parseMessage: (msg: string) => T,
  onMessage: (message: T) => void,
): Promise<void> {
  inbox = inboxToUse
  await ensureMailboxExists(inboxToUse)
  lastConsumedMessage = await getLastConsumedMessageNumber(inbox)
  onMessageCallback = onMessage
  pollInbox(parseMessage)
}

async function ensureMailboxExists(mailbox: Mailbox): Promise<void> {
  await ensureDirectoryExists(pathForMailbox(mailbox))
}

async function clearMailbox(mailbox: Mailbox): Promise<void> {
  const messagePaths = await childPaths(pathForMailbox(mailbox))
  await Promise.all(messagePaths.map((messagePath) => deletePath(messagePath, false)))
}

async function clearLastConsumedMessageFile(mailbox: Mailbox): Promise<void> {
  await deletePath(lastConsumedMessageKey(mailbox), false)
}

async function updateLastConsumedMessageFile(mailbox: Mailbox, value: number): Promise<void> {
  await writeFileAsUTF8(lastConsumedMessageKey(mailbox), `${value}`)
}

async function getLastConsumedMessageNumber(mailbox: Mailbox): Promise<number> {
  const lastConsumedMessageValueExists = await exists(lastConsumedMessageKey(mailbox))
  if (lastConsumedMessageValueExists) {
    const lastConsumedMessageName = await readFileAsUTF8(lastConsumedMessageKey(mailbox))
    return Number.parseInt(lastConsumedMessageName)
  } else {
    return -1
  }
}

export async function clearBothMailboxes(): Promise<void> {
  await ensureMailboxExists(UtopiaInbox)
  await clearMailbox(UtopiaInbox)
  await ensureMailboxExists(VSCodeInbox)
  await clearMailbox(VSCodeInbox)
  await clearLastConsumedMessageFile(UtopiaInbox)
  await clearLastConsumedMessageFile(VSCodeInbox)
}

export async function initMailbox<T>(
  inboxToUse: Mailbox,
  parseMessage: (msg: string) => T,
  onMessage: (message: T) => void,
): Promise<void> {
  await initOutbox(inboxToUse === VSCodeInbox ? UtopiaInbox : VSCodeInbox)
  await initInbox(inboxToUse, parseMessage, onMessage)
}

// TODO Do we need an unsubscribe feature?
