import {
  childPaths,
  deletePath,
  ensureDirectoryExists,
  exists,
  readDirectory,
  readFileSavedContentAsUTF8,
  writeFileSavedContentAsUTF8,
} from './fs/fs-utils'
import type { FromVSCodeMessage, ToVSCodeMessage } from './messages'
import { appendToPath } from './path-utils'

type Mailbox = 'VSCODE_MAILBOX' | 'UTOPIA_MAILBOX'
export const VSCodeInbox: Mailbox = 'VSCODE_MAILBOX'
export const UtopiaInbox: Mailbox = 'UTOPIA_MAILBOX'

let inbox: Mailbox
let outbox: Mailbox
let onMessageCallback: (message: any) => void
let lastSentMessage: number = 0
let lastConsumedMessage: number = -1
let mailboxLastClearedTimestamp: number = Date.now()
let queuedMessages: Array<ToVSCodeMessage | FromVSCodeMessage> = []
const MIN_POLLING_TIMEOUT = 8
const MAX_POLLING_TIMEOUT = MIN_POLLING_TIMEOUT * Math.pow(2, 4) // Max out at 128ms
let POLLING_TIMEOUT = MIN_POLLING_TIMEOUT
let pollTimeout: any | null = null

let reducePollingAttemptsCount = 0

function reducePollingFrequency() {
  if (POLLING_TIMEOUT < MAX_POLLING_TIMEOUT) {
    reducePollingAttemptsCount++
    if (reducePollingAttemptsCount >= 5) {
      reducePollingAttemptsCount = 0
      POLLING_TIMEOUT = POLLING_TIMEOUT * 2
    }
  }
}

function resetPollingFrequency() {
  reducePollingAttemptsCount = 0
  POLLING_TIMEOUT = MIN_POLLING_TIMEOUT
}

function lastConsumedMessageKey(mailbox: Mailbox): string {
  return `/${mailbox}_LAST_CONSUMED`
}

function mailboxClearedAtTimestampKey(mailbox: Mailbox): string {
  return `/${mailbox}_CLEARED`
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
  return `${lastSentMessage++}`
}

export async function sendMessage(message: ToVSCodeMessage | FromVSCodeMessage): Promise<void> {
  resetPollingFrequency()

  if (outbox == null) {
    queuedMessages.push(message)
  } else {
    return sendNamedMessage(generateMessageName(), JSON.stringify(message))
  }
}

async function sendNamedMessage(messageName: string, content: string): Promise<void> {
  return writeFileSavedContentAsUTF8(pathForOutboxMessage(messageName), content)
}

function maxMessageNumber(messageNames: Array<string>, minValue: number = 0): number {
  return Math.max(minValue, ...messageNames.map((messageName) => Number.parseInt(messageName)))
}

async function initOutbox(outboxToUse: Mailbox): Promise<void> {
  await ensureMailboxExists(outboxToUse)
  const previouslySentMessages = await readDirectory(pathForMailbox(outboxToUse))
  lastSentMessage = maxMessageNumber(previouslySentMessages)

  outbox = outboxToUse
  if (queuedMessages.length > 0) {
    queuedMessages.forEach(sendMessage)
    queuedMessages = []
  }
}

async function receiveMessage<T>(
  messageName: string,
  parseMessage: (msg: string) => T,
): Promise<T> {
  const messagePath = pathForInboxMessage(messageName)
  const content = await readFileSavedContentAsUTF8(messagePath)
  return parseMessage(content)
}

async function waitForPathToExist(path: string, maxWaitTime: number = 5000): Promise<void> {
  if (maxWaitTime >= 0) {
    const doesItExist: boolean = await exists(path)
    if (!doesItExist) {
      return waitForPathToExist(path, maxWaitTime - 100)
    } else {
      return Promise.resolve()
    }
  } else {
    return Promise.reject(`Waited too long for ${path} to exist.`)
  }
}

async function checkAndResetIfMailboxCleared(mailbox: Mailbox): Promise<void> {
  const mailboxClearedAtTimestamp = await getMailboxClearedAtTimestamp(mailbox)
  if (mailboxClearedAtTimestamp > mailboxLastClearedTimestamp) {
    // The mailbox was cleared since we last polled it, meaning our last consumed message
    // count is now invalid, and we need to start consuming messages from the beginning again.
    lastConsumedMessage = -1
    mailboxLastClearedTimestamp = mailboxClearedAtTimestamp
  }
}

async function pollInbox<T>(parseMessage: (msg: string) => T): Promise<void> {
  await checkAndResetIfMailboxCleared(inbox)

  const mailboxPath = pathForMailbox(inbox)
  waitForPathToExist(mailboxPath)
  const allMessages = await readDirectory(mailboxPath)

  // Filter messages to only those that haven't been processed yet. We do this rather than deleting processed
  // messages so that multiple instances in different browser tabs won't drive over eachother.
  const messagesToProcess = allMessages.filter(
    (messageName) => Number.parseInt(messageName) > lastConsumedMessage,
  )
  if (messagesToProcess.length > 0) {
    try {
      const messages = await Promise.all(
        messagesToProcess.map((m) => receiveMessage(m, parseMessage)),
      )
      lastConsumedMessage = maxMessageNumber(messagesToProcess, lastConsumedMessage)
      await updateLastConsumedMessageFile(inbox, lastConsumedMessage)
      messages.forEach(onMessageCallback)
    } catch (e) {
      // It's possible that the mailbox was cleared whilst something was trying to read the messages.
      // If that happens, we bail out of this poll, and the call `checkAndResetIfMailboxCleared` will
      // correct things on the next poll
    }
    resetPollingFrequency()
  } else {
    reducePollingFrequency()
  }
  pollTimeout = setTimeout(() => pollInbox(parseMessage), POLLING_TIMEOUT)
}

async function initInbox<T>(
  inboxToUse: Mailbox,
  parseMessage: (msg: string) => T,
  onMessage: (message: T) => void,
): Promise<void> {
  inbox = inboxToUse
  await ensureMailboxExists(inboxToUse)
  mailboxLastClearedTimestamp = await getMailboxClearedAtTimestamp(inbox)
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
  await writeFileSavedContentAsUTF8(lastConsumedMessageKey(mailbox), `${value}`)
}

async function getLastConsumedMessageNumber(mailbox: Mailbox): Promise<number> {
  const lastConsumedMessageValueExists = await exists(lastConsumedMessageKey(mailbox))
  if (lastConsumedMessageValueExists) {
    try {
      const lastConsumedMessageName = await readFileSavedContentAsUTF8(
        lastConsumedMessageKey(mailbox),
      )
      return Number.parseInt(lastConsumedMessageName)
    } catch (e) {
      // This can be cleared by the VSCode Bridge in between the above line and now, in which case we want to consume all messages from the start
      return -1
    }
  } else {
    return -1
  }
}

async function updateMailboxClearedAtTimestamp(mailbox: Mailbox, timestamp: number): Promise<void> {
  await writeFileSavedContentAsUTF8(mailboxClearedAtTimestampKey(mailbox), `${timestamp}`)
}

async function getMailboxClearedAtTimestamp(mailbox: Mailbox): Promise<number> {
  const mailboxClearedAtTimestampExists = await exists(mailboxClearedAtTimestampKey(mailbox))
  if (mailboxClearedAtTimestampExists) {
    const mailboxClearedAtTimestamp = await readFileSavedContentAsUTF8(
      mailboxClearedAtTimestampKey(mailbox),
    )
    return Number.parseInt(mailboxClearedAtTimestamp)
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
  await updateMailboxClearedAtTimestamp(UtopiaInbox, Date.now())
  await updateMailboxClearedAtTimestamp(VSCodeInbox, Date.now())
}

export function stopPollingMailbox(): void {
  if (pollTimeout != null) {
    clearTimeout(pollTimeout)
    pollTimeout = null
    lastConsumedMessage = -1
    lastSentMessage = 0
  }
}

export async function initMailbox<T>(
  inboxToUse: Mailbox,
  parseMessage: (msg: string) => T,
  onMessage: (message: T) => void,
): Promise<void> {
  await initOutbox(inboxToUse === VSCodeInbox ? UtopiaInbox : VSCodeInbox)
  await initInbox(inboxToUse, parseMessage, onMessage)
}
