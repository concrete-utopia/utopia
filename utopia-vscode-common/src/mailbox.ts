import {
  createDirectory,
  deletePath,
  exists,
  readFileWithEncoding,
  watch,
  writeFileWithEncoding,
} from './browserfs-utils'
import { appendToPath } from './path-utils'

type Mailbox = 'VSCODE_INBOX' | 'UTOPIA_INBOX'
const VSCodeInbox: Mailbox = 'VSCODE_INBOX'
const UtopiaInbox: Mailbox = 'UTOPIA_INBOX'
const MailboxReadyMessageName = 'READY'

let inbox: Mailbox
let outbox: Mailbox
let counter: number = 0

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

export async function sendMessageWithContent(content: string): Promise<void> {
  return sendMessage(generateMessageName(), content)
}

async function sendMessage(messageName: string, content: string): Promise<void> {
  return writeFileWithEncoding(pathForOutboxMessage(messageName), content)
}

async function initOutbox(outboxToUse: Mailbox): Promise<void> {
  outbox = outboxToUse
  const outboxPath = pathForMailbox(outbox)
  await deletePath(outboxPath, true)
  await createDirectory(outboxPath)
  await sendMessage(MailboxReadyMessageName, '')
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

async function readAndDeleteMessage(messagePath: string): Promise<string> {
  const content = await readFileWithEncoding(messagePath)
  await deletePath(messagePath, false)
  return content
}

async function initInbox(inboxToUse: Mailbox, onMessage: (content: string) => void): Promise<void> {
  inbox = inboxToUse
  const inboxPath = pathForMailbox(inbox)
  let inboxReady = false
  await waitUntilInboxReady()
  const onNewMessage = (newMessagePath: string) =>
    readAndDeleteMessage(newMessagePath).then(onMessage)
  watch(
    inboxPath,
    true,
    onNewMessage,
    () => {},
    () => {},
  )
}

export async function init(
  inboxToUse: Mailbox,
  onMessage: (content: string) => void,
): Promise<void> {
  await initOutbox(inboxToUse === VSCodeInbox ? UtopiaInbox : VSCodeInbox)
  await initInbox(inboxToUse, onMessage)
}
