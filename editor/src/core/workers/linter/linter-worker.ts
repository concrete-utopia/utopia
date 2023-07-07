import type { ErrorMessage } from '../../shared/error-messages'
import { lintCode } from './linter'

export interface LinterRequestMessage {
  type: 'linterrequest'
  filename: string
  code: string
}

export type IncomingLinterWorkerMessage = LinterRequestMessage

export interface LinterResultMessage {
  type: 'linterresult'
  filename: string
  errors: Array<ErrorMessage>
}

export type OutgoingLinterWorkerMessage = LinterResultMessage

export function createLinterRequestMessage(filename: string, code: string): LinterRequestMessage {
  return {
    type: 'linterrequest',
    filename: filename,
    code: code,
  }
}

function createLinterResultMessage(
  filename: string,
  errors: Array<ErrorMessage>,
): LinterResultMessage {
  return {
    type: 'linterresult',
    filename: filename,
    errors: errors,
  }
}

export function handleMessage(
  workerMessage: IncomingLinterWorkerMessage,
  sendMessage: (content: OutgoingLinterWorkerMessage) => void,
) {
  switch (workerMessage.type) {
    case 'linterrequest': {
      const lintErrors = lintCode(workerMessage.filename, workerMessage.code)
      sendMessage(createLinterResultMessage(workerMessage.filename, lintErrors))
      break
    }
  }
}
