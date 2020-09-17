import { NodeModules, ParseSuccess } from 'src/core/shared/project-file-types'
import {
  createLinterRequestMessage,
  handleMessage as handleLinterMessage,
  OutgoingLinterWorkerMessage,
} from './linter/linter-worker'
import { LinterWorker, ParserPrinterWorker, WatchdogWorker } from './workers'
import {
  createParseFileMessage,
  createPrintCodeMessage,
  handleMessage as handleParserPrinterMessage,
  ParserPrinterResultMessage,
} from './parser-printer/parser-printer-worker'
import { handleMessage as handleTSWorkerMessage } from './ts/ts-worker'
import { BundlerWorker } from './bundler-bridge'

export class FakeBundlerWorker implements BundlerWorker {
  messageListeners: Array<(ev: MessageEvent) => any> = []

  addMessageListener = (listener: (ev: MessageEvent) => any): void => {
    this.messageListeners.push(listener)
  }

  removeMessageListener = (listener: (ev: MessageEvent) => any): void => {
    this.messageListeners = this.messageListeners.filter((l) => l !== listener)
  }

  receiveMessage = (data: any) => {
    this.messageListeners.forEach((l) => {
      l(new MessageEvent('message', { data: data }))
    })
  }

  postMessage = (message: any): void => {
    handleTSWorkerMessage(message, this.receiveMessage)
  }
}

export class FakeParserPrinterWorker implements ParserPrinterWorker {
  messageListeners: Array<(ev: MessageEvent) => any> = []

  addParseFileResultEventListener = (listener: (ev: MessageEvent) => any): void => {
    this.messageListeners.push(listener)
  }

  removeParseFileResultEventListener = (listener: (ev: MessageEvent) => any): void => {
    this.messageListeners = this.messageListeners.filter((l) => l !== listener)
  }

  receiveMessage = (data: ParserPrinterResultMessage): void => {
    this.messageListeners.forEach((l) => {
      l(new MessageEvent('message', { data: data }))
    })
  }

  sendParseFileMessage = (filename: string, content: string): void => {
    handleParserPrinterMessage(createParseFileMessage(filename, content), this.receiveMessage)
  }

  sendPrintCodeMessage = (parseSuccess: ParseSuccess): void => {
    handleParserPrinterMessage(createPrintCodeMessage(parseSuccess, false), this.receiveMessage)
  }
}

export class FakeLinterWorker implements LinterWorker {
  linterMessageListeners: Array<(ev: MessageEvent) => any> = []

  receiveLinterMessage = (data: OutgoingLinterWorkerMessage): void => {
    this.linterMessageListeners.forEach((l) => {
      l(new MessageEvent('message', { data: data }))
    })
  }

  sendLinterRequestMessage = (filename: string, content: string): void => {
    handleLinterMessage(createLinterRequestMessage(filename, content), this.receiveLinterMessage)
  }

  addLinterResultEventListener = (listener: (e: MessageEvent) => void): void => {
    this.linterMessageListeners.push(listener)
  }

  removeLinterResultEventListener = (listener: (e: MessageEvent) => void): void => {
    this.linterMessageListeners = this.linterMessageListeners.filter((l) => l !== listener)
  }
}

export class FakeWatchdogWorker implements WatchdogWorker {
  initWatchdogWorker(_projectID: string): void {
    // empty
  }

  sendWatchdogTerminateMessage(): void {
    // empty
  }

  addHeartbeatRequestEventListener(_handler: (e: MessageEvent) => void): void {
    // empty
  }

  sendHeartbeatResponseMessage(_id: NodeJS.Timer, _projectId: string): void {
    // empty
  }
}
