import { TypeDefinitions } from '../shared/npm-dependency-types'
import { ProjectContents, ParseSuccess } from '../shared/project-file-types'
import {
  createParseFileMessage,
  createPrintCodeMessage,
} from './parser-printer/parser-printer-worker'
import { createLinterRequestMessage } from './linter/linter-worker'
import { NewBundlerWorker, BundlerWorker } from './bundler-bridge'
import { workerForFile } from './utils'
import {
  createWatchdogInitMessage,
  createHeartbeatResponseMessage,
  DEFAULT_HEARTBEAT_TIMEOUT_MS,
  DEFAULT_HEARTBEAT_INTERVAL_MS,
  createWatchdogTerminateMessage,
} from './watchdog-worker'
import { UtopiaTsWorkers, FileContent } from './common/worker-types'

export class UtopiaTsWorkersImplementation implements UtopiaTsWorkers {
  private bundlerWorker: NewBundlerWorker

  constructor(
    bundlerWorker: BundlerWorker,
    private parserPrinterWorker: ParserPrinterWorker,
    private linterWorker: LinterWorker,
    private watchdogWorker: WatchdogWorker,
  ) {
    this.bundlerWorker = new NewBundlerWorker(bundlerWorker)
  }

  sendInitMessage(typeDefinitions: TypeDefinitions, projectContents: ProjectContents) {
    this.bundlerWorker.sendInitMessage(typeDefinitions, projectContents)
  }

  sendUpdateFileMessage(filename: string, content: FileContent) {
    this.bundlerWorker.sendUpdateFileMessage(filename, content)
  }

  sendParseFileMessage(filename: string, content: string) {
    this.parserPrinterWorker.sendParseFileMessage(filename, content)
  }

  sendPrintCodeMessage(parseSuccess: ParseSuccess) {
    this.parserPrinterWorker.sendPrintCodeMessage(parseSuccess)
  }

  sendLinterRequestMessage(filename: string, content: string) {
    this.linterWorker.sendLinterRequestMessage(filename, content)
  }

  addBundleResultEventListener(handler: (e: MessageEvent) => void) {
    this.bundlerWorker.addBundleResultEventListener(handler)
  }

  removeBundleResultEventListener(handler: (e: MessageEvent) => void) {
    this.bundlerWorker.removeBundleResultEventListener(handler)
  }

  addParserPrinterEventListener(handler: (e: MessageEvent) => void) {
    this.parserPrinterWorker.addParseFileResultEventListener(handler)
  }

  removeParserPrinterEventListener(handler: (e: MessageEvent) => void) {
    this.parserPrinterWorker.removeParseFileResultEventListener(handler)
  }

  addLinterResultEventListener(handler: (e: MessageEvent) => void) {
    this.linterWorker.addLinterResultEventListener(handler)
  }

  removeLinterResultEventListener(handler: (e: MessageEvent) => void) {
    this.linterWorker.removeLinterResultEventListener(handler)
  }

  initWatchdogWorker(projectId: string): void {
    this.watchdogWorker.initWatchdogWorker(projectId)
  }

  addHeartbeatRequestEventListener(handler: (e: MessageEvent) => void): void {
    this.watchdogWorker.addHeartbeatRequestEventListener(handler)
  }

  sendHeartbeatResponseMessage(id: NodeJS.Timer, projectId: string, safeMode: boolean) {
    this.watchdogWorker.sendHeartbeatResponseMessage(id, projectId, safeMode)
  }
}

export interface ParserPrinterWorker {
  sendParseFileMessage(filename: string, content: string): void

  sendPrintCodeMessage(parseSuccess: ParseSuccess): void

  addParseFileResultEventListener(handler: (e: MessageEvent) => void): void

  removeParseFileResultEventListener(handler: (e: MessageEvent) => void): void
}

export class RealParserPrinterWorker implements ParserPrinterWorker {
  private worker: Worker
  constructor() {
    this.worker = workerForFile('editor/parserPrinterWorker.js')
  }

  sendParseFileMessage(filename: string, content: string) {
    this.worker.postMessage(createParseFileMessage(filename, content))
  }

  sendPrintCodeMessage(parseSuccess: ParseSuccess) {
    this.worker.postMessage(createPrintCodeMessage(parseSuccess, true))
  }

  addParseFileResultEventListener(handler: (e: MessageEvent) => void) {
    this.worker.addEventListener('message', handler)
  }

  removeParseFileResultEventListener(handler: (e: MessageEvent) => void) {
    this.worker.removeEventListener('message', handler)
  }
}

export interface LinterWorker {
  sendLinterRequestMessage(filename: string, content: string): void

  addLinterResultEventListener(handler: (e: MessageEvent) => void): void

  removeLinterResultEventListener(handler: (e: MessageEvent) => void): void
}

export class RealLinterWorker implements LinterWorker {
  private worker: Worker
  constructor() {
    this.worker = workerForFile('editor/linterWorker.js')
  }

  sendLinterRequestMessage(filename: string, content: string) {
    this.worker.postMessage(createLinterRequestMessage(filename, content))
  }

  addLinterResultEventListener(handler: (e: MessageEvent) => void) {
    this.worker.addEventListener('message', handler)
  }

  removeLinterResultEventListener(handler: (e: MessageEvent) => void) {
    this.worker.removeEventListener('message', handler)
  }
}
export interface WatchdogWorker {
  initWatchdogWorker(projectId: string): void

  addHeartbeatRequestEventListener(handler: (e: MessageEvent) => void): void

  sendHeartbeatResponseMessage(id: NodeJS.Timer, projectId: string, safeMode: boolean): void
}

export class RealWatchdogWorker implements WatchdogWorker {
  private worker: Worker
  private setIntervalId: NodeJS.Timer | null = null

  constructor() {
    this.worker = workerForFile('editor/watchdogWorker.js')
  }

  initWatchdogWorker(projectId: string): void {
    if (!document.hidden) {
      this.sendWatchdogInitMessage(projectId)
    }
    const handleVisibilityChange = () => {
      if (document.hidden) {
        this.sendWatchdogTerminateMessage()
      } else {
        this.sendWatchdogInitMessage(projectId)
      }
    }

    document.addEventListener('visibilitychange', handleVisibilityChange, false)

    const handleInitResponse = (event: MessageEvent) => {
      const message = event.data
      if (message.type === 'watchdoginitresponse') {
        this.setIntervalId = message.setIntervalId
      }
    }
    this.worker.addEventListener('message', handleInitResponse)
  }

  sendWatchdogInitMessage(projectID: string): void {
    this.worker.postMessage(
      createWatchdogInitMessage(
        projectID,
        DEFAULT_HEARTBEAT_TIMEOUT_MS,
        DEFAULT_HEARTBEAT_INTERVAL_MS,
      ),
    )
  }

  sendWatchdogTerminateMessage(): void {
    if (this.setIntervalId != null) {
      this.worker.postMessage(createWatchdogTerminateMessage(this.setIntervalId))
    }
  }

  addHeartbeatRequestEventListener(handler: (e: MessageEvent) => void): void {
    this.worker.addEventListener('message', handler)
  }

  removeHeartbeatRequestEventListener(handler: (e: MessageEvent) => void): void {
    this.worker.removeEventListener('message', handler)
  }

  sendHeartbeatResponseMessage(id: NodeJS.Timer, projectId: string, safeMode: boolean): void {
    this.worker.postMessage(createHeartbeatResponseMessage(id, projectId, safeMode))
  }
}

export class MockUtopiaTsWorkers implements UtopiaTsWorkers {
  sendInitMessage(typeDefinitions: TypeDefinitions, projectContents: ProjectContents) {
    // empty
  }

  sendUpdateFileMessage(filename: string, content: FileContent, emitBuild: boolean) {
    // empty
  }

  sendParseFileMessage(filename: string, content: string) {
    // empty
  }

  sendPrintCodeMessage(parseSuccess: ParseSuccess) {
    // empty
  }

  sendLinterRequestMessage(filename: string, content: string) {
    // empty
  }

  addBundleResultEventListener(handler: (e: MessageEvent) => void) {
    // empty
  }

  removeBundleResultEventListener(handler: (e: MessageEvent) => void) {
    // empty
  }

  addParserPrinterEventListener(handler: (e: MessageEvent) => void) {
    // empty
  }

  removeParserPrinterEventListener(handler: (e: MessageEvent) => void) {
    // empty
  }

  addLinterResultEventListener(handler: (e: MessageEvent) => void) {
    // empty
  }

  removeLinterResultEventListener(handler: (e: MessageEvent) => void) {
    // empty
  }

  initWatchdogWorker(projectID: string): void {
    // empty
  }

  addHeartbeatRequestEventListener(handler: (e: MessageEvent) => void) {
    // empty
  }

  sendHeartbeatResponseMessage(id: NodeJS.Timer, projectId: string): void {
    // empty
  }
}
