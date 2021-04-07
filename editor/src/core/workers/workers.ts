import { TypeDefinitions } from '../shared/npm-dependency-types'
import { createParsePrintFilesRequest, ParseOrPrint } from './parser-printer/parser-printer-worker'
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
import { ProjectContentTreeRoot } from '../../components/assets'

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
  sendInitMessage(typeDefinitions: TypeDefinitions, projectContents: ProjectContentTreeRoot): void {
    this.bundlerWorker.sendInitMessage(typeDefinitions, projectContents, null)
  }

  sendUpdateFileMessage(filename: string, content: FileContent): void {
    this.bundlerWorker.sendUpdateFileMessage(filename, content)
  }

  sendLinterRequestMessage(filename: string, content: string): void {
    this.linterWorker.sendLinterRequestMessage(filename, content)
  }

  addBundleResultEventListener(handler: (e: MessageEvent) => void): void {
    this.bundlerWorker.addBundleResultEventListener(handler)
  }

  removeBundleResultEventListener(handler: (e: MessageEvent) => void): void {
    this.bundlerWorker.removeBundleResultEventListener(handler)
  }

  sendParsePrintMessage(files: Array<ParseOrPrint>): void {
    this.parserPrinterWorker.sendParsePrintMessage(files)
  }

  addParserPrinterEventListener(handler: (e: MessageEvent) => void): void {
    this.parserPrinterWorker.addParseFileResultEventListener(handler)
  }

  removeParserPrinterEventListener(handler: (e: MessageEvent) => void): void {
    this.parserPrinterWorker.removeParseFileResultEventListener(handler)
  }

  addLinterResultEventListener(handler: (e: MessageEvent) => void): void {
    this.linterWorker.addLinterResultEventListener(handler)
  }

  removeLinterResultEventListener(handler: (e: MessageEvent) => void): void {
    this.linterWorker.removeLinterResultEventListener(handler)
  }

  initWatchdogWorker(projectId: string): void {
    this.watchdogWorker.initWatchdogWorker(projectId)
  }

  addHeartbeatRequestEventListener(handler: (e: MessageEvent) => void): void {
    this.watchdogWorker.addHeartbeatRequestEventListener(handler)
  }

  sendHeartbeatResponseMessage(id: NodeJS.Timer, projectId: string, safeMode: boolean): void {
    this.watchdogWorker.sendHeartbeatResponseMessage(id, projectId, safeMode)
  }
}

export interface ParserPrinterWorker {
  sendParsePrintMessage: (files: Array<ParseOrPrint>) => void

  addParseFileResultEventListener(handler: (e: MessageEvent) => void): void

  removeParseFileResultEventListener(handler: (e: MessageEvent) => void): void
}

export class RealParserPrinterWorker implements ParserPrinterWorker {
  private worker: Worker
  constructor() {
    this.worker = workerForFile('editor/parserPrinterWorker.js')
  }

  sendParsePrintMessage(files: Array<ParseOrPrint>): void {
    this.worker.postMessage(createParsePrintFilesRequest(files))
  }

  addParseFileResultEventListener(handler: (e: MessageEvent) => void): void {
    this.worker.addEventListener('message', handler)
  }

  removeParseFileResultEventListener(handler: (e: MessageEvent) => void): void {
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

  sendLinterRequestMessage(filename: string, content: string): void {
    this.worker.postMessage(createLinterRequestMessage(filename, content))
  }

  addLinterResultEventListener(handler: (e: MessageEvent) => void): void {
    this.worker.addEventListener('message', handler)
  }

  removeLinterResultEventListener(handler: (e: MessageEvent) => void): void {
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
  sendInitMessage(
    _typeDefinitions: TypeDefinitions,
    _projectContents: ProjectContentTreeRoot,
  ): void {
    // empty
  }

  sendUpdateFileMessage(_filename: string, _content: FileContent, _emitBuild: boolean): void {
    // empty
  }

  sendParsePrintMessage(files: Array<ParseOrPrint>): void {
    // empty
  }

  sendLinterRequestMessage(_filename: string, _content: string): void {
    // empty
  }

  addBundleResultEventListener(_handler: (e: MessageEvent) => void): void {
    // empty
  }

  removeBundleResultEventListener(_handler: (e: MessageEvent) => void): void {
    // empty
  }

  addParserPrinterEventListener(_handler: (e: MessageEvent) => void): void {
    // empty
  }

  removeParserPrinterEventListener(_handler: (e: MessageEvent) => void): void {
    // empty
  }

  addLinterResultEventListener(_handler: (e: MessageEvent) => void): void {
    // empty
  }

  removeLinterResultEventListener(_handler: (e: MessageEvent) => void): void {
    // empty
  }

  initWatchdogWorker(_projectID: string): void {
    // empty
  }

  addHeartbeatRequestEventListener(_handler: (e: MessageEvent) => void): void {
    // empty
  }

  sendHeartbeatResponseMessage(_id: NodeJS.Timer, _projectId: string): void {
    // empty
  }
}
