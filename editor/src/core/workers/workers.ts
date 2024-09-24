import type { TypeDefinitions } from '../shared/npm-dependency-types'
import { createLinterRequestMessage } from './linter/linter-worker'
import { createLinterWorker, createParserPrinterWorker, createWatchdogWorker } from 'worker-imports'
import {
  createWatchdogInitMessage,
  createHeartbeatResponseMessage,
  DEFAULT_HEARTBEAT_TIMEOUT_MS,
  DEFAULT_HEARTBEAT_INTERVAL_MS,
  createWatchdogTerminateMessage,
} from './watchdog-worker'
import {
  type UtopiaTsWorkers,
  type FileContent,
  type ParsePrintFilesRequest,
  createClearParseCacheMessage,
} from './common/worker-types'
import type { ProjectContentTreeRoot } from '../../components/assets'

export class UtopiaTsWorkersImplementation implements UtopiaTsWorkers {
  constructor(
    private parserPrinterWorker: ParserPrinterWorker,
    private linterWorker: LinterWorker,
    private watchdogWorker: WatchdogWorker,
  ) {}

  sendLinterRequestMessage(filename: string, content: string): void {
    this.linterWorker.sendLinterRequestMessage(filename, content)
  }

  sendParsePrintMessage(request: ParsePrintFilesRequest): void {
    this.parserPrinterWorker.sendParsePrintMessage(request)
  }

  sendClearParseCacheMessage(): void {
    this.parserPrinterWorker.sendClearParseCacheMessage()
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
  sendParsePrintMessage: (request: ParsePrintFilesRequest) => void

  sendClearParseCacheMessage: () => void

  addParseFileResultEventListener(handler: (e: MessageEvent) => void): void

  removeParseFileResultEventListener(handler: (e: MessageEvent) => void): void
}

export class RealParserPrinterWorker implements ParserPrinterWorker {
  private worker: Worker
  constructor() {
    this.worker = createParserPrinterWorker()
  }

  sendParsePrintMessage(request: ParsePrintFilesRequest): void {
    this.worker.postMessage(request)
  }

  sendClearParseCacheMessage(): void {
    this.worker.postMessage(createClearParseCacheMessage())
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
    this.worker = createLinterWorker()
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
    this.worker = createWatchdogWorker()
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

  sendParsePrintMessage(request: ParsePrintFilesRequest): void {
    // empty
  }

  sendClearParseCacheMessage(): void {
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
