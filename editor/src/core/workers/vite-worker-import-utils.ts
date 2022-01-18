import TSWorker from './ts/ts-worker?worker'
import ParserPrinterWorker from './parser-printer/parser-printer.worker?worker'
import LinterWorker from './linter/linter.worker?worker'
import WatchdogWorker from './watchdog.worker?worker'

export function createTsWorker(): Worker {
  return new TSWorker()
}

export function createParserPrinterWorker(): Worker {
  return new ParserPrinterWorker()
}

export function createLinterWorker(): Worker {
  return new LinterWorker()
}

export function createWatchdogWorker(): Worker {
  return new WatchdogWorker()
}
