import SimpleWorker from './worker/worker?worker'

export function createWorker() {
  return new SimpleWorker()
}
