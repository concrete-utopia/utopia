import { BASE_URL, URL_HASH } from '../../common/env-vars'

export function workerForFile(fileName: string): Worker {
  return new Worker(`${BASE_URL}${fileName}?hash=${URL_HASH}`)
}
