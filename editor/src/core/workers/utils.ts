import { BASE_URL } from '../../common/env-vars'
import { URL_HASH, PRODUCTION_CONFIG } from '../shared/detect-env'

export function workerForFile(fileName: string): Worker {
  return new Worker(`${BASE_URL(PRODUCTION_CONFIG)}${fileName}?hash=${URL_HASH}`)
}
