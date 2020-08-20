import { BASE_URL, URL_HASH } from '../../common/env-vars'
import { setBranchNameFromURL } from '../../utils/branches'

export function workerForFile(fileName: string): Worker {
  let workerURL = new URL(`${BASE_URL}${fileName}`)
  let workerQueryParams = workerURL.searchParams
  workerQueryParams.set('hash', URL_HASH)
  setBranchNameFromURL(workerQueryParams)
  return new Worker(workerURL.href)
}
