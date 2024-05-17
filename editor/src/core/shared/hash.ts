import * as murmur from 'murmurhash3js'
import { UID_LENGTH } from './uid-utils'

export function hashObject(input: any): string {
  const data = JSON.stringify(input)
  return murmur.x86.hash128(data).substring(0, UID_LENGTH)
}
