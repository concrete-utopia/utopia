import * as murmur from 'murmurhash3js'

export function hashObject(input: any): string {
  const data = JSON.stringify(input)
  return murmur.x86.hash128(data)
}
