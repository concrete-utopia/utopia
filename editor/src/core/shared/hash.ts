import { murmur3 } from 'murmurhash-js'

export function hashObject(input: any): string {
  const data = JSON.stringify(input)
  return murmur3(data).toString(16)
}
