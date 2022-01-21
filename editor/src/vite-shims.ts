if (typeof (globalThis as any).global === 'undefined') {
  ;(globalThis as any).global = globalThis
}

self.process = process ?? {}
self.process.cwd = process.cwd ?? new Function()
import { Buffer } from 'buffer'
self.Buffer = Buffer
