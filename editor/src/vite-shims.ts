if (typeof (globalThis as any).global === 'undefined') {
  ;(globalThis as any).global = globalThis
}

if (typeof process !== 'undefined') {
  self.process = process ?? {}
} else {
  self.process = {} as any
}
self.process.cwd = process.cwd ?? new Function()

// import * as Buffer from 'buffer'
import { Buffer } from 'buffer'
self.Buffer = Buffer as any
