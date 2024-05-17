/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @flow */
import { SOURCE_MAP_SEPARATOR, SourceMapCache } from '../../../core/shared/code-exec-utils'
import { RawSourceMap } from '../../../core/workers/ts/ts-typings/RawSourceMap'
import { generateUUID } from '../../../utils/utils'
import { extractSourceMapUrl } from './getSourceMap'
import StackFrame from './stack-frame'
import { strToBase64, base64ToStr } from '@root/encoding/base64'
const regexExtractLocation = /\(?(.+?)(?::(\d+))?(?::(\d+))?\)?$/

function extractLocation(token: string): [string, number, number] {
  return regexExtractLocation
    .exec(token)!
    .slice(1)
    .map((v) => {
      const p = Number(v)
      if (!isNaN(p)) {
        return p
      }
      return v
    }) as any
}

const regexValidFrame_Chrome = /^\s*(at|in)\s.+(:\d+)/
const regexValidFrame_FireFox = /(^|@)\S+:\d+|.+line\s+\d+\s+>\s+(eval|Function).+/

function replaceBase64SourceMapWithCacheKey(
  stackLine: string,
  sourceMapCache: SourceMapCache,
): string {
  const splitStackLine = stackLine.split(SOURCE_MAP_SEPARATOR)
  if (splitStackLine.length < 3) {
    return stackLine
  }
  const uid = generateUUID()
  sourceMapCache[uid] = JSON.parse(base64ToStr(splitStackLine[1]))
  return `${splitStackLine[0]}${SOURCE_MAP_SEPARATOR}${uid}${splitStackLine[2]}`
}

export function parseStack(stack: string[], sourceMapCache: SourceMapCache): StackFrame[] {
  const frames = stack
    // Replace the full source maps in the lines (encoded with base64) with a unique identifier and put the sourcemap into a cache.
    // This is needed because the regular expression operations later are too slow for really long lines.
    .map((e) => replaceBase64SourceMapWithCacheKey(e, sourceMapCache))
    .filter((e) => regexValidFrame_Chrome.test(e) || regexValidFrame_FireFox.test(e))
    .map((e) => {
      if (regexValidFrame_FireFox.test(e)) {
        // Strip eval, we don't care about it
        let isEval = false
        if (/ > (eval|Function)/.test(e)) {
          e = e.replace(/ line (\d+)(?: > eval line \d+)* > (eval|Function):\d+:\d+/g, ':$1')
          isEval = true
        }
        const data = e.split(/[@]/g)
        const last = data.pop()
        return new StackFrame(data.join('@') || (isEval ? 'eval' : null), ...extractLocation(last!))
      } else {
        // Strip eval, we don't care about it
        if (e.indexOf('(eval ') !== -1) {
          // this was the original regex but it was not producing good results with our own safe function...?
          // e = e.replace(/(\(eval at [^()]*)|(\),.*$)/g, '')
          e = e.replace(/(\(eval at [^()]*)|(\)$)/g, '')
        }
        if (e.indexOf('(at ') !== -1) {
          e = e.replace(/\(at /, '(')
        }
        const data = e.trim().split(/\s+/g).slice(1)
        const last = data.pop()
        return new StackFrame(data.join(' ') || null, ...extractLocation(last!))
      }
    })
  return frames
}

/**
 * Turns an <code>Error</code>, or similar object, into a set of <code>StackFrame</code>s.
 * @alias parse
 */
function parseError(
  error: Error | string | string[] | { stack: string },
  sourceMapCache: SourceMapCache,
): StackFrame[] {
  if (error == null) {
    throw new Error('You cannot pass a null object.')
  }
  if (typeof error === 'string') {
    return parseStack(error.split('\n'), sourceMapCache)
  }
  if (Array.isArray(error)) {
    return parseStack(error, sourceMapCache)
  }
  if (typeof error.stack === 'string') {
    return parseStack(error.stack.split('\n'), sourceMapCache)
  }
  throw new Error('The error you provided does not contain a stack trace.')
}

export { parseError as parse }
export default parseError
