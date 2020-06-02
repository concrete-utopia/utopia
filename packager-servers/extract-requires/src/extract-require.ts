import * as fs from 'fs'
import * as R from 'ramda'

import { createRequire } from 'module'

export function resolveRequirePath(absoluteFilePath: string) {
  const localRequire = createRequire(absoluteFilePath)
  const before = Object.keys(localRequire.cache)
  localRequire(absoluteFilePath)
  const after = Object.keys(localRequire.cache)
  const newImports = R.difference(after, before)
  return newImports
}
