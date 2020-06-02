import * as fs from 'fs'

import { createRequire } from 'module'

export function resolveRequirePath(absoluteFilePath: string, moduleToResolve: string) {
  const localRequire = createRequire(absoluteFilePath)
  const resolvedPath = localRequire.resolve(moduleToResolve)
  return resolvedPath
}
