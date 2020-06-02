import * as fs from 'fs'
import * as R from 'ramda'

import { createRequire } from 'module'

function monkeyPatchRequire(extensionsToMutate: NodeJS.RequireExtensions, fileExtension: string) {
  console.log('mutating extensions', fileExtension)
  const realRequire = extensionsToMutate[fileExtension]
  extensionsToMutate[fileExtension] = (...args) => {
    try {
      const realResult = realRequire!(...args)
      return realResult
    } catch (e) {
      return {}
    }
  }
}

function monkeyPatchExtensions(extensionsToMutate: NodeJS.RequireExtensions) {
  Object.keys(extensionsToMutate).forEach((fileExtension) =>
    monkeyPatchRequire(extensionsToMutate, fileExtension),
  )
}

export function resolveRequirePath(packageEntryPath: string, toRequire: string) {
  try {
    const localRequire = createRequire(packageEntryPath)
    monkeyPatchExtensions(localRequire.extensions)
    const before = Object.keys(localRequire.cache)
    localRequire(packageEntryPath)
    const after = Object.keys(localRequire.cache)
    const newImports = R.difference(after, before)
    return newImports
  } catch (e) {
    return []
  }
}
