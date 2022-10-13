import { Spec } from 'immutability-helper'

const SupportedCommands = ['$set']

function isCommand(key: string) {
  return key.startsWith('$')
}

function isSupportedCommand(key: string) {
  SupportedCommands.indexOf(key) > -1
}

// TODO: modify Spec type so it only supports the $set command
export function mergePatches<T>(patches: Array<Spec<T>>) {
  let modifiedPaths: Set<string> = new Set()
  let compressedPatches: Array<Spec<T>> = []

  for (let i = patches.length - 1; i >= 0; i--) {
    const currentPaths = getPaths(patches[i])
    if (currentPaths.some((path) => !modifiedPaths.has(path))) {
      compressedPatches = [patches[i], ...compressedPatches]
    }
    currentPaths.forEach((p) => modifiedPaths.add(p))
  }

  return compressedPatches
}

function getPaths<T>(patch: Spec<T>) {
  let paths: Array<string> = []
  getPathsInner(patch, '', paths)
  return paths
}

function getPathsInner<T, K extends keyof Spec<T>>(
  patch: Spec<T>,
  prefix: string,
  paths: Array<string>,
) {
  if (typeof patch !== 'object') {
    return
  }
  const keys = Object.keys(patch) as Array<K>
  if (keys.length === 0) {
    return
  }
  keys.forEach((key) => {
    if (isCommand(key) && !isSupportedCommand) {
      throw new Error(`${key} is an unsupported immutability-helper command`)
    }
    if (key === '$set') {
      paths.push(prefix)
    } else {
      getPathsInner(patch[key] as Spec<T>, `${prefix}.${key}`, paths)
    }
  })
}
