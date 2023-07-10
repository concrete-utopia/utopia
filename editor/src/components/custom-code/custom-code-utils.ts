import pathParse from 'path-parse'
import * as path from 'path'
import Utils from '../../utils/utils'
import type { ErrorMessages } from '../editor/store/editor-state'
import type { MultiFileBuildResult } from '../../core/workers/common/worker-types'

export function normalizeName(importOrigin: string, toImport: string): string {
  const looksLikeAPath = toImport.startsWith('.') || toImport.startsWith('/')
  // Try to handle the difference between paths within the project
  // and things that are module names of dependencies.
  if (looksLikeAPath) {
    if (toImport.startsWith('/')) {
      return stripExtension(toImport.slice(1))
    } else {
      const parsedOrigin = pathParse(importOrigin)
      const normalizedPath = path.normalize(`${parsedOrigin.dir}/${toImport}`)
      if (normalizedPath.startsWith('/')) {
        return stripExtension(normalizedPath.slice(1))
      } else {
        return stripExtension(normalizedPath)
      }
    }
  } else {
    return toImport
  }
}

export function stripExtension(name: string): string {
  if (name.endsWith('.js') || name.endsWith('.ts')) {
    return name.slice(0, name.length - 3)
  }
  if (name.endsWith('.jsx') || name.endsWith('.tsx')) {
    return name.slice(0, name.length - 4)
  }
  return name
}

export function getAllErrorsFromBuildResult(buildResult: MultiFileBuildResult): ErrorMessages {
  return Object.keys(buildResult).reduce((acc, filename) => {
    return {
      ...acc,
      [filename]: buildResult[filename].errors,
    }
  }, {})
}
