/* eslint-disable */

// Copy pasted and adapted from Remix: https://github.com/remix-run/remix/blob/8779b24d0e51cc49a887d16afab9789557b80124/packages/remix-dev/config/routesConvention.ts

import path from 'path-browserify'

export const routeModuleExts = ['.js', '.jsx', '.ts', '.tsx', '.md', '.mdx']

export function isRouteModuleFile(filename: string): boolean {
  return routeModuleExts.includes(path.extname(filename))
}

export let paramPrefixChar = '$' as const
export let escapeStart = '[' as const
export let escapeEnd = ']' as const

export let optionalStart = '(' as const
export let optionalEnd = ')' as const

export function isSegmentSeparator(checkChar: string | undefined) {
  if (!checkChar) return false
  return ['/', '.'].includes(checkChar)
}
