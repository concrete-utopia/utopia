import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import type { Config } from 'tailwindcss'

// taken from `@xengine/tailwindcss-class-parser`
type ParsedTailwindClassVariant = { value: string } & Record<string, unknown>
type ParsedTailwindValueDef = { value: string; class: Array<string> } & Record<string, unknown>

export type ParsedTailwindClass = {
  property: string
  value: string
  variants: Array<ParsedTailwindClassVariant>
  negative: boolean
  valueDef: ParsedTailwindValueDef
} & Record<string, unknown>

export function parseTailwindClass(className: string, config: Config | null) {
  try {
    return TailwindClassParser.parse(className, config ?? undefined)
  } catch (e) {
    return { kind: 'error', error: e }
  }
}

export function getTailwindClassName(parsedClass: ParsedTailwindClass, config: Config | null) {
  try {
    return TailwindClassParser.classname(parsedClass, config)
  } catch {
    return ''
  }
}
