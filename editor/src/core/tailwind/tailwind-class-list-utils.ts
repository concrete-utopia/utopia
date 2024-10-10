import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import type { Config } from 'tailwindcss/types/config'
import { mapDropNulls } from '../shared/array-utils'

export type ParsedTailwindClass = {
  property: string
  value: string
  variants: unknown[]
  negative: boolean
} & Record<string, unknown>

export type TailwindClassParserResult =
  | { type: 'unparsed'; className: string }
  | { type: 'parsed'; ast: ParsedTailwindClass }

export function getParsedClassList(
  classList: string,
  config: Config | null,
): TailwindClassParserResult[] {
  return classList.split(' ').map((c) => {
    const result = TailwindClassParser.parse(c, config ?? undefined)
    if (result.kind === 'error') {
      return { type: 'unparsed', className: c }
    }
    return { type: 'parsed', ast: result }
  })
}

export function getClassListFromParsedClassList(
  parsedClassList: TailwindClassParserResult[],
  config: Config | null,
): string {
  return parsedClassList
    .map((c) => {
      if (c.type === 'unparsed') {
        return c.className
      }
      return TailwindClassParser.classname(
        c.ast as any, // FIXME the types are not exported from @xengine/tailwindcss-class-parser
        config ?? undefined,
      )
    })
    .filter((part) => part != null && part?.length > 0)
    .join(' ')
}

export type ClassListTransform = (
  parsedClassList: TailwindClassParserResult[],
) => TailwindClassParserResult[]

export interface PropertiesToUpdate {
  [property: string]: string
}

export const addNewClasses =
  (propertiesToAdd: PropertiesToUpdate): ClassListTransform =>
  (parsedClassList: TailwindClassParserResult[]) => {
    const existingProperties = new Set(
      mapDropNulls((cls) => (cls.type !== 'parsed' ? null : cls.ast.property), parsedClassList),
    )

    const newClasses: TailwindClassParserResult[] = mapDropNulls(
      ([prop, value]) =>
        existingProperties.has(prop)
          ? null
          : {
              type: 'parsed',
              ast: { property: prop, value: value, variants: [], negative: false },
            },
      Object.entries(propertiesToAdd),
    )

    const classListWithNewClasses = [...parsedClassList, ...newClasses]
    return classListWithNewClasses
  }

export const updateExistingClasses =
  (propertiesToUpdate: PropertiesToUpdate): ClassListTransform =>
  (parsedClassList: TailwindClassParserResult[]) => {
    const classListWithUpdatedClasses: TailwindClassParserResult[] = parsedClassList.map((cls) => {
      if (cls.type !== 'parsed' || cls.ast.variants.length > 0) {
        return cls
      }
      const updatedProperty = propertiesToUpdate[cls.ast.property]
      if (updatedProperty == null) {
        return cls
      }
      return {
        type: 'parsed',
        ast: { property: cls.ast.property, value: updatedProperty, variants: [], negative: false },
      }
    })
    return classListWithUpdatedClasses
  }

export const removeClasses =
  (propertiesToRemove: string[]): ClassListTransform =>
  (parsedClassList: TailwindClassParserResult[]) => {
    const propertiesToRemoveSet = new Set(propertiesToRemove)
    const classListWithRemovedClasses = parsedClassList.filter((cls) => {
      if (cls.type !== 'parsed' || cls.ast.variants.length > 0) {
        return cls
      }
      return !propertiesToRemoveSet.has(cls.ast.property)
    })
    return classListWithRemovedClasses
  }
