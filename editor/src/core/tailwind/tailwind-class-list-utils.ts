import * as TailwindClassParser from '@xengine/tailwindcss-class-parser'
import type { Config } from 'tailwindcss/types/config'
import { mapDropNulls } from '../shared/array-utils'
import type { StyleMediaSizeModifier, StyleModifier } from '../../components/canvas/canvas-types'

export type ParsedTailwindClass = {
  property: string
  value: string
  variants: { type: string; value: string }[]
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
    .filter((part) => part != null && part.length > 0)
    .join(' ')
}

export type ClassListTransform = (
  parsedClassList: TailwindClassParserResult[],
) => TailwindClassParserResult[]

export interface PropertiesToUpdate {
  [property: string]: { newValue: string; modifiers: StyleModifier[] }
}

export interface PropertiesToRemove {
  [property: string]: {
    modifiers: StyleModifier[]
  }
}

export const addNewClasses =
  (propertiesToAdd: PropertiesToUpdate): ClassListTransform =>
  (parsedClassList: TailwindClassParserResult[]) => {
    const existingProperties = new Set(
      mapDropNulls((cls) => (cls.type !== 'parsed' ? null : cls.ast.property), parsedClassList),
    )

    const newClasses: TailwindClassParserResult[] = mapDropNulls(
      ([prop, update]) =>
        existingProperties.has(prop)
          ? null
          : {
              type: 'parsed',
              ast: { property: prop, value: update.newValue, variants: [], negative: false },
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
      if (!shouldUpdateClass(cls, propertiesToUpdate)) {
        return cls
      }
      const propertyToUpdate = propertiesToUpdate[cls.ast.property]
      return {
        type: 'parsed',
        ast: {
          property: cls.ast.property,
          value: propertyToUpdate.newValue,
          variants: cls.ast.variants,
          negative: false,
        },
      }
    })
    return classListWithUpdatedClasses
  }

export const removeClasses =
  (propertiesToRemove: PropertiesToRemove): ClassListTransform =>
  (parsedClassList: TailwindClassParserResult[]) => {
    const classListWithRemovedClasses = parsedClassList.filter((cls) => {
      if (!shouldUpdateClass(cls, propertiesToRemove)) {
        return cls
      }
      return propertiesToRemove[cls.ast.property] == null
    })
    return classListWithRemovedClasses
  }

function getTailwindSizeVariant(modifiers: StyleModifier[]): string | null {
  const mediaModifier = modifiers.find((m): m is StyleMediaSizeModifier => m.type === 'media-size')
  if (mediaModifier == null) {
    return null
  }
  if (mediaModifier.modifierOrigin?.type !== 'tailwind') {
    return null
  }
  return mediaModifier.modifierOrigin.variant
}

function shouldUpdateClass(
  cls: TailwindClassParserResult,
  propertiesToUpdate: PropertiesToUpdate | PropertiesToRemove,
): cls is TailwindClassParserResult & { type: 'parsed' } {
  if (cls.type !== 'parsed') {
    return false
  }
  const propertyToUpdate = propertiesToUpdate[cls.ast.property]
  if (propertyToUpdate == null) {
    // this property is not in the list
    return false
  }
  const sizeVariantToUpdate = getTailwindSizeVariant(propertyToUpdate.modifiers)
  if (sizeVariantToUpdate == null && cls.ast.variants.length > 0) {
    // we need to update the default property value but this class has variants
    return false
  }
  if (
    sizeVariantToUpdate != null &&
    !variantsHasMediaSizeVariant(cls.ast.variants, sizeVariantToUpdate)
  ) {
    // we need to update a specific size variant but this class doesn't have it
    return false
  }
  return true
}

function variantsHasMediaSizeVariant(
  variants: { type: string; value: string }[],
  sizeVariant: string,
): boolean {
  return variants.some((v) => v.type === 'media' && v.value === sizeVariant)
}
