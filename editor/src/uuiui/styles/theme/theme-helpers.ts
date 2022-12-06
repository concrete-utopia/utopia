import { objectFilter, forEachValue } from '../../../core/shared/object-utils'
import { createUtopiColor, UtopiColor } from '../utopi-color-helpers'
import { SubThemeObject } from './subthemes'
import { ThemeObject, FlatThemeObject, SubThemesParent } from './types'

export type ThemeVariableObject = Record<string, string>

function UtopiColorOrNot(thing: UtopiColor | SubThemeObject | string): thing is UtopiColor {
  if (typeof (thing as UtopiColor).cssValue === 'string') {
    return true
  }
  return false
}

export function generateCssVariablesFromThemeObject(
  themeObject: ThemeObject,
): [ThemeObject, ThemeVariableObject] {
  const valuesObject: ThemeObject = { ...themeObject }
  const variablesObject: ThemeVariableObject = {}

  const mainThemeObject = objectFilter<FlatThemeObject>(
    (value) => UtopiColorOrNot(value),
    themeObject,
  )

  const subThemesObject: SubThemesParent = objectFilter<ThemeObject>(
    (value) => !UtopiColorOrNot(value),
    themeObject,
  )

  forEachValue((value, key) => {
    const finalPath = `--utopitheme-${key}`

    const newValue = createUtopiColor(value.cssValue, finalPath)
    valuesObject[key] = newValue
    variablesObject[finalPath] = value.cssValue
  }, mainThemeObject)

  forEachValue((subTheme, name) => {
    const subThemeValues: SubThemeObject = { ...subTheme }

    forEachValue((value, key) => {
      if (!UtopiColorOrNot(value) && key === 'name') {
        subThemeValues[key] = value
      } else if (UtopiColorOrNot(value)) {
        const finalPath = `--utopitheme-${name}-${key}`

        const newValue = createUtopiColor(value.cssValue, finalPath)
        subThemeValues[key as keyof Omit<SubThemeObject, 'name'>] = newValue
        variablesObject[finalPath] = value.cssValue
      }
    }, subTheme)

    valuesObject[name] = subThemeValues
  }, subThemesObject)

  return [{ ...valuesObject }, variablesObject]
}
