import { objectFilter, forEachValue } from '../../../core/shared/object-utils'
import { createUtopiColor, UtopiColor } from '../utopi-color-helpers'
import { ThemeObject, FlatThemeObject, SubThemesParent, SubThemeObject } from './types'

export type SubThemeVariableObject = Record<string, string>
export type ThemeVariableObject = Record<string, string | SubThemeVariableObject>

function UtopiColorOrNot(thing: UtopiColor | SubThemeObject | string): thing is UtopiColor {
  if (typeof (thing as UtopiColor).cssValue === 'string') {
    return true
  }
  return false
}

export function generateColorThemeObject(themeObject: ThemeObject): ThemeObject {
  const valuesObject: ThemeObject = { ...themeObject }

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
      }
    }, subTheme)

    valuesObject[name] = subThemeValues
  }, subThemesObject)

  return valuesObject
}

export function generateCssVariablesFromThemeObject(themeObject: ThemeObject): ThemeVariableObject {
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
    variablesObject[finalPath] = value.cssValue
  }, mainThemeObject)

  forEachValue((subTheme, name) => {
    const subThemeVariables: SubThemeVariableObject = {}

    forEachValue((value, key) => {
      if (!UtopiColorOrNot(value) && key === 'name') {
        subThemeVariables[key] = value
      } else if (UtopiColorOrNot(value)) {
        const finalPath = `--utopitheme-${name}-${key}`

        const newValue = createUtopiColor(value.cssValue, finalPath)
        subThemeVariables[finalPath] = newValue.cssValue
      }
    }, subTheme)

    variablesObject[name] = subThemeVariables
  }, subThemesObject)

  return variablesObject
}
