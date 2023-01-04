import { objectFilter, forEachValue } from '../../../core/shared/object-utils'
import { createUtopiColor, UtopiColor } from '../utopi-color-helpers'
import { ThemeObject, FlatThemeObject, PartialThemesParent, PartialThemeObject } from './types'
import { IcnColor, IcnResultingColor, Theme } from '../..'
import { findLastIndex } from '../../../core/shared/array-utils'

export type PartialThemeVariableObject = Record<string, string>
export type ThemeVariableObject = Record<string, string | PartialThemeVariableObject>

function UtopiColorOrNot(
  thing: UtopiColor | PartialThemeObject | string | undefined,
): thing is UtopiColor {
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

  const partialThemesObject: PartialThemesParent = objectFilter<ThemeObject>(
    (value) => !UtopiColorOrNot(value),
    themeObject,
  )

  forEachValue((value, key) => {
    const finalPath = `--utopitheme-${key}`

    const newValue = createUtopiColor(value.cssValue, finalPath)
    valuesObject[key] = newValue
  }, mainThemeObject)

  forEachValue((partialTheme, name) => {
    const partialThemeValues: PartialThemeObject = { ...partialTheme }

    forEachValue((value, key) => {
      if (!UtopiColorOrNot(value)) {
        partialThemeValues[key as 'name'] = value ?? 'unnamed-partial-theme'
      } else if (UtopiColorOrNot(value)) {
        const finalPath = `--utopitheme-${key}`

        const newValue = createUtopiColor(value.cssValue, finalPath)
        partialThemeValues[key as keyof Omit<PartialThemeObject, 'name'>] = newValue
      }
    }, partialTheme)

    valuesObject[name] = partialThemeValues
  }, partialThemesObject)

  return valuesObject
}

export function generateCssVariablesFromThemeObject(themeObject: ThemeObject): ThemeVariableObject {
  const variablesObject: ThemeVariableObject = {}

  const mainThemeObject = objectFilter<FlatThemeObject>(
    (value) => UtopiColorOrNot(value),
    themeObject,
  )

  const partialThemesObject: PartialThemesParent = objectFilter<ThemeObject>(
    (value) => !UtopiColorOrNot(value),
    themeObject,
  )

  forEachValue((value, key) => {
    const finalPath = `--utopitheme-${key}`
    variablesObject[finalPath] = value.cssValue
  }, mainThemeObject)

  forEachValue((partialTheme, name) => {
    const partialThemeVariables: PartialThemeVariableObject = {}

    forEachValue((value, key) => {
      if (!UtopiColorOrNot(value) && key === 'name') {
        partialThemeVariables[key] = value ?? 'unnamed-partial-theme'
      } else if (UtopiColorOrNot(value)) {
        const finalPath = `--utopitheme-${key}`

        const newValue = createUtopiColor(value.cssValue, finalPath)
        partialThemeVariables[finalPath] = newValue.cssValue
      }
    }, partialTheme)

    variablesObject[name] = partialThemeVariables
  }, partialThemesObject)

  return variablesObject
}

export function IcnColorOrNot(thing: IcnColor | string): thing is IcnColor {
  const IcnColors: IcnColor[] = [
    'main',
    'secondary',
    'subdued',
    'primary',
    'warning',
    'error',
    'component',
    'on-highlight-main',
    'on-highlight-secondary',
    'on-light-main',
    'darkgray',
    'black',
  ]

  if (findLastIndex<IcnColor>((color) => color === thing, IcnColors) > -1) {
    return true
  }

  return false
}

export function getIconColor(intent: IcnColor, currentTheme: Theme): IcnResultingColor {
  if (currentTheme === 'light') {
    switch (intent) {
      case 'main':
        return 'black'
      case 'secondary':
        return 'gray'
      case 'subdued':
        return 'lightgray'
      case 'primary':
        return 'blue'
      case 'warning':
        return 'orange'
      case 'error':
        return 'red'
      case 'component':
        return 'purple'
      case 'on-highlight-main':
        return 'white'
      case 'on-highlight-secondary':
        return 'lightgray'
      case 'on-light-main':
        return 'white'
      default:
        return 'white'
    }
  } else if (currentTheme === 'dark') {
    switch (intent) {
      case 'main':
        return 'white'
      case 'secondary':
        return 'lightgray'
      case 'subdued':
        return 'gray'
      case 'primary':
        return 'blue'
      case 'component':
        return 'purple'
      case 'error':
        return 'red'
      case 'warning':
        return 'orange'
      case 'on-highlight-main':
        return 'white'
      case 'on-highlight-secondary':
        return 'lightgray'
      case 'on-light-main':
        return 'black'
      case 'black':
        return 'black'
      default:
        return 'white'
    }
  }
  return 'black'
}
