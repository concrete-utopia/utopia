import { createUtopiColor } from '../utopi-color-helpers'
import type { light } from './light'

export type ThemeObject = typeof light

export type ThemeVariableObject = Record<string, string>

export function generateCssVariablesFromThemeObject(
  themeObject: ThemeObject,
  pathModifier: string = '',
): [ThemeObject, ThemeVariableObject] {
  const valuesObject = { ...themeObject }
  const variablesObject: ThemeVariableObject = {}

  Object.entries(themeObject).forEach((entry) => {
    const [key, value] = entry
    const finalPath = `--utopitheme${pathModifier}-${key}`

    const newValue = createUtopiColor(value.cssValue, finalPath)
    valuesObject[key as keyof typeof light] = newValue
    variablesObject[finalPath] = value.cssValue
  })

  return [valuesObject, variablesObject]
}
