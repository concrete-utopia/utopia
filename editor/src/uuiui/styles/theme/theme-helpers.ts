import { UtopiColor } from '../utopi-color-helpers'
import { light } from './light'

export type ThemeObject = {
  [key in keyof typeof light]: UtopiColor
}

export type ThemeVariableObject = {
  [key in keyof typeof light]: string
}

export function generateCssVariablesFromThemeObject(
  themeObject: ThemeObject,
  basePath: string = '-',
): [ThemeObject, ThemeVariableObject] {
  const valuesObject = {}
  const variablesObject: Record<string, string> = {}

  traverseObjectAndShadow(themeObject, valuesObject as ThemeObject, basePath)

  return [valuesObject as ThemeObject, variablesObject as ThemeVariableObject]

  function traverseObjectAndShadow(o: ThemeObject, shadowObject: ThemeObject, path: string = '-') {
    Object.entries(o).forEach((entry) => {
      const [key, value] = entry
      const finalPath = path + '-' + key

      value.value = finalPath
      shadowObject[key as keyof typeof light] = value
      variablesObject[finalPath] = value.cssValue
    })

    return o
  }
}
