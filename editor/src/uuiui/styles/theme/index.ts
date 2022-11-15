import { Theme } from '../../../components/editor/store/editor-state'
import { useEditorState } from '../../../components/editor/store/store-hook'
import { dark } from './dark'
import { light } from './light'
import { ColorTheme, colorTheme, darkColorTheme, UtopiaTheme } from './utopia-theme'
import type { UtopiColor } from '../utopi-color-helpers'

export const colorThemeCssVariables = Object.keys(light).reduce((prev, curr) => {
  const parts = curr.replace(/([A-Z]|[0-9]+)/g, '-$1'),
    varName = `--${parts.toLowerCase()}`
  return { ...prev, [curr]: varName }
}, {})

// const [utopiaThemeCssVariables, utopiaThemeCssValues] =
//   generateCssVariablesFromThemeObject(UtopiaTheme)

// function generateCssVariablesFromThemeObject(themeObject: typeof UtopiaTheme) {
//   const startPath = '--utopitheme',
//     variablesObject: Record<string, any> = {},
//     valuesObject: Record<string, string> = {}

//   traverseObjectAndShadow(themeObject, variablesObject, startPath)

//   return [variablesObject, valuesObject]

//   function traverseObjectAndShadow(
//     o: Record<string, any>,
//     shadowObject: Record<string, any>,
//     path: string = '-',
//   ) {
//     Object.entries(o).forEach((entry) => {
//       const [key, value] = entry

//       //   TEMP: Check if it's a UtopiColor
//       if (value && value.value && value.cssValue && value.o) {
//         shadowObject[key] = value
//       } else if (typeof value === 'object' && value !== null) {
//         shadowObject[key] = {}
//         traverseObjectAndShadow(value, shadowObject[key], path + '-' + key)
//       } else {
//         const finalPath = path + '-' + key
//         shadowObject[key] = 'var(' + finalPath + ')'
//         valuesObject[finalPath] = typeof value === 'number' ? value + 'px' : value
//         return value
//       }
//     })
//   }
// }

Object.entries(light).forEach((entry) => {
  const [key, value] = entry
  value.value = `var(${colorThemeCssVariables[key as keyof typeof colorThemeCssVariables]})`
})

Object.entries(dark).forEach((entry) => {
  const [key, value] = entry
  value.value = `var(${colorThemeCssVariables[key as keyof typeof colorThemeCssVariables]})`
})

// TODO: don't export colorTheme anymore and just export useUtopiaTheme() hook
// prerequisites: no class components and usage of UtopiaTheme.color instead of colorTheme
export const useColorTheme = (): ColorTheme => {
  const currentTheme: Theme = useEditorState((store) => store.userState.themeConfig, 'currentTheme')
  return currentTheme === 'dark' ? darkColorTheme : colorTheme
}

export { UtopiaTheme, UtopiaStyles, colorTheme } from './utopia-theme'
export type { ColorTheme } from './utopia-theme'
// export { utopiaThemeCssVariables as UtopiaTheme }
// export { utopiaThemeCssValues }
