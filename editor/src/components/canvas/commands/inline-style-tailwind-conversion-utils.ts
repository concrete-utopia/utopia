import { CssToTailwindTranslator } from 'css-to-tailwind-translator'
import styleObjectToCSSString from 'style-object-to-css-string'
import type { Config } from 'tailwindcss'
import resolveConfig from 'tailwindcss/resolveConfig'
import { generateRules } from 'tailwindcss/lib/lib/generateRules'
import { createContext } from 'tailwindcss/lib/lib/setupContextUtils'

function getPropAndValue(node: unknown): { prop: string; value: string } | null {
  if (typeof node !== 'object' || node == null) {
    return null
  }
  if (
    'prop' in node &&
    'value' in node &&
    typeof node.prop === 'string' &&
    typeof node.value === 'string'
  ) {
    const { prop, value } = node
    return { prop, value }
  }
  return null
}

export function createTailwindParser(config: Config | null) {
  const context = createContext(resolveConfig(config ?? ({} as any)))

  return {
    classesToCss(classes: Set<string>): Array<{ prop: string; value: string }> | null {
      const sorted = [...classes].sort((a, z) => {
        if (a === z) return 0
        if (a < z) return -1
        return 1
      })
      const rules = generateRules([...sorted], context)
      if (rules == null) {
        return null
      }
      const css = rules.flatMap((rule: any) => {
        const props = rule
          .at(1)
          .nodes.flatMap((node: unknown): Array<{ prop: string; value: string }> => {
            const propAndValue = getPropAndValue(node)
            if (propAndValue == null) {
              return []
            }
            return [propAndValue]
          })
        return [{ value: props }]
      })
      return css
    },
  }
}

export function convertInlineStyleToTailwind(inlineStyle: Record<string, number | string>): string {
  const styleString = styleObjectToCSSString(inlineStyle)
  const cssString = `body { ${styleString} }`
  const { data } = CssToTailwindTranslator(cssString)
  return data[0].resultVal
}

export function convertTailwindToInlineStyle(
  tailwind: string,
  config: Config | null,
): Record<string, string | number> | null {
  const parser = createTailwindParser(config)
  const styles = parser.classesToCss(new Set(tailwind.split(' ')))
  if (styles == null) {
    return null
  }
  return styles.reduce((acc, style) => {
    acc[style.prop] = style.value
    return acc
  }, {} as Record<string, string | number>)
}
