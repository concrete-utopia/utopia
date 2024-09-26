import type { UtopiaJSXComponent } from '../shared/element-template'
import { isUtopiaJSXComponent } from '../shared/element-template'
import { isParseSuccess } from '../shared/project-file-types'
import { parseCode } from '../workers/parser-printer/parser-printer'

export function getComponentFromCode(componentName: string, code: string): UtopiaJSXComponent {
  const parseResult = parseCode('test.jsx', [], code, 'do-not-apply-steganography')
  if (isParseSuccess(parseResult)) {
    for (const topLevelElement of parseResult.topLevelElements) {
      if (isUtopiaJSXComponent(topLevelElement) && topLevelElement.name === componentName) {
        return topLevelElement
      }
    }
    throw new Error(`Could not find component ${componentName}`)
  } else {
    throw new Error(`Not a parse success: ${parseResult.type}`)
  }
}
