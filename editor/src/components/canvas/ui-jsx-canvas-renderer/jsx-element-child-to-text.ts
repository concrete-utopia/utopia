import type { JSXElementChild } from '../../../core/shared/element-template'
import { optionalMap } from '../../../core/shared/optional-utils'
import { assertNever } from '../../../core/shared/utils'

export function jsxElementToDataPath(element: JSXElementChild): (string | number)[] | null {
  switch (element.type) {
    case 'JSX_ELEMENT':
    case 'JSX_TEXT_BLOCK':
    case 'JSX_FRAGMENT':
    case 'JSX_MAP_EXPRESSION':
    case 'JSX_CONDITIONAL_EXPRESSION':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return null
    case 'JS_IDENTIFIER':
      return [element.name]
    case 'ATTRIBUTE_VALUE': {
      switch (typeof element.value) {
        case 'number':
          return [element.value]
        case 'string':
          return [element.value]
        default:
          return null
      }
    }
    case 'JS_ELEMENT_ACCESS':
      return optionalMap(
        (e) => optionalMap((onValue) => [...onValue, ...e], jsxElementToDataPath(element.onValue)),
        jsxElementToDataPath(element.element),
      )
    case 'JS_PROPERTY_ACCESS':
      return optionalMap(
        (onValue) => [...onValue, element.property],
        jsxElementToDataPath(element.onValue),
      )
    default:
      assertNever(element)
  }
}

export function jsxElementChildToText(
  element: JSXElementChild,
  prevElement: JSXElementChild | null,
  nextElement: JSXElementChild | null,
  expressionContext: 'jsx' | 'javascript',
  outermost: 'outermost' | 'inner',
): string {
  function outermostWrapInBraces(value: string): string {
    if (outermost === 'outermost' && expressionContext === 'jsx') {
      return `{${value}}`
    } else {
      return value
    }
  }
  switch (element.type) {
    case 'JSX_TEXT_BLOCK':
      return trimWhitespaces(element.text, prevElement ?? null, nextElement ?? null)
    case 'JSX_ELEMENT':
      if (element.name.baseVariable === 'br') {
        return '\n'
      }
      return ''
    case 'JSX_MAP_EXPRESSION':
      const valueToMapText = jsxElementChildToText(
        element.valueToMap,
        prevElement,
        nextElement,
        'javascript',
        'inner',
      )
      const mapFunctionText = jsxElementChildToText(
        element.mapFunction,
        prevElement,
        nextElement,
        'javascript',
        'inner',
      )
      const mapExpressionText = `${valueToMapText}.map(${mapFunctionText})`
      // when the context is jsx, we need to wrap expression in curly brackets
      return expressionContext === 'javascript' ? mapExpressionText : `{${mapExpressionText}}`
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      // when the context is jsx, we need to wrap expression in curly brackets
      return expressionContext === 'javascript'
        ? element.originalJavascript
        : `{${element.originalJavascript}}`
    case 'JSX_CONDITIONAL_EXPRESSION':
      // This is a best effort to reconstruct the original code of the conditional.
      // Maybe it would be better to store the originalJavascript in JSXConditionalExpression, but that also has its problems, e.g.
      // when we instantiate expressions from code (for example during wrapping), then we don't want to produce javascript code there from a full hierarchy of elements
      return `{ ${element.originalConditionString} ? ${jsxElementChildToText(
        element.whenTrue,
        null,
        null,
        'javascript',
        'inner',
      )} : ${jsxElementChildToText(element.whenFalse, null, null, 'javascript', 'inner')} }`
    case 'ATTRIBUTE_VALUE':
      if (typeof element.value === 'string') {
        switch (expressionContext) {
          // when the context is javascript we need to put string values between quotation marks
          case 'javascript':
            const multiline = element.value.split('\n').length > 1
            if (multiline) {
              const escaped = element.value.replace('`', '`')
              return '`' + escaped + '`'
            }
            const escaped = element.value.replace("'", "'")
            return "'" + escaped + "'"
          case 'jsx':
            return element.value
          default:
            assertNever(expressionContext)
        }
      }
      if (expressionContext == 'javascript') {
        if (element.value === null) {
          return 'null'
        }
        if (element.value === undefined) {
          return 'undefined'
        }
      }
      return element.value != null ? element.value.toString() : '{null}'
    case 'JSX_FRAGMENT':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
      return ''
    case 'JS_IDENTIFIER':
      return outermostWrapInBraces(element.name)
    case 'JS_ELEMENT_ACCESS': {
      const optionalChainedText = element.optionallyChained === 'optionally-chained' ? '?.' : ''
      return outermostWrapInBraces(
        `${jsxElementChildToText(
          element.onValue,
          null,
          null,
          'javascript',
          'inner',
        )}${optionalChainedText}[${jsxElementChildToText(
          element.element,
          null,
          null,
          'javascript',
          'inner',
        )}]`,
      )
    }
    case 'JS_PROPERTY_ACCESS': {
      const optionalChainedText = element.optionallyChained === 'optionally-chained' ? '?.' : '.'
      return outermostWrapInBraces(
        `${jsxElementChildToText(
          element.onValue,
          null,
          null,
          'javascript',
          'inner',
        )}${optionalChainedText}${element.property}`,
      )
    }
    default:
      assertNever(element)
  }
}

function trimWhitespaces(
  text: string,
  elementBefore: JSXElementChild | null,
  elementAfter: JSXElementChild | null,
): string {
  if (text.length === 0) {
    return ''
  }

  const trimmedText = text
    // split around all whitespaces, we don't want to keep newlines or repeated spaces
    .split(/\s/)
    // empty strings will appear between repeated whitespaces, we can ignore them
    .filter((s) => s.length > 0)
    // join back everything with a single space
    .join(' ')

  // when the text has a leading whitespace and there is an arbitrary block before that, we need to keep the whitespace
  const keepSpaceBefore = text[0] === ' ' && elementBefore?.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'
  // when the text has an trailing whitespace and there is an arbitrary block after that, we need to keep the whitespace
  const keepSpaceAfter =
    text[text.length - 1] === ' ' && elementAfter?.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'

  if (keepSpaceBefore && keepSpaceAfter) {
    return ' ' + trimmedText + ' '
  }

  if (keepSpaceAfter) {
    return trimmedText + ' '
  }
  if (keepSpaceBefore) {
    return ' ' + trimmedText
  }

  return trimmedText
}
