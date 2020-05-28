import * as csstree from 'css-tree'
import {
  cssKeyword,
  CSSKeyword,
  CSSNumber,
  cssNumber,
  CSSUnknownArrayItem,
  cssUnknownArrayItem,
  LengthUnits,
  ParsedCurlyBrace,
  parsedCurlyBrace,
  LengthUnit,
} from '../../components/inspector/common/css-utils'
import { Either, isRight, left, right, Right, sequenceEither } from '../../core/shared/either'
import * as csstreemissing from '../../missing-types/css-tree'
import {
  arrayIndexNotPresentParseError,
  descriptionParseError,
  parseAlternative,
  Parser,
} from '../../utils/value-parser-utils'

export function getLexerMatches(
  propertyName: string,
  propertyValue: unknown,
  syntaxNameToFilter?: string,
): Either<string, Array<LexerMatch>> {
  // todo support for number values
  if (typeof propertyValue === 'string') {
    const ast = csstree.parse(propertyValue, {
      context: 'value',
      positions: true,
    })
    const lexerMatch = (csstree as any).lexer.matchProperty(propertyName, ast)
    if (lexerMatch.error === null && ast.type === 'Value') {
      if (syntaxNameToFilter != null) {
        const filtered = lexerMatch.matched.match.filter(
          (m: LexerMatch) => 'name' in m.syntax && m.syntax.name === syntaxNameToFilter,
        )
        return right(filtered)
      } else {
        return right(lexerMatch.matched.match)
      }
    } else {
      return left(lexerMatch.error.message)
    }
  }
  return left(`Property ${propertyName}'s value is not a string`)
}

function parseUnknownCssValue(value: unknown): Either<string, CSSUnknownArrayItem> {
  if (isLexerMatch(value) && value.match.length === 1) {
    const leaf = value.match[0]
    if (isLexerToken(leaf)) {
      // TODO do not use an array item
      return right(cssUnknownArrayItem(leaf.token, true))
    }
  }
  return left('leaf is not leaf, hah')
}

// Keywords

export const parseCSSKeyword: Parser<CSSKeyword> = (match: unknown) => {
  if (isLexerToken(match) && match.syntax != null && match.syntax.type === 'Keyword') {
    return right(cssKeyword(match.syntax.name))
  } else {
    return left(descriptionParseError('Leaf is not a keyword'))
  }
}

export function parseCSSValidKeyword<T extends string>(valid: Array<T>): Parser<CSSKeyword<T>> {
  return function(value: unknown) {
    const parsed = parseCSSKeyword(value)
    if (isRight(parsed)) {
      if (valid.includes(parsed.value.value as T)) {
        return parsed as Right<CSSKeyword<T>>
      } else {
        return left(descriptionParseError(`${value} is not valid keyword`))
      }
    } else {
      return parsed
    }
  }
}

// Numbers

export const parsePercentage: Parser<CSSNumber> = (value: unknown) => {
  if (isLexerMatch(value) && value.match.length === 1) {
    const match = value.match[0]
    if (isLexerToken(match)) {
      if (match.node.type === 'Percentage') {
        const number = Number(match.node.value)
        if (!isNaN(number)) {
          return right(cssNumber(number, '%'))
        } else {
          return left(descriptionParseError(`${match.node.value} is not a valid percentage number`))
        }
      }
    }
  }
  return left(descriptionParseError('leaf is not Percentage'))
}

export const parseLength: Parser<CSSNumber> = (value: unknown) => {
  if (isLexerMatch(value)) {
    if (value.match.length === 1 && value.match[0] != null) {
      const leaf = value.match[0]
      if (isLexerToken(leaf)) {
        if (leaf.node.type === 'Dimension') {
          const number = Number(leaf.node.value)
          const unit = leaf.node.unit ?? 'px'
          if (!isNaN(number) && LengthUnits.includes(unit as LengthUnit)) {
            return right(cssNumber(number, unit as LengthUnit))
          }
        } else if (leaf.node.type === 'Number' && leaf.node.value === '0') {
          return right(cssNumber(0, null))
        }
      }
    } else {
      return left(arrayIndexNotPresentParseError(0))
    }
  }
  return left(descriptionParseError('leaf is not Dimension'))
}

export const parseLengthPercentage: Parser<CSSNumber> = (value: unknown) => {
  if (isLexerMatch(value) && value.match.length === 1) {
    return parseAlternative<CSSNumber>(
      [parseLength, parsePercentage],
      'Could not parse length-percentage',
    )(value.match[0])
  }
  return left(descriptionParseError('Could not parse length-percentage'))
}

export function parseWholeValue<T>(parser: Parser<T>): Parser<T> {
  return function(match: unknown) {
    if (Array.isArray(match) && match.length === 1) {
      return parser(match[0])
    } else {
      return left(descriptionParseError(`Match ${JSON.stringify(match)} is not an array`))
    }
  }
}

// Curly Braces
export function parseCurlyBraces<T>(
  min: number,
  max: number,
  parsers: Array<Parser<T>>,
): Parser<ParsedCurlyBrace<T>> {
  return function(match: unknown) {
    if (Array.isArray(match) && match.length >= min && match.length <= max) {
      const parsed = sequenceEither(
        match.map((m) => parseAlternative(parsers, 'Match is not valid curly brace value.')(m)),
      )
      if (isRight(parsed)) {
        return right(parsedCurlyBrace(parsed.value))
      } else {
        return parsed
      }
    }
    return left(descriptionParseError('Lexer element is not a match'))
  }
}

// Type is very much in flex, if you find it doesn't match the data, fix it please
export type LexerToken<T extends string = string> = {
  syntax: csstreemissing.Syntax.Keyword<T> | null
  token: string
  node: csstree.CssNode
}

function isLexerToken(leaf: unknown): leaf is LexerToken<string> {
  const anyLeaf = leaf as any
  return (
    typeof anyLeaf === 'object' &&
    anyLeaf.token != null &&
    anyLeaf.node != null &&
    anyLeaf.node.loc != null
  )
}

// Type is very much in flex, if you find it doesn't match the data, fix it please
export type LexerMatch = {
  syntax: csstreemissing.Syntax.SyntaxItem
  match: Array<LexerElement>
}

export function isLexerMatch(parent: unknown): parent is LexerMatch {
  const anyParent = parent as any
  return anyParent.match != null && Array.isArray(anyParent.match)
}

export type LexerElement = LexerMatch | LexerToken<string>
