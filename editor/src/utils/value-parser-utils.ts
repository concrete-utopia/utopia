import React from 'react'
import type { MapLike } from 'typescript'
import type { Either } from '../core/shared/either'
import {
  flatMapEither,
  isLeft,
  isRight,
  left,
  leftMapEither,
  mapEither,
  reduceWithEither,
  right,
  traverseEither,
} from '../core/shared/either'
import type { ComponentRendererComponent } from '../components/canvas/ui-jsx-canvas-renderer/component-renderer-component'
import { difference } from '../core/shared/set-utils'

export interface ArrayIndexNotPresentParseError {
  type: 'ARRAY_INDEX_NOT_PRESENT_PARSE_ERROR'
  index: number
}

export function arrayIndexNotPresentParseError(index: number): ArrayIndexNotPresentParseError {
  return {
    type: 'ARRAY_INDEX_NOT_PRESENT_PARSE_ERROR',
    index: index,
  }
}

export interface ArrayIndexParseError {
  type: 'ARRAY_INDEX_PARSE_ERROR'
  index: number
  innerError: ParseError
}

export function arrayIndexParseError(index: number, innerError: ParseError): ArrayIndexParseError {
  return {
    type: 'ARRAY_INDEX_PARSE_ERROR',
    index: index,
    innerError: innerError,
  }
}

export interface ObjectFieldNotPresentParseError {
  type: 'OBJECT_FIELD_NOT_PRESENT_PARSE_ERROR'
  field: string
}

export function objectFieldNotPresentParseError(field: string): ObjectFieldNotPresentParseError {
  return {
    type: 'OBJECT_FIELD_NOT_PRESENT_PARSE_ERROR',
    field: field,
  }
}

export interface ObjectFieldParseError {
  type: 'OBJECT_FIELD_PARSE_ERROR'
  field: string
  innerError: ParseError
}

export function objectFieldParseError(
  field: string,
  innerError: ParseError,
): ObjectFieldParseError {
  return {
    type: 'OBJECT_FIELD_PARSE_ERROR',
    field: field,
    innerError: innerError,
  }
}

export interface DescriptionParseError {
  type: 'DESCRIPTION_PARSE_ERROR'
  description: string
}

export function descriptionParseError(description: string): DescriptionParseError {
  return {
    type: 'DESCRIPTION_PARSE_ERROR',
    description: description,
  }
}

export type ParseError =
  | ArrayIndexNotPresentParseError
  | ArrayIndexParseError
  | ObjectFieldNotPresentParseError
  | ObjectFieldParseError
  | DescriptionParseError

export function isDescriptionParseError(value: unknown): value is DescriptionParseError {
  return (
    typeof value === 'object' && value != null && (value as any).type === 'DESCRIPTION_PARSE_ERROR'
  )
}

export interface ParseErrorDetails {
  path: string
  description: string
}

export function parseErrorDetails(path: string, description: string): ParseErrorDetails {
  return {
    path: path,
    description: description,
  }
}

export function getParseErrorDetails(parseError: ParseError): ParseErrorDetails {
  function innerDetails(pathSoFar: string[], parseErrorHere: ParseError): ParseErrorDetails {
    switch (parseErrorHere.type) {
      case 'DESCRIPTION_PARSE_ERROR':
        return parseErrorDetails(pathSoFar.join('.'), parseErrorHere.description)
      case 'OBJECT_FIELD_PARSE_ERROR':
        return innerDetails(pathSoFar.concat(parseErrorHere.field), parseErrorHere.innerError)
      case 'OBJECT_FIELD_NOT_PRESENT_PARSE_ERROR':
        return parseErrorDetails(
          pathSoFar.concat(parseErrorHere.field).join('.'),
          'Missing object field',
        )
      case 'ARRAY_INDEX_PARSE_ERROR':
        return innerDetails(pathSoFar.concat(`${parseErrorHere.index}`), parseErrorHere.innerError)
      case 'ARRAY_INDEX_NOT_PRESENT_PARSE_ERROR':
        return parseErrorDetails(
          pathSoFar.concat(`${parseErrorHere.index}`).join(','),
          'Missing array index.',
        )
      default:
        const _exhaustiveCheck: never = parseErrorHere
        throw new Error(`Unhandled type ${JSON.stringify(parseErrorHere)}`)
    }
  }

  return innerDetails([], parseError)
}

export type ParseResult<T> = Either<ParseError, T>

export type Parser<T> = (v: unknown) => ParseResult<T>

export function flatMapParser<T, U>(
  parser: Parser<T>,
  handleResult: (t: T) => ParseResult<U>,
): Parser<U> {
  return (value: unknown) => {
    const parserResult = parser(value)
    return flatMapEither(handleResult, parserResult)
  }
}

export function parseAny(value: unknown): ParseResult<any> {
  return right(value)
}

// Wraps around an existing parser to add to the "error stack" an ObjectFieldParseError.
export function objectValueParserWithError<V>(
  parser: (v: unknown, key: string) => ParseResult<V>,
): (v: unknown, key: string) => ParseResult<V> {
  return (v: unknown, key: string) => {
    const parsed = parser(v, key)
    return leftMapEither((err) => {
      return objectFieldParseError(key, err)
    }, parsed)
  }
}

type PropertyParserFn<T> = (v: unknown, key: string) => ParseResult<T>
type PropertyParser<T> = PropertyParserFn<T> | { optional: PropertyParserFn<T> }
type ObjectParserSpec<Type> = {
  [Key in keyof Type]: PropertyParser<Type[Key]>
}

export const optionalProp = <T>(
  parser: PropertyParserFn<T>,
): { optional: PropertyParserFn<T> } => ({ optional: parser })

interface ObjectParserOptions {
  allowUnknownKeys: boolean
}

const DefaultObjectParserOptions: ObjectParserOptions = {
  allowUnknownKeys: false,
}

export function objectParser<Type>(
  spec: ObjectParserSpec<Type>,
  options: Partial<ObjectParserOptions> = DefaultObjectParserOptions,
): (v: unknown) => ParseResult<Type> {
  return (value: unknown) => {
    if (typeof value !== 'object') {
      return left(descriptionParseError('Not an object'))
    }
    if (Array.isArray(value)) {
      return left(descriptionParseError('Object is an array'))
    }
    if (value == null) {
      return left(descriptionParseError('Object is `null`'))
    }

    const allProps: Set<string> = new Set(Object.keys(value))
    const checkedProps: Set<string> = new Set()

    const partialResult: Partial<Type> = {}

    for (const [key, parser] of Object.entries<PropertyParser<unknown>>(spec)) {
      const optional = typeof parser !== 'function'
      if (!(key in value)) {
        if (optional) {
          continue
        }
        return left(objectFieldNotPresentParseError(key))
      }

      const parserFn = typeof parser === 'function' ? parser : parser.optional
      const withErrorParser = objectValueParserWithError(parserFn as any)
      const parsed = withErrorParser((value as any)[key], key)
      if (isLeft(parsed)) {
        return parsed
      }

      ;(partialResult as any)[key] = parsed.value
      checkedProps.add(key)
    }

    const unknownProps = [...difference(allProps, checkedProps)]

    const allowUnknownKeys = options != null && options.allowUnknownKeys
    if (!allowUnknownKeys && unknownProps.length > 0) {
      return left(descriptionParseError(`Found unknown props: ${unknownProps.join(', ')}`))
    }

    return right(partialResult as Type)
  }
}

// Parses a single key out of an object.
export function objectKeyParser<V>(
  parser: (v: unknown, k: string) => ParseResult<V>,
  key: string,
): (v: unknown) => ParseResult<V> {
  return (value: unknown) => {
    if (typeof value === 'object' && !Array.isArray(value) && value != null) {
      const withErrorParser = objectValueParserWithError(parser)
      const valueAsObject: any = value
      if (key in valueAsObject) {
        return withErrorParser(valueAsObject[key], key)
      } else {
        return left(objectFieldNotPresentParseError(key))
      }
    } else {
      return left(descriptionParseError('Not an object.'))
    }
  }
}

// Parses a single key out of an object, returning
// undefined if the key doesn't exist.
export function optionalObjectKeyParser<V>(
  parser: (v: unknown, k: string) => ParseResult<V>,
  key: string,
): (v: unknown) => ParseResult<V | undefined> {
  return (value: unknown) => {
    if (typeof value === 'object' && !Array.isArray(value) && value != null) {
      const withErrorParser = objectValueParserWithError(parser)
      const valueAsObject: any = value
      if (key in valueAsObject) {
        return withErrorParser(valueAsObject[key], key)
      } else {
        return right(undefined)
      }
    } else {
      return left(descriptionParseError('Not an object.'))
    }
  }
}

export function parseObject<V>(
  objectValueParser: (v: unknown, key: string) => ParseResult<V>,
): Parser<MapLike<V>> {
  return (value: unknown) => {
    if (typeof value === 'object' && !Array.isArray(value) && value != null) {
      const valueAsObject: any = value
      const withErrorParser = objectValueParserWithError(objectValueParser)
      return reduceWithEither<string, ParseError, MapLike<V>>(
        (working: MapLike<V>, objectKey: string) => {
          return mapEither((parsedObjectValue) => {
            return {
              ...working,
              [objectKey]: parsedObjectValue,
            }
          }, withErrorParser(valueAsObject[objectKey], objectKey))
        },
        {},
        Object.keys(valueAsObject),
      )
    } else {
      return left(descriptionParseError('Not an object.'))
    }
  }
}

// Wraps around an existing parser to add to the "error stack" an ArrayIndexParseError.
export function arrayValueParserWithError<V>(
  parser: (v: unknown, index: number) => ParseResult<V>,
): (v: unknown, index: number) => ParseResult<V> {
  return (v: unknown, index: number) => {
    const parsed = parser(v, index)
    return leftMapEither((err) => {
      return arrayIndexParseError(index, err)
    }, parsed)
  }
}

export function parseArray<V>(
  arrayValueParser: (v: unknown, index: number) => ParseResult<V>,
): Parser<Array<V>> {
  return (value: unknown) => {
    if (typeof value === 'object' && Array.isArray(value) && value != null) {
      const valueAsArray: Array<unknown> = value
      const withErrorParser = arrayValueParserWithError(arrayValueParser)
      return traverseEither(withErrorParser, valueAsArray)
    } else {
      return left(descriptionParseError('Not an array.'))
    }
  }
}

export function parseTuple<T extends Array<unknown>>(
  arrayValueParser: (v: unknown, index: number) => ParseResult<unknown>,
  length: number,
): Parser<T> {
  return (value: unknown) => {
    if (typeof value === 'object' && Array.isArray(value) && value != null) {
      const valueAsArray: Array<unknown> = value
      const withErrorParser = arrayValueParserWithError(arrayValueParser)
      const parsedArray = traverseEither(withErrorParser, valueAsArray)
      return flatMapEither(
        (arr) =>
          arr.length === length
            ? right(arr as T)
            : left(descriptionParseError('Tuple has incorrect length.')),
        parsedArray,
      )
    } else {
      return left(descriptionParseError('Not a tuple.'))
    }
  }
}

export function parseBoolean(value: unknown): ParseResult<boolean> {
  if (typeof value === 'boolean') {
    return right(value)
  } else {
    return left(descriptionParseError('Not a boolean.'))
  }
}

export function parseFalse(value: unknown): ParseResult<false> {
  if (typeof value === 'boolean' && value === false) {
    return right(value)
  } else {
    return left(descriptionParseError('Not a boolean false.'))
  }
}

export function parseNumber(value: unknown): ParseResult<number> {
  if (typeof value === 'number') {
    return right(value)
  } else {
    return left(descriptionParseError('Not a number.'))
  }
}

export function parseString(value: unknown): ParseResult<string> {
  if (typeof value === 'string') {
    return right(value)
  } else {
    return left(descriptionParseError('Not a string.'))
  }
}

export type JSXParsedType =
  | 'null'
  | 'string'
  | 'number'
  | 'html'
  | 'internal-component'
  | 'external-component'
  | 'unknown'

export interface JSXParsedValue {
  type: JSXParsedType
  name: string
}

export function parseJsx(_: unknown, value: unknown): ParseResult<JSXParsedValue> {
  if (React.isValidElement(value)) {
    const { type } = value
    // if this is an html element, we want to return the tag name
    if (typeof type === 'string') {
      return right({
        type: 'html',
        name: type,
      })
    }
    if (value.hasOwnProperty('props')) {
      const props = (value as any).props
      if (props.hasOwnProperty('elementToRender') === true) {
        return right({
          type: 'internal-component',
          name: typeof props.elementToRender === 'string' ? props.elementToRender : 'JSX',
        })
      }
    }
    // if it is a spied component, the original type is stored in theOriginalType
    if (type.hasOwnProperty('theOriginalType')) {
      const originalType = (type as any).theOriginalType
      // if it is an internal component, it has an originalName property
      if (originalType.hasOwnProperty('originalName') === true) {
        const originalName = (originalType as ComponentRendererComponent).originalName
        return right({
          type: 'internal-component',
          name: originalName ?? 'JSX',
        })
      }

      // if it is an external component, try returning displayName or name
      if (originalType.hasOwnProperty('displayName') === true) {
        return right({
          type: 'external-component',
          name: (originalType as ComponentRendererComponent).displayName ?? 'JSX',
        })
      }
      if (originalType.hasOwnProperty('name') === true) {
        return right({
          type: 'external-component',
          name: (originalType as ComponentRendererComponent).name ?? 'JSX',
        })
      }
    }

    // if it is an external component, try returning displayName or name
    if (type.hasOwnProperty('displayName')) {
      return right({
        type: 'external-component',
        name: (type as any).displayName ?? 'JSX',
      })
    }
    if (type.hasOwnProperty('name')) {
      return right({
        type: 'external-component',
        name: (type as any).name ?? 'JSX',
      })
    }
  }

  if (typeof value === 'string') {
    return right({
      type: 'string',
      name: value,
    })
  }

  if (typeof value === 'number') {
    return right({
      type: 'number',
      name: value.toString(),
    })
  }

  if (value == null) {
    return right({
      type: 'null',
      name: 'null',
    })
  }

  return right({
    type: 'unknown',
    name: 'JSX',
  })
}

export function parseEnum<E extends string | number>(possibleValues: ReadonlyArray<E>): Parser<E> {
  return (value: unknown) => {
    for (const possibleValue of possibleValues) {
      if (possibleValue === value) {
        return right(possibleValue)
      }
    }
    return left(descriptionParseError('Not a member of an enum.'))
  }
}

export function parseConstant<T extends string | number | boolean | bigint | undefined | null>(
  constant: T,
): Parser<T> {
  return (value: unknown) => {
    if (value === constant) {
      return right(constant)
    } else {
      return left(descriptionParseError(`Value was not ${constant}.`))
    }
  }
}

export const parseNull: Parser<null> = parseConstant(null)

export const parseUndefined: Parser<undefined> = parseConstant(undefined)

export function parseNullable<T>(valueParser: Parser<T>): Parser<T | null> {
  return (value: unknown) => {
    if (value === null) {
      return right(null)
    } else {
      return valueParser(value)
    }
  }
}

export function parseFunction<T>(value: unknown): ParseResult<T> {
  if (typeof value === 'function') {
    return right(value as any)
  } else {
    return left(descriptionParseError('Not a function.'))
  }
}

export function parseAlternative<T>(parsers: Array<Parser<T>>, failMessage: string): Parser<T> {
  return (v: unknown) => {
    for (const parser of parsers) {
      const result = parser(v)
      if (isRight(result)) {
        return result
      }
    }
    return left(descriptionParseError(failMessage))
  }
}

export function parseValidate<T>(
  parser: Parser<T>,
  predicate: (value: T) => boolean,
  failMessage: string,
): Parser<T> {
  return function (value: unknown) {
    const parsed = parser(value)
    if (isRight(parsed)) {
      if (predicate(parsed.value)) {
        return parsed
      } else {
        return left(descriptionParseError(`${JSON.stringify(parsed.value)} is not valid`))
      }
    } else {
      return parsed
    }
  }
}
