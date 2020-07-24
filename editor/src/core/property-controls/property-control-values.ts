import {
  Parser,
  parseAlternative,
  parseString,
  parseBoolean,
  parseNumber,
  parseUndefined,
  parseNull,
  ParseResult,
  descriptionParseError,
  parseAny,
  parseObject,
} from '../../utils/value-parser-utils'
import {
  AllowedEnumType,
  BaseControlDescription,
  ControlDescription,
  UnionControlDescription,
  isBaseControlDescription,
} from 'utopia-api'
import { parseColor, CSSColor } from '../../components/inspector/common/css-utils'
import {
  Either,
  foldEither,
  left,
  right,
  isRight,
  flatMapEither,
  unwrapEither,
  sequenceEither,
  isLeft,
  reduceWithEither,
} from '../shared/either'
import { compose } from '../shared/function-utils'
import {
  isJSXAttributeOtherJavaScript,
  JSXAttribute,
  jsxAttributeValue,
  jsxAttributeOtherJavaScript,
} from '../shared/element-template'
import {
  ModifiableAttribute,
  jsxSimpleAttributeToValue,
  getModifiableJSXAttributeAtPathFromAttribute,
} from '../shared/jsx-attributes'
import { PropertyPathPart } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { fastForEach } from '../shared/utils'

type Printer<T> = (value: T) => JSXAttribute

export function parseColorValue(value: unknown): ParseResult<CSSColor> {
  return foldEither(
    (l) => left(descriptionParseError(l)),
    (r) => right(r),
    parseColor(value),
  )
}

export const parseEnumValue: Parser<AllowedEnumType> = parseAlternative<AllowedEnumType>(
  [parseString, parseBoolean, parseNumber, parseUndefined, parseNull],
  'Value is not a string/boolean/number/undefined/null.',
)

function rawAndRealValueAtKey(
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
  key: PropertyPathPart,
): { rawValue: Either<string, ModifiableAttribute>; realValue: unknown } {
  const rawValueAtPath = flatMapEither(
    (v) => getModifiableJSXAttributeAtPathFromAttribute(v, PP.create([key])),
    rawValue,
  )
  const realValueAtPath =
    typeof realValue === 'object' && realValue != null ? (realValue as any)[key] : undefined
  return {
    rawValue: rawValueAtPath,
    realValue: realValueAtPath,
  }
}

function unwrapAndParseObjectValues(objectControls: {
  [prop: string]: ControlDescription
}): UnwrapperAndParser<{ [prop: string]: unknown }> {
  return (rawValue: Either<string, ModifiableAttribute>, realValue: unknown) => {
    return reduceWithEither(
      (working: { [prop: string]: unknown }, key: string) => {
        const control = objectControls[key]
        const unwrapperAndParser = unwrapperAndParserForPropertyControl(control)
        const valuesForKey = rawAndRealValueAtKey(rawValue, realValue, key)
        const innerResult = unwrapperAndParser(valuesForKey.rawValue, valuesForKey.realValue)
        if (isLeft(innerResult)) {
          return left(descriptionParseError(`Unable to parse object at key ${key}`))
        } else {
          working[key] = innerResult.value
          return right(working)
        }
      },
      {},
      Object.keys(objectControls),
    )
  }
}

// Unwrappers take the raw (from parser printer) value and real (measured from DOM) value, and return the value we're interested in
type Unwrapper = (rawValue: Either<string, ModifiableAttribute>, realValue: unknown) => unknown

function defaultUnwrapper(
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
): unknown {
  // First try to extract the simple value, then fall back to the real value
  const simpleValue = flatMapEither(jsxSimpleAttributeToValue, rawValue)
  return unwrapEither(simpleValue, realValue)
}

// For returning the JS code as a string
function jsUnwrapper(
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
): string | null {
  if (isRight(rawValue) && isJSXAttributeOtherJavaScript(rawValue.value)) {
    return rawValue.value.javascript
  } else {
    return null
  }
}

type UnwrapperAndParser<T> = (
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
) => ParseResult<T>

const defaultUnwrapFirst = <T>(parser: Parser<T>) => compose(defaultUnwrapper, parser)
const jsUnwrapFirst = (parser: Parser<string>) => compose(jsUnwrapper, parser)

function unwrapAndParseAlternative<T>(
  unwrappersAndParsers: Array<UnwrapperAndParser<T>>,
  failMessage: string,
): UnwrapperAndParser<T> {
  return (rawValue: Either<string, ModifiableAttribute>, realValue: unknown) => {
    for (const unwrapperAndParser of unwrappersAndParsers) {
      const result = unwrapperAndParser(rawValue, realValue)
      if (isRight(result)) {
        return result
      }
    }
    return left(descriptionParseError(failMessage))
  }
}

function unwrapAndParseUnionValue(
  controls: Array<ControlDescription>,
): UnwrapperAndParser<unknown> {
  const unwrappersAndParsers = controls.map(unwrapperAndParserForPropertyControl)
  return unwrapAndParseAlternative(
    unwrappersAndParsers,
    "Value can't be parsed by any of the available controls",
  )
}

export function unwrapperAndParserForBaseControl(
  control: BaseControlDescription,
): UnwrapperAndParser<unknown> {
  switch (control.type) {
    case 'boolean':
      return defaultUnwrapFirst(parseBoolean)
    case 'color':
      return defaultUnwrapFirst(parseColorValue)
    case 'componentinstance':
      return jsUnwrapFirst(parseString)
    case 'enum':
      return defaultUnwrapFirst(parseEnumValue)
    case 'eventhandler':
      return jsUnwrapFirst(parseAny)
    case 'ignore':
      return defaultUnwrapFirst(parseAny)
    case 'image':
      return defaultUnwrapFirst(parseString)
    case 'number':
      return defaultUnwrapFirst(parseNumber)
    case 'options':
      return defaultUnwrapFirst(parseAny)
    case 'popuplist':
      return defaultUnwrapFirst(parseAny)
    case 'slider':
      return defaultUnwrapFirst(parseNumber)
    case 'string':
      return defaultUnwrapFirst(parseString)
    case 'style-object':
      return defaultUnwrapFirst(parseAny)
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled control ${JSON.stringify(control)}`)
  }
}

function unwrapperAndParserForPropertyControl(
  control: ControlDescription,
): UnwrapperAndParser<unknown> {
  switch (control.type) {
    case 'boolean':
    case 'color':
    case 'componentinstance':
    case 'enum':
    case 'eventhandler':
    case 'ignore':
    case 'image':
    case 'number':
    case 'options':
    case 'popuplist':
    case 'slider':
    case 'string':
    case 'style-object':
      return unwrapperAndParserForBaseControl(control)

    case 'array':
      return unwrapperAndParserForPropertyControl(control.propertyControl)
    case 'object':
      return unwrapAndParseObjectValues(control.object)
    case 'union':
      return unwrapAndParseUnionValue(control.controls)
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled control ${JSON.stringify(control)}`)
  }
}

export function controlToUseForUnion(
  control: UnionControlDescription,
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
): ControlDescription {
  // Find the first control that parses the value
  const foundControl = control.controls.find((inner) => {
    const parser = unwrapperAndParserForPropertyControl(inner)
    const parsed = parser(rawValue, realValue)
    return isRight(parsed)
  })
  return foundControl == null ? control.controls[0] : foundControl
}

function printSimple<T>(value: T): JSXAttribute {
  return jsxAttributeValue(value)
}

function printAsString<T>(value: T): JSXAttribute {
  return jsxAttributeValue(`${value}`)
}

function printJS<T>(value: T): JSXAttribute {
  return jsxAttributeOtherJavaScript(`${value}`, `return ${value}`, [], null)
}

export function printerForBasePropertyControl(control: BaseControlDescription): Printer<unknown> {
  switch (control.type) {
    case 'boolean':
      return printSimple
    case 'color':
      return printAsString
    case 'componentinstance':
      return printJS
    case 'enum':
      return printSimple
    case 'eventhandler':
      return printJS
    case 'ignore':
      return printSimple
    case 'image':
      return printSimple
    case 'number':
      return printSimple
    case 'options':
      return printSimple
    case 'popuplist':
      return printSimple
    case 'slider':
      return printSimple
    case 'string':
      return printSimple
    case 'style-object':
      return printSimple
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}
