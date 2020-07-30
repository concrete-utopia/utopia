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
import {
  parseColor,
  CSSColor,
  printColorToJsx,
  isCSSColor,
} from '../../components/inspector/common/css-utils'
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
  mapEither,
} from '../shared/either'
import { compose } from '../shared/function-utils'
import {
  isJSXAttributeOtherJavaScript,
  JSXAttribute,
  jsxArrayValue,
  jsxAttributeValue,
  jsxAttributeOtherJavaScript,
  jsxAttributeNestedArray,
  jsxAttributeNestedObject,
  jsxPropertyAssignment,
} from '../shared/element-template'
import {
  ModifiableAttribute,
  jsxSimpleAttributeToValue,
  getModifiableJSXAttributeAtPathFromAttribute,
} from '../shared/jsx-attributes'
import { PropertyPathPart } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { fastForEach } from '../shared/utils'
import { mapToArray } from '../shared/object-utils'

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

function parseAllowedEnum(allowedValues: AllowedEnumType[]): Parser<AllowedEnumType> {
  return (v: unknown) => {
    const parsed = parseEnumValue(v)
    return flatMapEither(
      (parsedValue) =>
        allowedValues.includes(parsedValue)
          ? right(parsedValue)
          : left(descriptionParseError('Value is not an allowed enum')),
      parsed,
    )
  }
}

function rawAndRealValueAtIndex(
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
  index: number,
): { rawValue: Either<string, ModifiableAttribute>; realValue: unknown } {
  const rawValueAtIndex = flatMapEither(
    (v) => getModifiableJSXAttributeAtPathFromAttribute(v, PP.create([index])),
    rawValue,
  )
  const realValueAtIndex = Array.isArray(realValue) ? realValue[index] : undefined

  return {
    rawValue: rawValueAtIndex,
    realValue: realValueAtIndex,
  }
}

function unwrapAndParseArrayValues(
  propertyControl: ControlDescription,
): UnwrapperAndParser<Array<unknown>> {
  return (rawValue: Either<string, ModifiableAttribute>, realValue: unknown) => {
    const unwrapperAndParser = unwrapperAndParserForPropertyControl(propertyControl)
    const unwrappedValue = defaultUnwrapper(rawValue, realValue)
    const length = Array.isArray(unwrappedValue) ? unwrappedValue.length : 0

    let parsedContents: Array<unknown> = []
    for (let i = 0; i < length; i++) {
      const valuesForIndex = rawAndRealValueAtIndex(rawValue, realValue, i)
      const innerResult = unwrapperAndParser(valuesForIndex.rawValue, valuesForIndex.realValue)
      if (isLeft(innerResult)) {
        return left(descriptionParseError(`Unable to parse object at index ${i}`))
      } else {
        parsedContents.push(innerResult.value)
      }
    }

    return right(parsedContents)
  }
}

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
      return jsUnwrapFirst(parseAny)
    case 'enum':
      return defaultUnwrapFirst(parseAllowedEnum(control.options))
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
    case 'styleobject':
      return defaultUnwrapFirst(parseAny)
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled control ${JSON.stringify(control)}`)
  }
}

export function unwrapperAndParserForPropertyControl(
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
    case 'styleobject':
      return unwrapperAndParserForBaseControl(control)

    case 'array':
      return unwrapAndParseArrayValues(control.propertyControl)
    case 'object':
      return unwrapAndParseObjectValues(control.object)
    case 'union':
      return unwrapAndParseUnionValue(control.controls)
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled control ${JSON.stringify(control)}`)
  }
}

function findFirstSuitableControl(
  controls: Array<ControlDescription>,
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
): ControlDescription {
  // Find the first control that parses the value
  const foundControl = controls.find((inner) => {
    const parser = unwrapperAndParserForPropertyControl(inner)
    const parsed = parser(rawValue, realValue)
    return isRight(parsed)
  })
  return foundControl ?? controls[0]
}

export function controlToUseForUnion(
  control: UnionControlDescription,
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
): ControlDescription {
  return findFirstSuitableControl(control.controls, rawValue, realValue)
}

function printSimple<T>(value: T): JSXAttribute {
  return jsxAttributeValue(value)
}

function printColor(value: unknown): JSXAttribute {
  if (isCSSColor(value) || value === undefined) {
    return printColorToJsx(value)
  } else {
    return jsxAttributeValue(`${value}`)
  }
}

function printJS<T>(value: T): JSXAttribute {
  return jsxAttributeOtherJavaScript(`${value}`, `return ${value}`, [], null)
}

export function printerForBasePropertyControl(control: BaseControlDescription): Printer<unknown> {
  switch (control.type) {
    case 'boolean':
      return printSimple
    case 'color':
      return printColor
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
    case 'styleobject':
      return printSimple
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}

function printerForArray<T>(control: ControlDescription): Printer<Array<T>> {
  const printContentsValue = printerForPropertyControl(control)
  return (array: Array<T>): JSXAttribute => {
    const printedContents = array.map((value) => jsxArrayValue(printContentsValue(value)))
    return jsxAttributeNestedArray(printedContents)
  }
}

function printerForObject(objectControls: {
  [prop: string]: ControlDescription
}): Printer<{ [prop: string]: unknown }> {
  return (object: { [prop: string]: unknown }): JSXAttribute => {
    const printedContents = mapToArray((value, key) => {
      const valueControl = objectControls[key]
      const valuePrinter =
        valueControl == null ? printSimple : printerForPropertyControl(valueControl)
      return jsxPropertyAssignment(key, valuePrinter(value))
    }, object)

    return jsxAttributeNestedObject(printedContents)
  }
}

function printerForUnion<T>(controls: Array<ControlDescription>): Printer<T> {
  return (value: T): JSXAttribute => {
    const controlToUse = findFirstSuitableControl(controls, left('ignore'), value)
    const printerToUse = printerForPropertyControl(controlToUse)
    return printerToUse(value)
  }
}

export function printerForPropertyControl(control: ControlDescription): Printer<unknown> {
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
    case 'styleobject':
      return printerForBasePropertyControl(control)

    case 'array':
      return printerForArray(control.propertyControl) as Printer<unknown> // Why???!!
    case 'object':
      return printerForObject(control.object) as Printer<unknown> // Why???!!
    case 'union':
      return printerForUnion(control.controls)
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}
