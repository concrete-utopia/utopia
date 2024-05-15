import type { Parser, ParseResult } from '../../utils/value-parser-utils'
import {
  parseAlternative,
  parseString,
  parseBoolean,
  parseNumber,
  parseUndefined,
  parseNull,
  descriptionParseError,
  parseAny,
  parseObject,
  parseArray,
  parseJsx,
} from '../../utils/value-parser-utils'
import type {
  AllowedEnumType,
  BaseControlDescription,
  ControlDescription,
  UnionControlDescription,
  PropertyControls,
  RegularControlDescription,
} from '../../components/custom-code/internal-property-controls'
import type { CSSColor } from '../../components/inspector/common/css-utils'
import {
  parseColor,
  printColorToJsx,
  isCSSColor,
} from '../../components/inspector/common/css-utils'
import type { Either } from '../shared/either'
import {
  foldEither,
  left,
  right,
  isRight,
  flatMapEither,
  unwrapEither,
  isLeft,
  reduceWithEither,
} from '../shared/either'
import { compose } from '../shared/function-utils'
import type { JSExpression, JSXArrayValue } from '../shared/element-template'
import {
  modifiableAttributeIsAttributeOtherJavaScript,
  jsxArrayValue,
  jsExpressionValue,
  jsExpressionOtherJavaScript,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsxPropertyAssignment,
  emptyComments,
  modifiableAttributeIsAttributeNotFound,
  jsOpaqueArbitraryStatement,
} from '../shared/element-template'
import type { ModifiableAttribute } from '../shared/jsx-attributes'
import { getModifiableJSXAttributeAtPathFromAttribute } from '../shared/jsx-attributes'
import type { PropertyPathPart } from '../shared/project-file-types'
import * as PP from '../shared/property-path'
import { forEachValue, mapToArray, objectValues } from '../shared/object-utils'
import { jsxSimpleAttributeToValue } from '../shared/jsx-attribute-utils'

type Printer<T> = (value: T) => JSExpression

export function parseColorValue(value: unknown): ParseResult<CSSColor> {
  return foldEither(
    (l) => left(descriptionParseError(l)),
    (r) => right(r),
    parseColor(value, 'hex-hash-optional'),
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
    (v) => getModifiableJSXAttributeAtPathFromAttribute(v, PP.create(index)),
    rawValue,
  )
  const realValueAtIndex = Array.isArray(realValue) ? realValue[index] : undefined

  return {
    rawValue: rawValueAtIndex,
    realValue: realValueAtIndex,
  }
}

const noRawOrRealValue: { rawValue: Either<string, ModifiableAttribute>; realValue: unknown } = {
  rawValue: left('No value'),
  realValue: undefined,
}

function unwrapAndParseArrayValues(
  propertyControl: RegularControlDescription,
): UnwrapperAndParser<Array<unknown>> {
  return (rawValue: Either<string, ModifiableAttribute>, realValue: unknown) => {
    const unwrapperAndParser = unwrapperAndParserForPropertyControl(propertyControl)
    const unwrappedValue = defaultUnwrapper(rawValue, realValue)
    if (Array.isArray(unwrappedValue)) {
      const length = unwrappedValue.length

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
    } else {
      return left(descriptionParseError(`Value isn't an array`))
    }
  }
}

function unwrapAndParseTupleValues(
  propertyControls: Array<RegularControlDescription>,
): UnwrapperAndParser<Array<unknown>> {
  return (rawValue: Either<string, ModifiableAttribute>, realValue: unknown) => {
    const unwrappedValue = defaultUnwrapper(rawValue, realValue)
    if (Array.isArray(unwrappedValue)) {
      const length = unwrappedValue.length
      const controlsLength = propertyControls.length

      let parsedContents: Array<unknown> = []
      for (let i = 0; i < controlsLength; i++) {
        // If there aren't enough values, we use undefined for missing values
        // If there are too many, we just parse up to the last value we care about
        const unwrapperAndParser = unwrapperAndParserForPropertyControl(propertyControls[i])
        const valuesForIndex =
          i < length ? rawAndRealValueAtIndex(rawValue, realValue, i) : noRawOrRealValue
        const innerResult = unwrapperAndParser(valuesForIndex.rawValue, valuesForIndex.realValue)
        if (isLeft(innerResult)) {
          return left(descriptionParseError(`Unable to parse object at index ${i}`))
        } else {
          parsedContents.push(innerResult.value)
        }
      }

      return right(parsedContents)
    } else {
      return left(descriptionParseError(`Value isn't a tuple`))
    }
  }
}

function rawAndRealValueAtKey(
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
  key: PropertyPathPart,
): { rawValue: Either<string, ModifiableAttribute>; realValue: unknown } {
  const rawValueAtPath = flatMapEither(
    (v) => getModifiableJSXAttributeAtPathFromAttribute(v, PP.create(key)),
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
  [prop: string]: RegularControlDescription
}): UnwrapperAndParser<{ [prop: string]: unknown }> {
  return (rawValue: Either<string, ModifiableAttribute>, realValue: unknown) => {
    return reduceWithEither(
      (working: { [prop: string]: unknown }, key: string) => {
        const control = objectControls[key]
        const unwrapperAndParser = unwrapperAndParserForPropertyControl(control)
        const valuesForKey = rawAndRealValueAtKey(rawValue, realValue, key)
        const missingKey = foldEither(
          (_) => false,
          (attr) => {
            return modifiableAttributeIsAttributeNotFound(attr)
          },
          valuesForKey.rawValue,
        )
        if (missingKey) {
          return right(working)
        } else {
          const innerResult = unwrapperAndParser(valuesForKey.rawValue, valuesForKey.realValue)
          if (isLeft(innerResult)) {
            return left(descriptionParseError(`Unable to parse object at key ${key}`))
          } else {
            working[key] = innerResult.value
            return right(working)
          }
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
  if (isRight(rawValue) && modifiableAttributeIsAttributeOtherJavaScript(rawValue.value)) {
    return rawValue.value.javascriptWithUIDs
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
  controls: Array<RegularControlDescription>,
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
  switch (control.control) {
    case 'checkbox':
      return defaultUnwrapFirst(parseBoolean)
    case 'color':
      return defaultUnwrapFirst(parseColorValue)
    case 'expression-input':
      return jsUnwrapFirst(parseAny)
    case 'expression-popuplist':
      return defaultUnwrapFirst(parseAny)
    case 'euler':
      return defaultUnwrapFirst(parseArray(parseAny))
    case 'matrix3':
    case 'matrix4':
      return defaultUnwrapFirst(parseArray(parseNumber))
    case 'none':
      return defaultUnwrapFirst(parseAny)
    case 'number-input':
      return defaultUnwrapFirst(parseNumber)
    case 'popuplist':
      return defaultUnwrapFirst(parseAny)
    case 'radio':
      return defaultUnwrapFirst(parseAny)
    case 'string-input':
      return defaultUnwrapFirst(parseString)
    case 'html-input':
      return defaultUnwrapFirst(parseString)
    case 'style-controls':
      return defaultUnwrapFirst(parseAny)
    case 'jsx':
      return parseJsx
    case 'vector2':
    case 'vector3':
    case 'vector4':
      return defaultUnwrapFirst(parseArray(parseNumber)) // FIXME Also needs to handle a single number
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled control ${JSON.stringify(control)}`)
  }
}

export function unwrapperAndParserForPropertyControl(
  control: RegularControlDescription,
): UnwrapperAndParser<unknown> {
  switch (control.control) {
    case 'checkbox':
    case 'color':
    case 'expression-input':
    case 'expression-popuplist':
    case 'euler':
    case 'matrix3':
    case 'matrix4':
    case 'none':
    case 'number-input':
    case 'popuplist':
    case 'radio':
    case 'string-input':
    case 'html-input':
    case 'style-controls':
    case 'vector2':
    case 'vector3':
    case 'vector4':
    case 'jsx':
      return unwrapperAndParserForBaseControl(control)

    case 'array':
      return unwrapAndParseArrayValues(control.propertyControl)
    case 'object':
      return unwrapAndParseObjectValues(control.object)
    case 'tuple':
      return unwrapAndParseTupleValues(control.propertyControls)
    case 'union':
      return unwrapAndParseUnionValue(control.controls)
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled control ${JSON.stringify(control)}`)
  }
}

function findFirstSuitableControl(
  controls: Array<RegularControlDescription>,
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
): RegularControlDescription | null {
  // Find the first control that parses the value
  for (const inner of controls) {
    const parser = unwrapperAndParserForPropertyControl(inner)
    const parsed = parser(rawValue, realValue)
    if (isRight(parsed)) {
      return inner
    }
  }
  return null
}

export function controlToUseForUnion(
  control: UnionControlDescription,
  rawValue: Either<string, ModifiableAttribute>,
  realValue: unknown,
): RegularControlDescription | null {
  return findFirstSuitableControl(control.controls, rawValue, realValue)
}

export function walkRegularControlDescriptions(
  propertyControls: PropertyControls,
  walkWith: (propertyName: string, propertyControl: RegularControlDescription) => void,
): void {
  forEachValue((propertyControl, propertyName) => {
    if (typeof propertyName === 'string') {
      walkWith(propertyName, propertyControl)
    }
  }, propertyControls)
}

export function getPropertyControlNames(propertyControls: PropertyControls): Array<string> {
  let result: Array<string> = []
  walkRegularControlDescriptions(propertyControls, (propertyName) => {
    result.push(propertyName)
  })
  return result
}

function printSimple<T>(value: T): JSExpression {
  return jsExpressionValue(value, emptyComments)
}

function printColor(value: unknown): JSExpression {
  if (isCSSColor(value) || value === undefined) {
    return printColorToJsx(value)
  } else {
    return jsExpressionValue(`${value}`, emptyComments)
  }
}

function printJS<T>(value: T): JSExpression {
  return jsExpressionOtherJavaScript([], `${value}`, `${value}`, ``, [], null, {}, emptyComments)
}

export function printerForBasePropertyControl(control: BaseControlDescription): Printer<unknown> {
  switch (control.control) {
    case 'checkbox':
      return printSimple
    case 'color':
      return printColor
    case 'expression-input':
      return printJS
    case 'expression-popuplist':
      return printJS
    case 'euler':
      return printSimple
    case 'matrix3':
    case 'matrix4':
      return printSimple
    case 'none':
      return printSimple
    case 'number-input':
      return printSimple
    case 'popuplist':
      return printSimple
    case 'radio':
      return printSimple
    case 'string-input':
      return printSimple
    case 'html-input':
      return printSimple
    case 'style-controls':
      return printSimple
    case 'vector2':
      return printSimple
    case 'vector3':
      return printSimple
    case 'vector4':
      return printSimple
    case 'jsx':
      return printSimple
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}

function printerForArray<T>(control: RegularControlDescription): Printer<Array<T>> {
  const printContentsValue = printerForPropertyControl(control)
  return (array: Array<T>): JSExpression => {
    const printedContents = array.map((value) =>
      jsxArrayValue(printContentsValue(value), emptyComments),
    )
    return jsExpressionNestedArray(printedContents, emptyComments)
  }
}

function printerForTuple(controls: Array<RegularControlDescription>): Printer<Array<unknown>> {
  return (array: Array<unknown>): JSExpression => {
    const length = Math.min(array.length, controls.length)
    let printedContents: Array<JSXArrayValue> = []

    for (let i = 0; i < length; i++) {
      const value = array[i]
      const control = controls[i]
      const printContentsValue = printerForPropertyControl(control)
      printedContents.push(jsxArrayValue(printContentsValue(value), emptyComments))
    }

    return jsExpressionNestedArray(printedContents, emptyComments)
  }
}

function printerForObject(objectControls: {
  [prop: string]: RegularControlDescription
}): Printer<{ [prop: string]: unknown }> {
  return (objectToPrint: { [prop: string]: unknown }): JSExpression => {
    const printedContents = mapToArray((value, key) => {
      const valueControl = objectControls[key]
      const valuePrinter =
        valueControl == null ? printSimple : printerForPropertyControl(valueControl)
      return jsxPropertyAssignment(key, valuePrinter(value), emptyComments, emptyComments)
    }, objectToPrint)

    return jsExpressionNestedObject(printedContents, emptyComments)
  }
}

function printerForUnion<T>(controls: Array<RegularControlDescription>): Printer<T> {
  return (value: T): JSExpression => {
    const controlToUse = findFirstSuitableControl(controls, left('ignore'), value)
    if (controlToUse == null) {
      return printSimple(value)
    } else {
      const printerToUse = printerForPropertyControl(controlToUse)
      return printerToUse(value)
    }
  }
}

export function printerForPropertyControl(control: RegularControlDescription): Printer<unknown> {
  switch (control.control) {
    case 'checkbox':
    case 'color':
    case 'expression-input':
    case 'expression-popuplist':
    case 'euler':
    case 'matrix3':
    case 'matrix4':
    case 'none':
    case 'number-input':
    case 'popuplist':
    case 'radio':
    case 'string-input':
    case 'html-input':
    case 'style-controls':
    case 'vector2':
    case 'vector3':
    case 'vector4':
    case 'jsx':
      return printerForBasePropertyControl(control)

    case 'array':
      return printerForArray(control.propertyControl) as Printer<unknown> // Why???!!
    case 'object':
      return printerForObject(control.object) as Printer<unknown> // Why???!!
    case 'tuple':
      return printerForTuple(control.propertyControls) as Printer<unknown> // Why???!!
    case 'union':
      return printerForUnion(control.controls)
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}
