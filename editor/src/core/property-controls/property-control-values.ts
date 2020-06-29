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
} from 'utopia-api'
import { parseColor, CSSColor } from '../../components/inspector/common/css-utils'
import { foldEither, left, right, isRight } from '../shared/either'

type Printer<T> = (value: T) => unknown

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

function parseObjectValues(objectControls: {
  [prop: string]: ControlDescription
}): Parser<unknown> {
  return parseObject((v: unknown, k: string) => {
    const controlForKey = objectControls[k]
    const parseInnerValue =
      controlForKey == null ? parseAny : parserForPropertyControl(controlForKey)
    return parseInnerValue(v)
  })
}

function parseUnionValue(controls: Array<ControlDescription>): Parser<unknown> {
  const parsers = controls.map(parserForPropertyControl)
  return parseAlternative(parsers, "Value can't be parsed by any of the available controls")
}

function parserForBasePropertyControl(control: BaseControlDescription): Parser<unknown> {
  switch (control.type) {
    case 'boolean':
      return parseBoolean
    case 'color':
      return parseColorValue
    case 'componentinstance':
      return parseString
    case 'enum':
      return parseEnumValue
    case 'eventhandler':
      return parseAny
    case 'ignore':
      return parseAny
    case 'image':
      return parseString
    case 'number':
      return parseNumber
    case 'options':
      return parseAny
    case 'popuplist':
      return parseAny
    case 'slider':
      return parseNumber
    case 'string':
      return parseString
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}

function parserForPropertyControl(control: ControlDescription): Parser<unknown> {
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
      return parserForBasePropertyControl(control)

    case 'array':
      return parserForPropertyControl(control.propertyControl)
    case 'object':
      return parseObjectValues(control.object)
    case 'union':
      return parseUnionValue(control.controls)
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}

export function controlToUseForUnion(
  control: UnionControlDescription,
  value: unknown,
): ControlDescription {
  // Find the first control that parses the value
  const foundControl = control.controls.find((inner) => {
    const parser = parserForPropertyControl(inner)
    const parsed = parser(value)
    return isRight(parsed)
  })
  return foundControl == null ? control.controls[0] : foundControl
}

export function printerForPropertyControl(control: ControlDescription): Printer<unknown> {
  throw new Error('Not yet implemented')
}
