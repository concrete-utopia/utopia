import {
  CheckboxControlDescription,
  ColorControlDescription,
  ControlDescription,
  NumberInputControlDescription,
  RadioControlDescription,
  PopUpListControlDescription,
  StringInputControlDescription,
  NoneControlDescription,
  UnionControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
  StyleControlsControlDescription,
  Vector2ControlDescription,
  Vector3ControlDescription,
  ExpressionPopUpListControlDescription,
  ImportType,
  FolderControlDescription,
  PropertyControls,
  RegularControlDescription,
  ExpressionInputControlDescription,
  RegularControlType,
  Vector4ControlDescription,
  EulerControlDescription,
  Matrix3ControlDescription,
  Matrix4ControlDescription,
  BasicControlOption,
  BasicControlOptions,
  ExpressionControlOption,
} from 'utopia-api'
import { parseColor } from '../../components/inspector/common/css-utils'
import {
  descriptionParseError,
  objectFieldNotPresentParseError,
  objectFieldParseError,
  objectKeyParser,
  optionalObjectKeyParser,
  parseAlternative,
  parseAny,
  parseArray,
  parseBoolean,
  parseConstant,
  parseEnum,
  parseFunction,
  parseNull,
  parseNullable,
  parseNumber,
  parseObject,
  Parser,
  ParseResult,
  parseString,
  parseTuple,
  parseUndefined,
} from '../../utils/value-parser-utils'
import {
  applicative2Either,
  applicative3Either,
  applicative4Either,
  applicative5Either,
  applicative6Either,
  applicative8Either,
  applicative9Either,
  foldEither,
  left,
  right,
  isRight,
  mapEither,
  flatMapEither,
  isLeft,
} from '../shared/either'
import {
  objectMap,
  setOptionalProp,
  forEachValue,
  objectMapDropNulls,
} from '../shared/object-utils'
import { parseEnumValue } from './property-control-values'
import { filterSpecialProp, filterSpecialProps } from './property-controls-utils'

export function parseNumberInputControlDescription(
  value: unknown,
): ParseResult<NumberInputControlDescription> {
  return applicative9Either(
    (label, control, defaultValue, max, min, unit, step, displayStepper, visibleByDefault) => {
      let numberInputControlDescription: NumberInputControlDescription = {
        control: control,
      }
      setOptionalProp(numberInputControlDescription, 'label', label)
      setOptionalProp(numberInputControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(numberInputControlDescription, 'max', max)
      setOptionalProp(numberInputControlDescription, 'min', min)
      setOptionalProp(numberInputControlDescription, 'unit', unit)
      setOptionalProp(numberInputControlDescription, 'step', step)
      setOptionalProp(numberInputControlDescription, 'displayStepper', displayStepper)
      setOptionalProp(numberInputControlDescription, 'visibleByDefault', visibleByDefault)

      return numberInputControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['number-input']), 'control')(value),
    optionalObjectKeyParser(parseNullable(parseNumber), 'defaultValue')(value),
    optionalObjectKeyParser(parseNumber, 'max')(value),
    optionalObjectKeyParser(parseNumber, 'min')(value),
    optionalObjectKeyParser(parseString, 'unit')(value),
    optionalObjectKeyParser(parseNumber, 'step')(value),
    optionalObjectKeyParser(parseBoolean, 'displayStepper')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

function parseBasicControlOption<V>(valueParser: Parser<V>): Parser<BasicControlOption<V>> {
  return (value: unknown) => {
    return applicative2Either(
      (label, optionValue) => {
        return {
          value: optionValue,
          label: label,
        }
      },
      objectKeyParser(parseString, 'label')(value),
      objectKeyParser(valueParser, 'value')(value),
    )
  }
}

const parseBasicControlOptions: Parser<BasicControlOptions<unknown>> = parseAlternative<
  BasicControlOptions<unknown>
>(
  [parseArray(parseEnumValue), parseArray(parseBasicControlOption<unknown>(parseAny))],
  'Not a valid array of options',
)

export function parsePopUpListControlDescription(
  value: unknown,
): ParseResult<PopUpListControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, options, visibleByDefault) => {
      let popupListControlDescription: PopUpListControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(popupListControlDescription, 'label', label)
      setOptionalProp(popupListControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(popupListControlDescription, 'visibleByDefault', visibleByDefault)

      return popupListControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['popuplist']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseBasicControlOptions, 'options')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseExpressionPopUpListControlDescription(
  value: unknown,
): ParseResult<ExpressionPopUpListControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, options, visibleByDefault) => {
      let enumControlDescription: ExpressionPopUpListControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(enumControlDescription, 'label', label)
      setOptionalProp(enumControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(enumControlDescription, 'visibleByDefault', visibleByDefault)

      return enumControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['expression-popuplist']), 'control')(value),
    optionalObjectKeyParser(parseExpressionControlOption, 'defaultValue')(value),
    objectKeyParser(parseArray(parseExpressionControlOption), 'options')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

const parseImportType: Parser<ImportType> = (value: unknown) => {
  return applicative3Either(
    (source, name, type) => {
      let importType: ImportType = {
        source,
        name,
        type,
      }
      return importType
    },
    objectKeyParser(parseString, 'source')(value),
    objectKeyParser(parseString, 'name')(value),
    objectKeyParser(
      parseAlternative(
        [parseConstant('default'), parseConstant('star'), parseNull],
        'invalid import type',
      ),
      'type',
    )(value),
  )
}

function parseExpressionControlOption(
  value: unknown,
): ParseResult<ExpressionControlOption<unknown>> {
  return applicative4Either(
    (expressionValue, expression, label, requiredImport) => {
      let expressionControlOption: ExpressionControlOption<unknown> = {
        value: expressionValue,
        expression: expression,
      }
      setOptionalProp(expressionControlOption, 'label', label)
      setOptionalProp(expressionControlOption, 'requiredImport', requiredImport)
      return expressionControlOption
    },
    objectKeyParser(parseAny, 'value')(value),
    objectKeyParser(parseString, 'expression')(value),
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseImportType, 'requiredImport')(value),
  )
}

export function parseCheckboxControlDescription(
  value: unknown,
): ParseResult<CheckboxControlDescription> {
  return applicative6Either(
    (label, control, defaultValue, disabledTitle, enabledTitle, visibleByDefault) => {
      let checkboxControlDescription: CheckboxControlDescription = {
        control: control,
      }
      setOptionalProp(checkboxControlDescription, 'label', label)
      setOptionalProp(checkboxControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(checkboxControlDescription, 'disabledTitle', disabledTitle)
      setOptionalProp(checkboxControlDescription, 'enabledTitle', enabledTitle)
      setOptionalProp(checkboxControlDescription, 'visibleByDefault', visibleByDefault)

      return checkboxControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['checkbox']), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'defaultValue')(value),
    optionalObjectKeyParser(parseString, 'disabledTitle')(value),
    optionalObjectKeyParser(parseString, 'enabledTitle')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseStringInputControlDescription(
  value: unknown,
): ParseResult<StringInputControlDescription> {
  return applicative6Either(
    (label, control, defaultValue, placeholder, obscured, visibleByDefault) => {
      let stringInputControlDescription: StringInputControlDescription = {
        control: control,
      }
      setOptionalProp(stringInputControlDescription, 'label', label)
      setOptionalProp(stringInputControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(stringInputControlDescription, 'placeholder', placeholder)
      setOptionalProp(stringInputControlDescription, 'obscured', obscured)
      setOptionalProp(stringInputControlDescription, 'visibleByDefault', visibleByDefault)

      return stringInputControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['string-input']), 'control')(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
    optionalObjectKeyParser(parseString, 'placeholder')(value),
    optionalObjectKeyParser(parseBoolean, 'obscured')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseRadioControlDescription(value: unknown): ParseResult<RadioControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, options, visibleByDefault) => {
      let radioControlDescription: RadioControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(radioControlDescription, 'label', label)
      setOptionalProp(radioControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(radioControlDescription, 'visibleByDefault', visibleByDefault)

      return radioControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['radio']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseBasicControlOptions, 'options')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

const invalidColorStringResult: ParseResult<string> = left(
  descriptionParseError('Not a valid color string.'),
)

// We want to parse the string, but check that it can be parsed as a color.
// Returning the string, only if it does validate against `parseColor`.
export function parseStringValidateAsColor(value: unknown): ParseResult<string> {
  const parsed = parseString(value)
  return foldEither(
    (_) => {
      return invalidColorStringResult
    },
    (text) => {
      const parsedColor = parseColor(text, 'hex-hash-required')
      return foldEither(
        (_) => {
          return invalidColorStringResult
        },
        (_) => {
          return right(text)
        },
        parsedColor,
      )
    },
    parsed,
  )
}

export function parseColorControlDescription(value: unknown): ParseResult<ColorControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, visibleByDefault) => {
      let colorControlDescription: ColorControlDescription = {
        control: control,
      }
      setOptionalProp(colorControlDescription, 'label', label)
      setOptionalProp(colorControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(colorControlDescription, 'visibleByDefault', visibleByDefault)

      return colorControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['color']), 'control')(value),
    optionalObjectKeyParser(parseStringValidateAsColor, 'defaultValue')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseExpressionInputControlDescription(
  value: unknown,
): ParseResult<ExpressionInputControlDescription> {
  return applicative3Either(
    (label, control, visibleByDefault) => {
      let expressionInputControlDescription: ExpressionInputControlDescription = {
        control: control,
      }
      setOptionalProp(expressionInputControlDescription, 'label', label)
      setOptionalProp(expressionInputControlDescription, 'visibleByDefault', visibleByDefault)

      return expressionInputControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['expression-input']), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseNoneControlDescription(value: unknown): ParseResult<NoneControlDescription> {
  return applicative3Either(
    (label, control, visibleByDefault) => {
      let noneControlDescription: NoneControlDescription = {
        control: control,
      }
      setOptionalProp(noneControlDescription, 'label', label)
      setOptionalProp(noneControlDescription, 'visibleByDefault', visibleByDefault)
      return noneControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['none']), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseStyleControlsControlDescription(
  value: unknown,
): ParseResult<StyleControlsControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, placeholder, visibleByDefault) => {
      let styleControlsControlDescription: StyleControlsControlDescription = {
        control: control,
      }
      setOptionalProp(styleControlsControlDescription, 'label', label)
      setOptionalProp(styleControlsControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(styleControlsControlDescription, 'placeholder', placeholder)
      setOptionalProp(styleControlsControlDescription, 'visibleByDefault', visibleByDefault)

      return styleControlsControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['style-controls']), 'control')(value),
    optionalObjectKeyParser(parseObject(parseAny), 'defaultValue')(value), // FIXME
    optionalObjectKeyParser(parseObject(parseAny), 'placeholder')(value), // FIXME
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseArrayControlDescription(value: unknown): ParseResult<ArrayControlDescription> {
  return applicative6Either(
    (label, control, defaultValue, propertyControl, maxCount, visibleByDefault) => {
      let arrayControlDescription: ArrayControlDescription = {
        control: control,
        propertyControl: propertyControl,
      }
      setOptionalProp(arrayControlDescription, 'label', label)
      setOptionalProp(arrayControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(arrayControlDescription, 'maxCount', maxCount)
      setOptionalProp(arrayControlDescription, 'visibleByDefault', visibleByDefault)

      return arrayControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['array']), 'control')(value),
    optionalObjectKeyParser(parseArray(parseAny), 'defaultValue')(value),
    objectKeyParser(parseRegularControlDescription, 'propertyControl')(value),
    optionalObjectKeyParser(parseNumber, 'maxCount')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseObjectControlDescription(
  value: unknown,
): ParseResult<ObjectControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, object, visibleByDefault) => {
      let objectControlDescription: ObjectControlDescription = {
        control: control,
        object: object,
      }
      setOptionalProp(objectControlDescription, 'label', label)
      setOptionalProp(objectControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(objectControlDescription, 'visibleByDefault', visibleByDefault)

      return objectControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['object']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseObject(parseRegularControlDescription), 'object')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseUnionControlDescription(value: unknown): ParseResult<UnionControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, controls, visibleByDefault) => {
      let unionControlDescription: UnionControlDescription = {
        control: control,
        controls: controls,
      }
      setOptionalProp(unionControlDescription, 'label', label)
      setOptionalProp(unionControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(unionControlDescription, 'visibleByDefault', visibleByDefault)
      return unionControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['union']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseArray(parseRegularControlDescription), 'controls')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseVector2ControlDescription(
  value: unknown,
): ParseResult<Vector2ControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, visibleByDefault) => {
      let controlDescription: Vector2ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['vector2']), 'control')(value),
    optionalObjectKeyParser(parseTuple<[number, number]>(parseNumber, 2), 'defaultValue')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseVector3ControlDescription(
  value: unknown,
): ParseResult<Vector3ControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, visibleByDefault) => {
      let controlDescription: Vector3ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['vector3']), 'control')(value),
    optionalObjectKeyParser(
      parseTuple<[number, number, number]>(parseNumber, 3),
      'defaultValue',
    )(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseVector4ControlDescription(
  value: unknown,
): ParseResult<Vector4ControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, visibleByDefault) => {
      let controlDescription: Vector4ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['vector4']), 'control')(value),
    optionalObjectKeyParser(
      parseTuple<[number, number, number, number]>(parseNumber, 4),
      'defaultValue',
    )(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseEulerControlDescription(value: unknown): ParseResult<EulerControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, visibleByDefault) => {
      let controlDescription: EulerControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['euler']), 'control')(value),
    optionalObjectKeyParser(
      parseTuple<[number, number, number, string]>(
        (v, i) => (i === 3 ? parseString(v) : parseNumber(v)),
        4,
      ),
      'defaultValue',
    )(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

// prettier-ignore
type Matrix3 = [
  number, number, number,
  number, number, number,
  number, number, number,
]

export function parseMatrix3ControlDescription(
  value: unknown,
): ParseResult<Matrix3ControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, visibleByDefault) => {
      let controlDescription: Matrix3ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['matrix3']), 'control')(value),
    optionalObjectKeyParser(parseTuple<Matrix3>(parseNumber, 9), 'defaultValue')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

// prettier-ignore
type Matrix4 = [
  number, number, number, number,
  number, number, number, number,
  number, number, number, number,
  number, number, number, number,
]

export function parseMatrix4ControlDescription(
  value: unknown,
): ParseResult<Matrix4ControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, visibleByDefault) => {
      let controlDescription: Matrix4ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['matrix4']), 'control')(value),
    optionalObjectKeyParser(parseTuple<Matrix4>(parseNumber, 16), 'defaultValue')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
  )
}

export function parseFolderControlDescription(
  value: unknown,
  filterSpecialPropsFromResult: 'includeSpecialProps' | 'filterSpecialProps',
): ParseResult<FolderControlDescription> {
  // Results in parse errors within individual property names.
  const propertiesResult = objectKeyParser(
    (v) => parsePropertyControls(v, filterSpecialPropsFromResult),
    'controls',
  )(value)
  // Flatten out the errors within each property.
  const parsedControlDescriptions: ParseResult<PropertyControls> = flatMapEither(
    (parsedControlResults) => {
      let workingResult: PropertyControls = {}
      for (const propertyName of Object.keys(parsedControlResults)) {
        const propertyResult = parsedControlResults[propertyName]
        if (isLeft(propertyResult)) {
          return left(
            objectFieldParseError(
              'controls',
              objectFieldParseError(propertyName, propertyResult.value),
            ),
          )
        } else {
          workingResult[propertyName] = propertyResult.value
        }
      }
      return right(workingResult)
    },
    propertiesResult,
  )
  // Create the result on a success.
  return applicative2Either(
    (properties, label) => {
      let controlDescription: FolderControlDescription = {
        control: 'folder',
        controls: properties,
      }
      setOptionalProp(controlDescription, 'label', label)
      return controlDescription
    },
    parsedControlDescriptions,
    optionalObjectKeyParser(parseString, 'label')(value),
  )
}

export function parseRegularControlDescription(
  value: unknown,
): ParseResult<RegularControlDescription> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    const controlType = (value as any)['control'] as RegularControlType
    switch (controlType) {
      case 'array':
        return parseArrayControlDescription(value)
      case 'checkbox':
        return parseCheckboxControlDescription(value)
      case 'color':
        return parseColorControlDescription(value)
      case 'euler':
        return parseEulerControlDescription(value)
      case 'expression-input':
        return parseExpressionInputControlDescription(value)
      case 'expression-popuplist':
        return parseExpressionPopUpListControlDescription(value)
      case 'matrix3':
        return parseMatrix3ControlDescription(value)
      case 'matrix4':
        return parseMatrix4ControlDescription(value)
      case 'none':
        return parseNoneControlDescription(value)
      case 'number-input':
        return parseNumberInputControlDescription(value)
      case 'object':
        return parseObjectControlDescription(value)
      case 'radio':
        return parseRadioControlDescription(value)
      case 'popuplist':
        return parsePopUpListControlDescription(value)
      case 'string-input':
        return parseStringInputControlDescription(value)
      case 'style-controls':
        return parseStyleControlsControlDescription(value)
      case 'vector2':
        return parseVector2ControlDescription(value)
      case 'vector3':
        return parseVector3ControlDescription(value)
      case 'vector4':
        return parseVector4ControlDescription(value)
      case 'union':
        return parseUnionControlDescription(value)
      case undefined:
        return left(objectFieldNotPresentParseError('control'))
      default:
        const _exhaustiveCheck: never = controlType
        return left(
          objectFieldParseError(
            'control',
            descriptionParseError('Unexpected property control type.'),
          ),
        )
    }
  } else {
    return left(descriptionParseError('Not an object.'))
  }
}

export function parseControlDescription(
  value: unknown,
  key: string | number,
  filterSpecialPropsFromResult: 'includeSpecialProps' | 'filterSpecialProps',
): ParseResult<ControlDescription> | null {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    switch ((value as any)['control']) {
      case 'folder':
        return parseFolderControlDescription(value, filterSpecialPropsFromResult)
      default:
        if (filterSpecialPropsFromResult === 'includeSpecialProps' || filterSpecialProp(key)) {
          return parseRegularControlDescription(value)
        } else {
          return null
        }
    }
  } else {
    return left(descriptionParseError('Not an object.'))
  }
}

export type ParsedPropertyControls = { [prop: string]: ParseResult<ControlDescription> }
export type ParsedPropertyControlsForFile = {
  [componentName: string]: ParseResult<ParsedPropertyControls>
}

export function parsePropertyControls(
  value: unknown,
  filterSpecialPropsFromResult: 'includeSpecialProps' | 'filterSpecialProps',
): ParseResult<ParsedPropertyControls> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    return right(
      objectMapDropNulls(
        (v, k) => parseControlDescription(v, k, filterSpecialPropsFromResult),
        value as any,
      ),
    )
  } else {
    return left(descriptionParseError('Not an object.'))
  }
}

export function parsePropertyControlsForFile(
  allControls: {
    [componentName: string]: unknown
  },
  filterSpecialPropsFromResult: 'includeSpecialProps' | 'filterSpecialProps',
): ParsedPropertyControlsForFile {
  return objectMap((v) => parsePropertyControls(v, filterSpecialPropsFromResult), allControls)
}
