import {
  BooleanControlDescription,
  ColorControlDescription,
  ControlDescription,
  EnumControlDescription,
  NumberControlDescription,
  OptionsControlDescription,
  PopUpListControlDescription,
  StringControlDescription,
  IgnoreControlDescription,
  UnionControlDescription,
  ImageControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
  StyleObjectControlDescription,
  Vector2ControlDescription,
  Vector3ControlDescription,
  ExpressionEnumControlDescription,
  ExpressionEnum,
  ImportType,
  FolderControlDescription,
  PropertyControls,
  RegularControlDescription,
  RawJSControlDescription,
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
  parseUndefined,
} from '../../utils/value-parser-utils'
import {
  applicative2Either,
  applicative3Either,
  applicative4Either,
  applicative5Either,
  applicative6Either,
  applicative8Either,
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

export function parseNumberControlDescription(
  value: unknown,
): ParseResult<NumberControlDescription> {
  return applicative8Either(
    (label, control, defaultValue, max, min, unit, step, displayStepper) => {
      let numberControlDescription: NumberControlDescription = {
        control: control,
      }
      setOptionalProp(numberControlDescription, 'label', label)
      setOptionalProp(numberControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(numberControlDescription, 'max', max)
      setOptionalProp(numberControlDescription, 'min', min)
      setOptionalProp(numberControlDescription, 'unit', unit)
      setOptionalProp(numberControlDescription, 'step', step)
      setOptionalProp(numberControlDescription, 'displayStepper', displayStepper)

      return numberControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['number']), 'control')(value),
    optionalObjectKeyParser(parseNullable(parseNumber), 'defaultValue')(value),
    optionalObjectKeyParser(parseNumber, 'max')(value),
    optionalObjectKeyParser(parseNumber, 'min')(value),
    optionalObjectKeyParser(parseString, 'unit')(value),
    optionalObjectKeyParser(parseNumber, 'step')(value),
    optionalObjectKeyParser(parseBoolean, 'displayStepper')(value),
  )
}

type OptionTitles<P> = Array<string> | ((props: P | null) => Array<string>)

const parseOptionTitles: Parser<OptionTitles<any>> = parseAlternative<OptionTitles<any>>(
  [parseArray(parseString), parseFunction],
  'Not a string array or a function.',
)

export function parseEnumControlDescription(value: unknown): ParseResult<EnumControlDescription> {
  return applicative6Either(
    (label, control, defaultValue, options, optionTitles, displaySegmentedControl) => {
      let enumControlDescription: EnumControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(enumControlDescription, 'label', label)
      setOptionalProp(enumControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(enumControlDescription, 'optionTitles', optionTitles)
      setOptionalProp(enumControlDescription, 'displaySegmentedControl', displaySegmentedControl)

      return enumControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['enum']), 'control')(value),
    optionalObjectKeyParser(parseEnumValue, 'defaultValue')(value),
    objectKeyParser(parseArray(parseEnumValue), 'options')(value),
    optionalObjectKeyParser(parseOptionTitles, 'optionTitles')(value),
    optionalObjectKeyParser(parseBoolean, 'displaySegmentedControl')(value),
  )
}

export function parseExpressionEnumControlDescription(
  value: unknown,
): ParseResult<ExpressionEnumControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, options, optionTitles) => {
      let enumControlDescription: ExpressionEnumControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(enumControlDescription, 'label', label)
      setOptionalProp(enumControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(enumControlDescription, 'optionTitles', optionTitles)

      return enumControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['expression-enum']), 'control')(value),
    optionalObjectKeyParser(parseExpressionEnum, 'defaultValue')(value),
    objectKeyParser(parseArray(parseExpressionEnum), 'options')(value),
    optionalObjectKeyParser(parseOptionTitles, 'optionTitles')(value),
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

export function parseExpressionEnum(value: unknown): ParseResult<ExpressionEnum> {
  return applicative3Either(
    (enumValue, expression, importType) => {
      let expressionEnum: ExpressionEnum = {
        value: enumValue,
        expression: expression,
      }
      setOptionalProp(expressionEnum, 'import', importType)
      return expressionEnum
    },
    objectKeyParser(parseEnumValue, 'value')(value),
    objectKeyParser(parseString, 'expression')(value),
    optionalObjectKeyParser(parseImportType, 'import')(value),
  )
}

export function parseBooleanControlDescription(
  value: unknown,
): ParseResult<BooleanControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, disabledTitle, enabledTitle) => {
      let booleanControlDescription: BooleanControlDescription = {
        control: control,
      }
      setOptionalProp(booleanControlDescription, 'label', label)
      setOptionalProp(booleanControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(booleanControlDescription, 'disabledTitle', disabledTitle)
      setOptionalProp(booleanControlDescription, 'enabledTitle', enabledTitle)

      return booleanControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['boolean']), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'defaultValue')(value),
    optionalObjectKeyParser(parseString, 'disabledTitle')(value),
    optionalObjectKeyParser(parseString, 'enabledTitle')(value),
  )
}

export function parseStringControlDescription(
  value: unknown,
): ParseResult<StringControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, placeholder, obscured) => {
      let stringControlDescription: StringControlDescription = {
        control: control,
      }
      setOptionalProp(stringControlDescription, 'label', label)
      setOptionalProp(stringControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(stringControlDescription, 'placeholder', placeholder)
      setOptionalProp(stringControlDescription, 'obscured', obscured)

      return stringControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['string']), 'control')(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
    optionalObjectKeyParser(parseString, 'placeholder')(value),
    optionalObjectKeyParser(parseBoolean, 'obscured')(value),
  )
}

export function parsePropertyOption(value: unknown): ParseResult<{ value: any; label: string }> {
  return applicative2Either(
    (valueValue, label) => {
      return {
        value: valueValue,
        label: label,
      }
    },
    objectKeyParser(parseAny, 'value')(value),
    objectKeyParser(parseString, 'label')(value),
  )
}

export function parsePopUpListControlDescription(
  value: unknown,
): ParseResult<PopUpListControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, options) => {
      let popUpListControlDescription: PopUpListControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(popUpListControlDescription, 'label', label)
      setOptionalProp(popUpListControlDescription, 'defaultValue', defaultValue)

      return popUpListControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['popuplist']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseArray(parsePropertyOption), 'options')(value),
  )
}

export function parseOptionsControlDescription(
  value: unknown,
): ParseResult<OptionsControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, options) => {
      let optionsControlDescription: OptionsControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(optionsControlDescription, 'label', label)
      setOptionalProp(optionsControlDescription, 'defaultValue', defaultValue)

      return optionsControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['options']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseArray(parsePropertyOption), 'options')(value),
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
      const parsedColor = parseColor(text)
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
  return applicative3Either(
    (label, control, defaultValue) => {
      let colorControlDescription: ColorControlDescription = {
        control: control,
      }
      setOptionalProp(colorControlDescription, 'label', label)
      setOptionalProp(colorControlDescription, 'defaultValue', defaultValue)

      return colorControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['color']), 'control')(value),
    optionalObjectKeyParser(parseStringValidateAsColor, 'defaultValue')(value),
  )
}

export function parseRawJSControlDescription(value: unknown): ParseResult<RawJSControlDescription> {
  return applicative2Either(
    (label, control) => {
      let rawJSControlDescription: RawJSControlDescription = {
        control: control,
      }
      setOptionalProp(rawJSControlDescription, 'label', label)

      return rawJSControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['rawjs']), 'control')(value),
  )
}

export function parseIgnoreControlDescription(
  value: unknown,
): ParseResult<IgnoreControlDescription> {
  return applicative2Either(
    (label, control) => {
      let ignoreControlDescription: IgnoreControlDescription = {
        control: control,
      }
      setOptionalProp(ignoreControlDescription, 'label', label)
      return ignoreControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['ignore']), 'control')(value),
  )
}

export function parseImageControlDescription(value: unknown): ParseResult<ImageControlDescription> {
  return applicative3Either(
    (label, control, defaultValue) => {
      let imageControlDescription: ImageControlDescription = {
        control: control,
      }
      setOptionalProp(imageControlDescription, 'label', label)
      setOptionalProp(imageControlDescription, 'defaultValue', defaultValue)

      return imageControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['image']), 'control')(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
  )
}

export function parseStyleObjectControlDescription(
  value: unknown,
): ParseResult<StyleObjectControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, placeholder) => {
      let styleObjectControlDescription: StyleObjectControlDescription = {
        control: control,
      }
      setOptionalProp(styleObjectControlDescription, 'label', label)
      setOptionalProp(styleObjectControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(styleObjectControlDescription, 'placeholder', placeholder)

      return styleObjectControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['styleobject']), 'control')(value),
    optionalObjectKeyParser(parseObject(parseAny), 'defaultValue')(value), // FIXME
    optionalObjectKeyParser(parseObject(parseAny), 'placeholder')(value), // FIXME
  )
}

export function parseArrayControlDescription(value: unknown): ParseResult<ArrayControlDescription> {
  return applicative5Either(
    (label, control, defaultValue, propertyControl, maxCount) => {
      let arrayControlDescription: ArrayControlDescription = {
        control: control,
        propertyControl: propertyControl,
      }
      setOptionalProp(arrayControlDescription, 'label', label)
      setOptionalProp(arrayControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(arrayControlDescription, 'maxCount', maxCount)

      return arrayControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['array']), 'control')(value),
    optionalObjectKeyParser(parseArray(parseAny), 'defaultValue')(value),
    objectKeyParser(parseRegularControlDescription, 'propertyControl')(value),
    optionalObjectKeyParser(parseNumber, 'maxCount')(value),
  )
}

export function parseObjectControlDescription(
  value: unknown,
): ParseResult<ObjectControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, object) => {
      let objectControlDescription: ObjectControlDescription = {
        control: control,
        object: object,
      }
      setOptionalProp(objectControlDescription, 'label', label)
      setOptionalProp(objectControlDescription, 'defaultValue', defaultValue)

      return objectControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['object']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseObject(parseRegularControlDescription), 'object')(value),
  )
}

export function parseUnionControlDescription(value: unknown): ParseResult<UnionControlDescription> {
  return applicative4Either(
    (label, control, defaultValue, controls) => {
      let unionControlDescription: UnionControlDescription = {
        control: control,
        controls: controls,
      }
      setOptionalProp(unionControlDescription, 'label', label)
      setOptionalProp(unionControlDescription, 'defaultValue', defaultValue)
      return unionControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['union']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseArray(parseRegularControlDescription), 'controls')(value),
  )
}

export function parseVector2ControlDescription(
  value: unknown,
): ParseResult<Vector2ControlDescription> {
  return applicative3Either(
    (label, control, defaultValue) => {
      let controlDescription: Vector2ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['vector2']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
  )
}
export function parseVector3ControlDescription(
  value: unknown,
): ParseResult<Vector3ControlDescription> {
  return applicative3Either(
    (label, control, defaultValue) => {
      let controlDescription: Vector3ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    objectKeyParser(parseEnum(['vector3']), 'control')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
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
    switch ((value as any)['control']) {
      case 'boolean':
        return parseBooleanControlDescription(value)
      case 'color':
        return parseColorControlDescription(value)
      case 'enum':
        return parseEnumControlDescription(value)
      case 'expression-enum':
        return parseExpressionEnumControlDescription(value)
      case 'ignore':
        return parseIgnoreControlDescription(value)
      case 'image':
        return parseImageControlDescription(value)
      case 'number':
        return parseNumberControlDescription(value)
      case 'options':
        return parseOptionsControlDescription(value)
      case 'popuplist':
        return parsePopUpListControlDescription(value)
      case 'rawjs':
        return parseRawJSControlDescription(value)
      case 'string':
        return parseStringControlDescription(value)
      case 'styleobject':
        return parseStyleObjectControlDescription(value)
      case 'array':
        return parseArrayControlDescription(value)
      case 'object':
        return parseObjectControlDescription(value)
      case 'vector2':
        return parseVector2ControlDescription(value)
      case 'vector3':
        return parseVector3ControlDescription(value)
      case 'union':
        return parseUnionControlDescription(value)
      case undefined:
        return left(objectFieldNotPresentParseError('control'))
      default:
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
