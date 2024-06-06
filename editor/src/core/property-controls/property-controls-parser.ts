import {
  type CheckboxControlDescription,
  type ColorControlDescription,
  type ControlDescription,
  type NumberInputControlDescription,
  type RadioControlDescription,
  type PopUpListControlDescription,
  type StringInputControlDescription,
  type NoneControlDescription,
  type UnionControlDescription,
  type ArrayControlDescription,
  type ObjectControlDescription,
  type StyleControlsControlDescription,
  type Vector2ControlDescription,
  type Vector3ControlDescription,
  type ExpressionPopUpListControlDescription,
  type ImportType,
  type RegularControlDescription,
  type ExpressionInputControlDescription,
  type RegularControlType,
  type Vector4ControlDescription,
  type EulerControlDescription,
  type Matrix3ControlDescription,
  type Matrix4ControlDescription,
  type BasicControlOption,
  type BasicControlOptions,
  type ExpressionControlOption,
  type TupleControlDescription,
  type HtmlInputControlDescription,
  type JSXControlDescription,
  type AllowedEnumType,
  type Matrix3,
  type Matrix4,
  type PreferredContents,
  type BasicControlOptionWithIcon,
  PropertyControlIcons,
} from 'utopia-api/core'
import { parseColor } from '../../components/inspector/common/css-utils'
import type { Parser, ParseResult } from '../../utils/value-parser-utils'
import {
  descriptionParseError,
  flatMapParser,
  objectFieldNotPresentParseError,
  objectFieldParseError,
  objectKeyParser,
  objectParser,
  optionalObjectKeyParser,
  optionalProp,
  parseAlternative,
  parseAny,
  parseArray,
  parseBoolean,
  parseConstant,
  parseEnum,
  parseNull,
  parseNumber,
  parseObject,
  parseString,
} from '../../utils/value-parser-utils'
import {
  applicative2Either,
  applicative3Either,
  applicative4Either,
  applicative6Either,
  applicative8Either,
  foldEither,
  left,
  right,
  applicative7Either,
} from '../shared/either'
import { objectMap, setOptionalProp } from '../shared/object-utils'
import { parseEnumValue } from './property-control-values'
import { parsePreferredContents } from './property-controls-local'

const requiredFieldParser = optionalObjectKeyParser(parseBoolean, 'required')

export const parseNumberInputControlDescription = objectParser<NumberInputControlDescription>({
  control: parseConstant('number-input'),
  label: optionalProp(parseString),
  folder: optionalProp(parseString),
  max: optionalProp(parseNumber),
  min: optionalProp(parseNumber),
  unit: optionalProp(parseString),
  step: optionalProp(parseNumber),
  displayStepper: optionalProp(parseBoolean),
  visibleByDefault: optionalProp(parseBoolean),
  required: optionalProp(parseBoolean),
  defaultValue: optionalProp(parseNumber),
})

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

const parseBasicControlOptionsWithIcon: Parser<
  AllowedEnumType[] | BasicControlOptionWithIcon<unknown>[]
> = parseAlternative<AllowedEnumType[] | BasicControlOptionWithIcon<unknown>[]>(
  [
    parseArray(parseEnumValue),
    parseArray(
      objectParser<BasicControlOptionWithIcon<unknown>>({
        label: parseString,
        value: parseAny,
        icon: optionalProp(parseAny),
      }),
    ),
  ],
  'Not a valid array of options',
)

const parseEnumValueOrBasicControlOption: Parser<AllowedEnumType | BasicControlOption<unknown>> =
  parseAlternative<AllowedEnumType | BasicControlOption<unknown>>(
    [parseEnumValue, parseBasicControlOption<unknown>(parseAny)],
    'Not an enum or basic control option.',
  )

export function parsePopUpListControlDescription(
  value: unknown,
): ParseResult<PopUpListControlDescription> {
  return applicative7Either(
    (label, folder, control, options, visibleByDefault, required, defaultValue) => {
      let popupListControlDescription: PopUpListControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(popupListControlDescription, 'label', label)
      setOptionalProp(popupListControlDescription, 'folder', folder)
      setOptionalProp(popupListControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(popupListControlDescription, 'required', required)
      setOptionalProp(popupListControlDescription, 'defaultValue', defaultValue)

      return popupListControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('popuplist'), 'control')(value),
    objectKeyParser(parseBasicControlOptions, 'options')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseEnumValueOrBasicControlOption, 'defaultValue')(value),
  )
}

export function parseExpressionPopUpListControlDescription(
  value: unknown,
): ParseResult<ExpressionPopUpListControlDescription> {
  return applicative7Either(
    (label, folder, control, options, visibleByDefault, required, defaultValue) => {
      let enumControlDescription: ExpressionPopUpListControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(enumControlDescription, 'label', label)
      setOptionalProp(enumControlDescription, 'folder', folder)
      setOptionalProp(enumControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(enumControlDescription, 'required', required)
      setOptionalProp(enumControlDescription, 'defaultValue', defaultValue)

      return enumControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('expression-popuplist'), 'control')(value),
    objectKeyParser(parseArray(parseExpressionControlOption), 'options')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseEnumValueOrBasicControlOption, 'defaultValue')(value),
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
  return applicative8Either(
    (
      label,
      folder,
      control,
      disabledTitle,
      enabledTitle,
      visibleByDefault,
      required,
      defaultValue,
    ) => {
      let checkboxControlDescription: CheckboxControlDescription = {
        control: control,
      }
      setOptionalProp(checkboxControlDescription, 'label', label)
      setOptionalProp(checkboxControlDescription, 'folder', folder)
      setOptionalProp(checkboxControlDescription, 'disabledTitle', disabledTitle)
      setOptionalProp(checkboxControlDescription, 'enabledTitle', enabledTitle)
      setOptionalProp(checkboxControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(checkboxControlDescription, 'required', required)
      setOptionalProp(checkboxControlDescription, 'defaultValue', defaultValue)

      return checkboxControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('checkbox'), 'control')(value),
    optionalObjectKeyParser(parseString, 'disabledTitle')(value),
    optionalObjectKeyParser(parseString, 'enabledTitle')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseBoolean, 'defaultValue')(value),
  )
}

export function parseStringInputControlDescription(
  value: unknown,
): ParseResult<StringInputControlDescription> {
  return applicative8Either(
    (label, folder, control, placeholder, obscured, visibleByDefault, required, defaultValue) => {
      let stringInputControlDescription: StringInputControlDescription = {
        control: control,
      }
      setOptionalProp(stringInputControlDescription, 'label', label)
      setOptionalProp(stringInputControlDescription, 'folder', folder)
      setOptionalProp(stringInputControlDescription, 'placeholder', placeholder)
      setOptionalProp(stringInputControlDescription, 'obscured', obscured)
      setOptionalProp(stringInputControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(stringInputControlDescription, 'required', required)
      setOptionalProp(stringInputControlDescription, 'defaultValue', defaultValue)

      return stringInputControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('string-input'), 'control')(value),
    optionalObjectKeyParser(parseString, 'placeholder')(value),
    optionalObjectKeyParser(parseBoolean, 'obscured')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
  )
}

export function parseHtmlInputControlDescription(
  value: unknown,
): ParseResult<HtmlInputControlDescription> {
  return applicative8Either(
    (label, folder, control, placeholder, obscured, visibleByDefault, required, defaultValue) => {
      let htmlInputControlDescription: HtmlInputControlDescription = {
        control: control,
      }
      setOptionalProp(htmlInputControlDescription, 'label', label)
      setOptionalProp(htmlInputControlDescription, 'folder', folder)
      setOptionalProp(htmlInputControlDescription, 'placeholder', placeholder)
      setOptionalProp(htmlInputControlDescription, 'obscured', obscured)
      setOptionalProp(htmlInputControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(htmlInputControlDescription, 'required', required)
      setOptionalProp(htmlInputControlDescription, 'defaultValue', defaultValue)

      return htmlInputControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('html-input'), 'control')(value),
    optionalObjectKeyParser(parseString, 'placeholder')(value),
    optionalObjectKeyParser(parseBoolean, 'obscured')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
  )
}

export function parseRadioControlDescription(value: unknown): ParseResult<RadioControlDescription> {
  return applicative7Either(
    (label, folder, control, options, visibleByDefault, required, defaultValue) => {
      let radioControlDescription: RadioControlDescription = {
        control: control,
        options: options,
      }
      setOptionalProp(radioControlDescription, 'label', label)
      setOptionalProp(radioControlDescription, 'folder', folder)
      setOptionalProp(radioControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(radioControlDescription, 'required', required)
      setOptionalProp(radioControlDescription, 'defaultValue', defaultValue)

      return radioControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('radio'), 'control')(value),
    objectKeyParser(parseBasicControlOptionsWithIcon, 'options')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseEnumValueOrBasicControlOption, 'defaultValue')(value),
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
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let colorControlDescription: ColorControlDescription = {
        control: control,
      }
      setOptionalProp(colorControlDescription, 'label', label)
      setOptionalProp(colorControlDescription, 'folder', folder)
      setOptionalProp(colorControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(colorControlDescription, 'required', required)
      setOptionalProp(colorControlDescription, 'defaultValue', defaultValue)

      return colorControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('color'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
  )
}

export function parseExpressionInputControlDescription(
  value: unknown,
): ParseResult<ExpressionInputControlDescription> {
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let expressionInputControlDescription: ExpressionInputControlDescription = {
        control: control,
      }
      setOptionalProp(expressionInputControlDescription, 'label', label)
      setOptionalProp(expressionInputControlDescription, 'folder', folder)
      setOptionalProp(expressionInputControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(expressionInputControlDescription, 'required', required)
      setOptionalProp(expressionInputControlDescription, 'defaultValue', defaultValue)

      return expressionInputControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('expression-input'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
  )
}

export function parseNoneControlDescription(value: unknown): ParseResult<NoneControlDescription> {
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let noneControlDescription: NoneControlDescription = {
        control: control,
      }
      setOptionalProp(noneControlDescription, 'label', label)
      setOptionalProp(noneControlDescription, 'folder', folder)
      setOptionalProp(noneControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(noneControlDescription, 'required', required)
      setOptionalProp(noneControlDescription, 'defaultValue', defaultValue)
      return noneControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('none'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
  )
}

export function parseStyleControlsControlDescription(
  value: unknown,
): ParseResult<StyleControlsControlDescription> {
  return applicative7Either(
    (label, folder, control, placeholder, visibleByDefault, required, defaultValue) => {
      let styleControlsControlDescription: StyleControlsControlDescription = {
        control: control,
      }
      setOptionalProp(styleControlsControlDescription, 'label', label)
      setOptionalProp(styleControlsControlDescription, 'folder', folder)
      setOptionalProp(styleControlsControlDescription, 'placeholder', placeholder)
      setOptionalProp(styleControlsControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(styleControlsControlDescription, 'required', required)
      setOptionalProp(styleControlsControlDescription, 'defaultValue', defaultValue)

      return styleControlsControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('style-controls'), 'control')(value),
    optionalObjectKeyParser(parseObject(parseAny), 'placeholder')(value), // FIXME
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
  )
}

export function parseArrayControlDescription(value: unknown): ParseResult<ArrayControlDescription> {
  return applicative8Either(
    (
      label,
      folder,
      control,
      propertyControl,
      maxCount,
      visibleByDefault,
      required,
      defaultValue,
    ) => {
      let arrayControlDescription: ArrayControlDescription = {
        control: control,
        propertyControl: propertyControl,
      }
      setOptionalProp(arrayControlDescription, 'label', label)
      setOptionalProp(arrayControlDescription, 'folder', folder)
      setOptionalProp(arrayControlDescription, 'maxCount', maxCount)
      setOptionalProp(arrayControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(arrayControlDescription, 'required', required)
      setOptionalProp(arrayControlDescription, 'defaultValue', defaultValue)

      return arrayControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('array'), 'control')(value),
    objectKeyParser(parseRegularControlDescription, 'propertyControl')(value),
    optionalObjectKeyParser(parseNumber, 'maxCount')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseArray(parseAny), 'defaultValue')(value),
  )
}

export function parseTupleControlDescription(value: unknown): ParseResult<TupleControlDescription> {
  return applicative7Either(
    (label, folder, control, propertyControls, visibleByDefault, required, defaultValue) => {
      let tupleControlDescription: TupleControlDescription = {
        control: control,
        propertyControls: propertyControls,
      }
      setOptionalProp(tupleControlDescription, 'label', label)
      setOptionalProp(tupleControlDescription, 'folder', folder)
      setOptionalProp(tupleControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(tupleControlDescription, 'required', required)
      setOptionalProp(tupleControlDescription, 'defaultValue', defaultValue)

      return tupleControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('tuple'), 'control')(value),
    objectKeyParser(parseArray(parseRegularControlDescription), 'propertyControls')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseArray(parseAny), 'defaultValue')(value),
  )
}

export function parseObjectControlDescription(
  value: unknown,
): ParseResult<ObjectControlDescription> {
  return applicative7Either(
    (label, folder, control, object, visibleByDefault, required, defaultValue) => {
      let objectControlDescription: ObjectControlDescription = {
        control: control,
        object: object,
      }
      setOptionalProp(objectControlDescription, 'label', label)
      setOptionalProp(objectControlDescription, 'folder', folder)
      setOptionalProp(objectControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(objectControlDescription, 'required', required)
      setOptionalProp(objectControlDescription, 'defaultValue', defaultValue)

      return objectControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('object'), 'control')(value),
    objectKeyParser(parseObject(parseRegularControlDescription), 'object')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseObject(parseAny), 'defaultValue')(value),
  )
}

export function parseUnionControlDescription(value: unknown): ParseResult<UnionControlDescription> {
  return applicative7Either(
    (label, folder, control, controls, visibleByDefault, required, defaultValue) => {
      let unionControlDescription: UnionControlDescription = {
        control: control,
        controls: controls,
      }
      setOptionalProp(unionControlDescription, 'label', label)
      setOptionalProp(unionControlDescription, 'folder', folder)
      setOptionalProp(unionControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(unionControlDescription, 'required', required)
      setOptionalProp(unionControlDescription, 'defaultValue', defaultValue)
      return unionControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('union'), 'control')(value),
    objectKeyParser(parseArray(parseRegularControlDescription), 'controls')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
  )
}

export const parseVector2 = flatMapParser<Array<number>, [number, number]>(
  parseArray(parseNumber),
  (array) => {
    if (array.length === 2) {
      return right(array as [number, number])
    } else {
      return left(
        descriptionParseError(`Array not of length 2 but instead of length ${array.length}`),
      )
    }
  },
)

export function parseVector2ControlDescription(
  value: unknown,
): ParseResult<Vector2ControlDescription> {
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let controlDescription: Vector2ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'folder', folder)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(controlDescription, 'required', required)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('vector2'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseVector2, 'defaultValue')(value),
  )
}

export const parseVector3 = flatMapParser<Array<number>, [number, number, number]>(
  parseArray(parseNumber),
  (array) => {
    if (array.length === 3) {
      return right(array as [number, number, number])
    } else {
      return left(
        descriptionParseError(`Array not of length 3 but instead of length ${array.length}`),
      )
    }
  },
)

export function parseVector3ControlDescription(
  value: unknown,
): ParseResult<Vector3ControlDescription> {
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let controlDescription: Vector3ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'folder', folder)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(controlDescription, 'required', required)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('vector3'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseVector3, 'defaultValue')(value),
  )
}

export const parseVector4 = flatMapParser<Array<number>, [number, number, number, number]>(
  parseArray(parseNumber),
  (array) => {
    if (array.length === 4) {
      return right(array as [number, number, number, number])
    } else {
      return left(
        descriptionParseError(`Array not of length 4 but instead of length ${array.length}`),
      )
    }
  },
)

export function parseVector4ControlDescription(
  value: unknown,
): ParseResult<Vector4ControlDescription> {
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let controlDescription: Vector4ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'folder', folder)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(controlDescription, 'required', required)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('vector4'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseVector4, 'defaultValue')(value),
  )
}

export const parseEuler = flatMapParser<Array<unknown>, [number, number, number, string]>(
  parseArray(parseAny),
  (array) => {
    if (array.length === 4) {
      if (typeof array[0] !== 'number') {
        return left(descriptionParseError(`Element 0 should be a number.`))
      } else if (typeof array[1] !== 'number') {
        return left(descriptionParseError(`Element 1 should be a number.`))
      } else if (typeof array[2] !== 'number') {
        return left(descriptionParseError(`Element 2 should be a number.`))
      } else if (typeof array[3] !== 'string') {
        return left(descriptionParseError(`Element 3 should be a string.`))
      } else {
        return right(array as [number, number, number, string])
      }
    } else {
      return left(
        descriptionParseError(`Array not of length 4 but instead of length ${array.length}`),
      )
    }
  },
)

export function parseEulerControlDescription(value: unknown): ParseResult<EulerControlDescription> {
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let controlDescription: EulerControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'folder', folder)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(controlDescription, 'required', required)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('euler'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseEuler, 'defaultValue')(value),
  )
}

export const parseMatrix3 = flatMapParser<Array<number>, Matrix3>(
  parseArray(parseNumber),
  (array) => {
    if (array.length === 9) {
      return right(array as Matrix3)
    } else {
      return left(
        descriptionParseError(`Array not of length 9 but instead of length ${array.length}`),
      )
    }
  },
)

export function parseMatrix3ControlDescription(
  value: unknown,
): ParseResult<Matrix3ControlDescription> {
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let controlDescription: Matrix3ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'folder', folder)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(controlDescription, 'required', required)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('matrix3'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseMatrix3, 'defaultValue')(value),
  )
}

export const parseMatrix4 = flatMapParser<Array<number>, Matrix4>(
  parseArray(parseNumber),
  (array) => {
    if (array.length === 16) {
      return right(array as Matrix4)
    } else {
      return left(
        descriptionParseError(`Array not of length 16 but instead of length ${array.length}`),
      )
    }
  },
)

export function parseMatrix4ControlDescription(
  value: unknown,
): ParseResult<Matrix4ControlDescription> {
  return applicative6Either(
    (label, folder, control, visibleByDefault, required, defaultValue) => {
      let controlDescription: Matrix4ControlDescription = {
        control: control,
      }
      setOptionalProp(controlDescription, 'label', label)
      setOptionalProp(controlDescription, 'folder', folder)
      setOptionalProp(controlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(controlDescription, 'required', required)
      setOptionalProp(controlDescription, 'defaultValue', defaultValue)

      return controlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('matrix4'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseMatrix4, 'defaultValue')(value),
  )
}

export function parseJSXControlDescription(value: unknown): ParseResult<JSXControlDescription> {
  return applicative7Either(
    (label, folder, control, visibleByDefault, preferredContents, required, defaultValue) => {
      let jsxControlDescription: JSXControlDescription = {
        control: control,
      }
      setOptionalProp(jsxControlDescription, 'label', label)
      setOptionalProp(jsxControlDescription, 'folder', folder)
      setOptionalProp(jsxControlDescription, 'visibleByDefault', visibleByDefault)
      setOptionalProp(jsxControlDescription, 'preferredContents', preferredContents)
      setOptionalProp(jsxControlDescription, 'required', required)
      setOptionalProp(jsxControlDescription, 'defaultValue', defaultValue)

      return jsxControlDescription
    },
    optionalObjectKeyParser(parseString, 'label')(value),
    optionalObjectKeyParser(parseString, 'folder')(value),
    objectKeyParser(parseConstant('jsx'), 'control')(value),
    optionalObjectKeyParser(parseBoolean, 'visibleByDefault')(value),
    optionalObjectKeyParser(
      parseAlternative<PreferredContents | PreferredContents[]>(
        [parsePreferredContents, parseArray(parsePreferredContents)],
        'Invalid preferredContents value',
      ),
      'preferredContents',
    )(value),
    requiredFieldParser(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
  )
}

function parseRegularControlDescription(value: unknown): ParseResult<RegularControlDescription> {
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
      case 'html-input':
        return parseHtmlInputControlDescription(value)
      case 'style-controls':
        return parseStyleControlsControlDescription(value)
      case 'tuple':
        return parseTupleControlDescription(value)
      case 'vector2':
        return parseVector2ControlDescription(value)
      case 'vector3':
        return parseVector3ControlDescription(value)
      case 'vector4':
        return parseVector4ControlDescription(value)
      case 'union':
        return parseUnionControlDescription(value)
      case 'jsx':
        return parseJSXControlDescription(value)
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

export function parseControlDescription(value: unknown): ParseResult<ControlDescription> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    return parseRegularControlDescription(value)
  } else {
    return left(descriptionParseError('Not an object.'))
  }
}

export type ParsedPropertyControls = { [prop: string]: ParseResult<ControlDescription> }
export type ParsedPropertyControlsForFile = {
  [componentName: string]: ParseResult<ParsedPropertyControls>
}

export function parsePropertyControls(value: unknown): ParseResult<ParsedPropertyControls> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    return right(objectMap((v) => parseControlDescription(v), value as any))
  } else {
    return left(descriptionParseError('Not an object.'))
  }
}
