import {
  BooleanControlDescription,
  ColorControlDescription,
  ComponentInstanceDescription,
  ControlDescription,
  ControlType,
  EnumControlDescription,
  NumberControlDescription,
  OptionsControlDescription,
  PopUpListControlDescription,
  SliderControlDescription,
  StringControlDescription,
  IgnoreControlDescription,
} from 'utopia-api'
import { parseCSSColor } from '../../components/inspector/common/css-utils'
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
  parseEnum,
  parseFunction,
  parseNull,
  parseNumber,
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
} from '../shared/either'
import { objectMap, setOptionalProp, forEachValue } from '../shared/object-utils'

export function parseNumberControlDescription(
  value: unknown,
): ParseResult<NumberControlDescription<any>> {
  return applicative8Either(
    (title, type, defaultValue, max, min, unit, step, displayStepper) => {
      let numberControlDescription: NumberControlDescription<any> = {
        type: type,
      }
      setOptionalProp(numberControlDescription, 'title', title)
      setOptionalProp(numberControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(numberControlDescription, 'max', max)
      setOptionalProp(numberControlDescription, 'min', min)
      setOptionalProp(numberControlDescription, 'unit', unit)
      setOptionalProp(numberControlDescription, 'step', step)
      setOptionalProp(numberControlDescription, 'displayStepper', displayStepper)

      return numberControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.Number]), 'type')(value),
    optionalObjectKeyParser(parseNumber, 'defaultValue')(value),
    optionalObjectKeyParser(parseNumber, 'max')(value),
    optionalObjectKeyParser(parseNumber, 'min')(value),
    optionalObjectKeyParser(parseString, 'unit')(value),
    optionalObjectKeyParser(parseNumber, 'step')(value),
    optionalObjectKeyParser(parseBoolean, 'displayStepper')(value),
  )
}

type StringBooleanNumberUndefinedNull = string | boolean | number | undefined | null

const parseStringBooleanNumberUndefinedNull: Parser<StringBooleanNumberUndefinedNull> = parseAlternative<
  StringBooleanNumberUndefinedNull
>(
  [parseString, parseBoolean, parseNumber, parseUndefined, parseNull],
  'Value is not a string/boolean/number/undefined/null.',
)

type OptionTitles<P> = Array<string> | ((props: P | null) => Array<string>)

const parseOptionTitles: Parser<OptionTitles<any>> = parseAlternative<OptionTitles<any>>(
  [parseArray(parseString), parseFunction],
  'Value is not an array of strings or a function.',
)

export function parseEnumControlDescription(
  value: unknown,
): ParseResult<EnumControlDescription<any>> {
  return applicative6Either(
    (title, type, defaultValue, options, optionTitles, displaySegmentedControl) => {
      let enumControlDescription: EnumControlDescription<any> = {
        type: type,
        options: options,
      }
      setOptionalProp(enumControlDescription, 'title', title)
      setOptionalProp(enumControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(enumControlDescription, 'optionTitles', optionTitles)
      setOptionalProp(enumControlDescription, 'displaySegmentedControl', displaySegmentedControl)

      return enumControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.Enum]), 'type')(value),
    optionalObjectKeyParser(parseStringBooleanNumberUndefinedNull, 'defaultValue')(value),
    objectKeyParser(parseArray(parseStringBooleanNumberUndefinedNull), 'options')(value),
    optionalObjectKeyParser(parseOptionTitles, 'optionTitles')(value),
    optionalObjectKeyParser(parseBoolean, 'displaySegmentedControl')(value),
  )
}

export function parseBooleanControlDescription(
  value: unknown,
): ParseResult<BooleanControlDescription<any>> {
  return applicative5Either(
    (title, type, defaultValue, disabledTitle, enabledTitle) => {
      let booleanControlDescription: BooleanControlDescription<any> = {
        type: type,
      }
      setOptionalProp(booleanControlDescription, 'title', title)
      setOptionalProp(booleanControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(booleanControlDescription, 'disabledTitle', disabledTitle)
      setOptionalProp(booleanControlDescription, 'enabledTitle', enabledTitle)

      return booleanControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.Boolean]), 'type')(value),
    optionalObjectKeyParser(parseBoolean, 'defaultValue')(value),
    optionalObjectKeyParser(parseString, 'disabledTitle')(value),
    optionalObjectKeyParser(parseString, 'enabledTitle')(value),
  )
}

export function parseStringControlDescription(
  value: unknown,
): ParseResult<StringControlDescription<any>> {
  return applicative5Either(
    (title, type, defaultValue, placeholder, obscured) => {
      let stringControlDescription: StringControlDescription<any> = {
        type: type,
      }
      setOptionalProp(stringControlDescription, 'title', title)
      setOptionalProp(stringControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(stringControlDescription, 'placeholder', placeholder)
      setOptionalProp(stringControlDescription, 'obscured', obscured)

      return stringControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.String]), 'type')(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
    optionalObjectKeyParser(parseString, 'placeholder')(value),
    optionalObjectKeyParser(parseBoolean, 'obscured')(value),
  )
}

export function parseSliderControlDescription(
  value: unknown,
): ParseResult<SliderControlDescription<any>> {
  return applicative6Either(
    (title, type, defaultValue, min, max, step) => {
      let sliderControlDescription: SliderControlDescription<any> = {
        type: type,
        min: min,
        max: max,
        step: step,
      }
      setOptionalProp(sliderControlDescription, 'title', title)
      setOptionalProp(sliderControlDescription, 'defaultValue', defaultValue)

      return sliderControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.Slider]), 'type')(value),
    optionalObjectKeyParser(parseNumber, 'defaultValue')(value),
    objectKeyParser(parseNumber, 'min')(value),
    objectKeyParser(parseNumber, 'max')(value),
    objectKeyParser(parseNumber, 'step')(value),
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
): ParseResult<PopUpListControlDescription<any>> {
  return applicative4Either(
    (title, type, defaultValue, options) => {
      let popUpListControlDescription: PopUpListControlDescription<any> = {
        type: type,
        options: options,
      }
      setOptionalProp(popUpListControlDescription, 'title', title)
      setOptionalProp(popUpListControlDescription, 'defaultValue', defaultValue)

      return popUpListControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.PopUpList]), 'type')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseArray(parsePropertyOption), 'options')(value),
  )
}

export function parseOptionsControlDescription(
  value: unknown,
): ParseResult<OptionsControlDescription<any>> {
  return applicative4Either(
    (title, type, defaultValue, options) => {
      let optionsControlDescription: OptionsControlDescription<any> = {
        type: type,
        options: options,
      }
      setOptionalProp(optionsControlDescription, 'title', title)
      setOptionalProp(optionsControlDescription, 'defaultValue', defaultValue)

      return optionsControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.Options]), 'type')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseArray(parsePropertyOption), 'options')(value),
  )
}

const invalidColorStringResult: ParseResult<string> = left(
  descriptionParseError('Value is not a valid color string.'),
)

// We want to parse the string, but check that it can be parsed as a color.
// Returning the string, only if it does validate against `parseColor`.
function parseStringValidateAsColor(value: unknown): ParseResult<string> {
  const parsed = parseString(value)
  return foldEither(
    (_) => {
      return invalidColorStringResult
    },
    (text) => {
      const parsedColor = parseCSSColor(text)
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

export function parseColorControlDescription(
  value: unknown,
): ParseResult<ColorControlDescription<any>> {
  return applicative3Either(
    (title, type, defaultValue) => {
      let colorControlDescription: ColorControlDescription<any> = {
        type: type,
      }
      setOptionalProp(colorControlDescription, 'title', title)
      setOptionalProp(colorControlDescription, 'defaultValue', defaultValue)

      return colorControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.Color]), 'type')(value),
    optionalObjectKeyParser(parseStringValidateAsColor, 'defaultValue')(value),
  )
}

export function parseComponentInstanceControlDescription(
  value: unknown,
): ParseResult<ComponentInstanceDescription<any>> {
  return applicative2Either(
    (title, type) => {
      let componentInstanceDescription: ComponentInstanceDescription<any> = {
        type: type,
      }
      setOptionalProp(componentInstanceDescription, 'title', title)

      return componentInstanceDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.ComponentInstance]), 'type')(value),
  )
}

export function parseIgnoreControlDescription(
  value: unknown,
): ParseResult<IgnoreControlDescription<any>> {
  return applicative2Either(
    (title, type) => {
      let ignoreControlDescription: IgnoreControlDescription<any> = {
        type: type,
      }
      setOptionalProp(ignoreControlDescription, 'title', title)
      return ignoreControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum([ControlType.Ignore]), 'type')(value),
  )
}

export function parseControlDescription(value: unknown): ParseResult<ControlDescription<any>> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    switch ((value as any)['type']) {
      case 'number':
        return parseNumberControlDescription(value)
      case 'enum':
        return parseEnumControlDescription(value)
      case 'boolean':
        return parseBooleanControlDescription(value)
      case 'string':
        return parseStringControlDescription(value)
      case 'slider':
        return parseSliderControlDescription(value)
      case 'popuplist':
        return parsePopUpListControlDescription(value)
      case 'options':
        return parseOptionsControlDescription(value)
      case 'color':
        return parseColorControlDescription(value)
      case 'componentinstance':
        return parseComponentInstanceControlDescription(value)
      case 'ignore':
        return parseIgnoreControlDescription(value)
      case undefined:
        return left(objectFieldNotPresentParseError('type'))
      default:
        return left(
          objectFieldParseError('type', descriptionParseError('Unexpected property control type.')),
        )
    }
  } else {
    return left(descriptionParseError('Value is not an object.'))
  }
}

export type ParsedPropertyControls = { [prop: string]: ParseResult<ControlDescription<any>> }
export type ParsedPropertyControlsForFile = {
  [componentName: string]: ParseResult<ParsedPropertyControls>
}

export function parsePropertyControls(value: unknown): ParseResult<ParsedPropertyControls> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    return right(objectMap(parseControlDescription, value as any))
  } else {
    return left(descriptionParseError('Property controls are not an object.'))
  }
}

export function parsePropertyControlsForFile(allControls: {
  [componentName: string]: unknown
}): ParsedPropertyControlsForFile {
  return objectMap(parsePropertyControls, allControls)
}
