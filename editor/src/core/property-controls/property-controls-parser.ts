import {
  BooleanControlDescription,
  ColorControlDescription,
  ComponentInstanceDescription,
  ControlDescription,
  EnumControlDescription,
  NumberControlDescription,
  OptionsControlDescription,
  PopUpListControlDescription,
  SliderControlDescription,
  StringControlDescription,
  IgnoreControlDescription,
  UnionControlDescription,
  ImageControlDescription,
  EventHandlerControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
  StyleObjectControlDescription,
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
  parseEnum,
  parseFunction,
  parseNull,
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
} from '../shared/either'
import { objectMap, setOptionalProp, forEachValue } from '../shared/object-utils'
import { parseEnumValue } from './property-control-values'

export function parseNumberControlDescription(
  value: unknown,
): ParseResult<NumberControlDescription> {
  return applicative8Either(
    (title, type, defaultValue, max, min, unit, step, displayStepper) => {
      let numberControlDescription: NumberControlDescription = {
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
    objectKeyParser(parseEnum(['number']), 'type')(value),
    optionalObjectKeyParser(parseNumber, 'defaultValue')(value),
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
  'Value is not an array of strings or a function.',
)

export function parseEnumControlDescription(value: unknown): ParseResult<EnumControlDescription> {
  return applicative6Either(
    (title, type, defaultValue, options, optionTitles, displaySegmentedControl) => {
      let enumControlDescription: EnumControlDescription = {
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
    objectKeyParser(parseEnum(['enum']), 'type')(value),
    optionalObjectKeyParser(parseEnumValue, 'defaultValue')(value),
    objectKeyParser(parseArray(parseEnumValue), 'options')(value),
    optionalObjectKeyParser(parseOptionTitles, 'optionTitles')(value),
    optionalObjectKeyParser(parseBoolean, 'displaySegmentedControl')(value),
  )
}

export function parseEventHandlerControlDescription(
  value: unknown,
): ParseResult<EventHandlerControlDescription> {
  return applicative2Either(
    (title, type) => {
      let eventHandlerControlDescription: EventHandlerControlDescription = {
        type: type,
      }
      setOptionalProp(eventHandlerControlDescription, 'title', title)

      return eventHandlerControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['eventhandler']), 'type')(value),
  )
}

export function parseBooleanControlDescription(
  value: unknown,
): ParseResult<BooleanControlDescription> {
  return applicative5Either(
    (title, type, defaultValue, disabledTitle, enabledTitle) => {
      let booleanControlDescription: BooleanControlDescription = {
        type: type,
      }
      setOptionalProp(booleanControlDescription, 'title', title)
      setOptionalProp(booleanControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(booleanControlDescription, 'disabledTitle', disabledTitle)
      setOptionalProp(booleanControlDescription, 'enabledTitle', enabledTitle)

      return booleanControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['boolean']), 'type')(value),
    optionalObjectKeyParser(parseBoolean, 'defaultValue')(value),
    optionalObjectKeyParser(parseString, 'disabledTitle')(value),
    optionalObjectKeyParser(parseString, 'enabledTitle')(value),
  )
}

export function parseStringControlDescription(
  value: unknown,
): ParseResult<StringControlDescription> {
  return applicative5Either(
    (title, type, defaultValue, placeholder, obscured) => {
      let stringControlDescription: StringControlDescription = {
        type: type,
      }
      setOptionalProp(stringControlDescription, 'title', title)
      setOptionalProp(stringControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(stringControlDescription, 'placeholder', placeholder)
      setOptionalProp(stringControlDescription, 'obscured', obscured)

      return stringControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['string']), 'type')(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
    optionalObjectKeyParser(parseString, 'placeholder')(value),
    optionalObjectKeyParser(parseBoolean, 'obscured')(value),
  )
}

export function parseSliderControlDescription(
  value: unknown,
): ParseResult<SliderControlDescription> {
  return applicative6Either(
    (title, type, defaultValue, min, max, step) => {
      let sliderControlDescription: SliderControlDescription = {
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
    objectKeyParser(parseEnum(['slider']), 'type')(value),
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
): ParseResult<PopUpListControlDescription> {
  return applicative4Either(
    (title, type, defaultValue, options) => {
      let popUpListControlDescription: PopUpListControlDescription = {
        type: type,
        options: options,
      }
      setOptionalProp(popUpListControlDescription, 'title', title)
      setOptionalProp(popUpListControlDescription, 'defaultValue', defaultValue)

      return popUpListControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['popuplist']), 'type')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseArray(parsePropertyOption), 'options')(value),
  )
}

export function parseOptionsControlDescription(
  value: unknown,
): ParseResult<OptionsControlDescription> {
  return applicative4Either(
    (title, type, defaultValue, options) => {
      let optionsControlDescription: OptionsControlDescription = {
        type: type,
        options: options,
      }
      setOptionalProp(optionsControlDescription, 'title', title)
      setOptionalProp(optionsControlDescription, 'defaultValue', defaultValue)

      return optionsControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['options']), 'type')(value),
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
    (title, type, defaultValue) => {
      let colorControlDescription: ColorControlDescription = {
        type: type,
      }
      setOptionalProp(colorControlDescription, 'title', title)
      setOptionalProp(colorControlDescription, 'defaultValue', defaultValue)

      return colorControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['color']), 'type')(value),
    optionalObjectKeyParser(parseStringValidateAsColor, 'defaultValue')(value),
  )
}

export function parseComponentInstanceControlDescription(
  value: unknown,
): ParseResult<ComponentInstanceDescription> {
  return applicative2Either(
    (title, type) => {
      let componentInstanceDescription: ComponentInstanceDescription = {
        type: type,
      }
      setOptionalProp(componentInstanceDescription, 'title', title)

      return componentInstanceDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['componentinstance']), 'type')(value),
  )
}

export function parseIgnoreControlDescription(
  value: unknown,
): ParseResult<IgnoreControlDescription> {
  return applicative2Either(
    (title, type) => {
      let ignoreControlDescription: IgnoreControlDescription = {
        type: type,
      }
      setOptionalProp(ignoreControlDescription, 'title', title)
      return ignoreControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['ignore']), 'type')(value),
  )
}

export function parseImageControlDescription(value: unknown): ParseResult<ImageControlDescription> {
  return applicative3Either(
    (title, type, defaultValue) => {
      let imageControlDescription: ImageControlDescription = {
        type: type,
      }
      setOptionalProp(imageControlDescription, 'title', title)
      setOptionalProp(imageControlDescription, 'defaultValue', defaultValue)

      return imageControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['image']), 'type')(value),
    optionalObjectKeyParser(parseString, 'defaultValue')(value),
  )
}

export function parseStyleObjectControlDescription(
  value: unknown,
): ParseResult<StyleObjectControlDescription> {
  return applicative4Either(
    (title, type, defaultValue, placeholder) => {
      let styleObjectControlDescription: StyleObjectControlDescription = {
        type: type,
      }
      setOptionalProp(styleObjectControlDescription, 'title', title)
      setOptionalProp(styleObjectControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(styleObjectControlDescription, 'placeholder', placeholder)

      return styleObjectControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['styleobject']), 'type')(value),
    optionalObjectKeyParser(parseObject(parseAny), 'defaultValue')(value), // FIXME
    optionalObjectKeyParser(parseObject(parseAny), 'placeholder')(value), // FIXME
  )
}

export function parseArrayControlDescription(value: unknown): ParseResult<ArrayControlDescription> {
  return applicative5Either(
    (title, type, defaultValue, propertyControl, maxCount) => {
      let arrayControlDescription: ArrayControlDescription = {
        type: type,
        propertyControl: propertyControl,
      }
      setOptionalProp(arrayControlDescription, 'title', title)
      setOptionalProp(arrayControlDescription, 'defaultValue', defaultValue)
      setOptionalProp(arrayControlDescription, 'maxCount', maxCount)

      return arrayControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['array']), 'type')(value),
    optionalObjectKeyParser(parseArray(parseAny), 'defaultValue')(value),
    objectKeyParser(parseControlDescription, 'propertyControl')(value),
    optionalObjectKeyParser(parseNumber, 'maxCount')(value),
  )
}

export function parseObjectControlDescription(
  value: unknown,
): ParseResult<ObjectControlDescription> {
  return applicative4Either(
    (title, type, defaultValue, object) => {
      let objectControlDescription: ObjectControlDescription = {
        type: type,
        object: object,
      }
      setOptionalProp(objectControlDescription, 'title', title)
      setOptionalProp(objectControlDescription, 'defaultValue', defaultValue)

      return objectControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['object']), 'type')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseObject(parseControlDescription), 'object')(value),
  )
}

export function parseUnionControlDescription(value: unknown): ParseResult<UnionControlDescription> {
  return applicative4Either(
    (title, type, defaultValue, controls) => {
      let unionControlDescription: UnionControlDescription = {
        type: type,
        controls: controls,
      }
      setOptionalProp(unionControlDescription, 'title', title)
      setOptionalProp(unionControlDescription, 'defaultValue', defaultValue)
      return unionControlDescription
    },
    optionalObjectKeyParser(parseString, 'title')(value),
    objectKeyParser(parseEnum(['union']), 'type')(value),
    optionalObjectKeyParser(parseAny, 'defaultValue')(value),
    objectKeyParser(parseArray(parseControlDescription), 'controls')(value),
  )
}

export function parseControlDescription(value: unknown): ParseResult<ControlDescription> {
  if (typeof value === 'object' && !Array.isArray(value) && value != null) {
    switch ((value as any)['type']) {
      case 'boolean':
        return parseBooleanControlDescription(value)
      case 'color':
        return parseColorControlDescription(value)
      case 'componentinstance':
        return parseComponentInstanceControlDescription(value)
      case 'enum':
        return parseEnumControlDescription(value)
      case 'eventhandler':
        return parseEventHandlerControlDescription(value)
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
      case 'slider':
        return parseSliderControlDescription(value)
      case 'string':
        return parseStringControlDescription(value)
      case 'styleobject':
        return parseStyleObjectControlDescription(value)
      case 'array':
        return parseArrayControlDescription(value)
      case 'object':
        return parseObjectControlDescription(value)
      case 'union':
        return parseUnionControlDescription(value)
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

export type ParsedPropertyControls = { [prop: string]: ParseResult<ControlDescription> }
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
