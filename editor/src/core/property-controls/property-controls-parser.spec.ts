import {
  NumberControlDescription,
  EnumControlDescription,
  BooleanControlDescription,
  StringControlDescription,
  SliderControlDescription,
  PopUpListControlDescription,
  OptionsControlDescription,
  ColorControlDescription,
  ComponentInstanceDescription,
  IgnoreControlDescription,
  EventHandlerControlDescription,
  ImageControlDescription,
  StyleObjectControlDescription,
} from 'utopia-api'
import {
  parseNumberControlDescription,
  parseEnumControlDescription,
  parseBooleanControlDescription,
  parseStringControlDescription,
  parseSliderControlDescription,
  parsePopUpListControlDescription,
  parseOptionsControlDescription,
  parseColorControlDescription,
  parseComponentInstanceControlDescription,
  parseControlDescription,
  ParsedPropertyControls,
  parsePropertyControls,
  parseIgnoreControlDescription,
  parseEventHandlerControlDescription,
  parseImageControlDescription,
  parseStyleObjectControlDescription,
} from './property-controls-parser'
import { right, left, isLeft } from '../shared/either'
import {
  objectFieldParseError,
  descriptionParseError,
  arrayIndexParseError,
  ParseResult,
  ParseError,
} from '../../utils/value-parser-utils'
import { pick } from '../shared/object-utils'
import { fastForEach } from '../shared/utils'

function runBaseTestSuite<T>(
  validObject: T,
  requiredFields: Array<keyof T>,
  invalidDefaults: unknown[],
  defaultAllowed: boolean,
  parseFn: (value: unknown) => ParseResult<T>,
) {
  it('parses a full value correctly', () => {
    expect(parseFn(validObject)).toEqual(right(validObject))
  })
  it('parses a minimal value correctly', () => {
    const value = pick(requiredFields, validObject)
    expect(parseFn(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validObject,
      title: true,
    }
    expect(parseFn(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validObject,
      type: 'ham sandwich',
    }
    expect(parseFn(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })

  if (defaultAllowed) {
    it('fails on an invalid default', () => {
      fastForEach(invalidDefaults, (invalidDefault) => {
        const value = {
          ...validObject,
          defaultValue: invalidDefault,
        }
        expect(isLeft(parseFn(value))).toBeTruthy()
      })
    })
  } else {
    it('ignores a default value', () => {
      const value = {
        ...validObject,
        defaultValue: 'anything really',
      }

      expect(parseFn(value)).toEqual(right(validObject))
    })
  }
}

const validBooleanControlDescriptionValue: BooleanControlDescription = {
  title: 'Boolean Control',
  type: 'boolean',
  defaultValue: true,
  disabledTitle: 'Value is not set.',
  enabledTitle: 'Value is set',
}

describe('parseBooleanControlDescription', () => {
  runBaseTestSuite(
    validBooleanControlDescriptionValue,
    ['type'],
    ['hat'],
    true,
    parseBooleanControlDescription,
  )
})

const validColorControlDescriptionValue: ColorControlDescription = {
  title: 'Slider Control',
  type: 'color',
  defaultValue: '#FFFFFF',
}

describe('parseColorControlDescription', () => {
  runBaseTestSuite(
    validColorControlDescriptionValue,
    ['type'],
    ['hat', 9],
    true,
    parseColorControlDescription,
  )
})

const validComponentInstanceControlDescriptionValue: ComponentInstanceDescription = {
  title: 'Component Instance Control',
  type: 'componentinstance',
}

describe('parseComponentInstanceControlDescription', () => {
  runBaseTestSuite(
    validComponentInstanceControlDescriptionValue,
    ['type'],
    [],
    false,
    parseComponentInstanceControlDescription,
  )
})

const validEnumControlDescriptionValue: EnumControlDescription = {
  title: 'Enum Control',
  type: 'enum',
  defaultValue: 5,
  options: ['hat', 5, true, undefined, null],
  optionTitles: ['first title', 'second title'],
  displaySegmentedControl: true,
}

describe('parseEnumControlDescription', () => {
  runBaseTestSuite(
    validEnumControlDescriptionValue,
    ['type', 'options'],
    [['hat']],
    true,
    parseEnumControlDescription,
  )
})

const validEventHandlerControlDescriptionValue: EventHandlerControlDescription = {
  title: 'Event Handler Control',
  type: 'eventhandler',
}

describe('parseEventHandlerControlDescription', () => {
  runBaseTestSuite(
    validEventHandlerControlDescriptionValue,
    ['type'],
    [],
    false,
    parseEventHandlerControlDescription,
  )
})

const validIgnoreControlDescriptionValue: IgnoreControlDescription = {
  title: 'Ignore Description',
  type: 'ignore',
}

describe('parseIgnoreControlDescription', () => {
  runBaseTestSuite(
    validIgnoreControlDescriptionValue,
    ['type'],
    [],
    false,
    parseIgnoreControlDescription,
  )
})

const validImageControlDescriptionValue: ImageControlDescription = {
  title: 'Image Control',
  type: 'image',
  defaultValue: 'www.somewebsite.com/iamanimage.jpg',
}

describe('parseImageControlDescription', () => {
  runBaseTestSuite(
    validImageControlDescriptionValue,
    ['type'],
    [0],
    true,
    parseImageControlDescription,
  )
})

const validNumberControlDescriptionValue: NumberControlDescription = {
  title: 'Number Title',
  type: 'number',
  defaultValue: 5,
  max: 10,
  min: 2,
  unit: 'Some Unit',
  step: 1,
  displayStepper: true,
}

describe('parseNumberControlDescription', () => {
  runBaseTestSuite(
    validNumberControlDescriptionValue,
    ['type'],
    ['hat'],
    true,
    parseNumberControlDescription,
  )
})

const validOptionsControlDescriptionValue: OptionsControlDescription = {
  title: 'Pop Up List Control',
  type: 'options',
  defaultValue: 5,
  options: [
    { value: 5, label: 'Five' },
    { value: 8, label: 'Eight' },
  ],
}

describe('parseOptionsControlDescription', () => {
  runBaseTestSuite(
    validOptionsControlDescriptionValue,
    ['type', 'options'],
    [],
    true,
    parseOptionsControlDescription,
  )

  it('fails on an invalid option', () => {
    const value = {
      ...validOptionsControlDescriptionValue,
      options: ['error'],
    }
    expect(parseOptionsControlDescription(value)).toEqual(
      left(
        objectFieldParseError(
          'options',
          arrayIndexParseError(0, descriptionParseError('Value is not an object.')),
        ),
      ),
    )
  })
})

const validPopUpListControlDescriptionValue: PopUpListControlDescription = {
  title: 'Pop Up List Control',
  type: 'popuplist',
  defaultValue: 5,
  options: [
    { value: 5, label: 'Five' },
    { value: 8, label: 'Eight' },
  ],
}

describe('parsePopUpListControlDescription', () => {
  runBaseTestSuite(
    validPopUpListControlDescriptionValue,
    ['type', 'options'],
    [],
    true,
    parsePopUpListControlDescription,
  )

  it('fails on an invalid option', () => {
    const value = {
      ...validPopUpListControlDescriptionValue,
      options: ['error'],
    }
    expect(parsePopUpListControlDescription(value)).toEqual(
      left(
        objectFieldParseError(
          'options',
          arrayIndexParseError(0, descriptionParseError('Value is not an object.')),
        ),
      ),
    )
  })
})

const validSliderControlDescriptionValue: SliderControlDescription = {
  title: 'Slider Control',
  type: 'slider',
  defaultValue: 5,
  min: 2,
  max: 10,
  step: 1,
}

describe('parseSliderControlDescription', () => {
  runBaseTestSuite(
    validSliderControlDescriptionValue,
    ['type', 'max', 'min', 'step'],
    ['hat'],
    true,
    parseSliderControlDescription,
  )
})

const validStringControlDescriptionValue: StringControlDescription = {
  title: 'String Control',
  type: 'string',
  defaultValue: 'Some text',
  placeholder: 'Enter text',
  obscured: true,
}

describe('parseStringControlDescription', () => {
  runBaseTestSuite(
    validStringControlDescriptionValue,
    ['type'],
    [9],
    true,
    parseStringControlDescription,
  )
})

const validStyleObjectControlDescriptionValue: StyleObjectControlDescription = {
  title: 'Style Object Control',
  type: 'styleobject',
  defaultValue: { width: 100 },
  placeholder: { height: 100 },
}

describe('parseStyleObjectControlDescription', () => {
  runBaseTestSuite(
    validStyleObjectControlDescriptionValue,
    ['type'],
    ['hat', 9],
    true,
    parseStyleObjectControlDescription,
  )
})

describe('parseControlDescription', () => {
  it('parses a number description correctly', () => {
    expect(parseControlDescription(validNumberControlDescriptionValue)).toEqual(
      right(validNumberControlDescriptionValue),
    )
  })
  it('parses an enum description correctly', () => {
    expect(parseControlDescription(validEnumControlDescriptionValue)).toEqual(
      right(validEnumControlDescriptionValue),
    )
  })
  it('parses a boolean description correctly', () => {
    expect(parseControlDescription(validBooleanControlDescriptionValue)).toEqual(
      right(validBooleanControlDescriptionValue),
    )
  })
  it('parses a string description correctly', () => {
    expect(parseControlDescription(validStringControlDescriptionValue)).toEqual(
      right(validStringControlDescriptionValue),
    )
  })
  it('parses a slider description correctly', () => {
    expect(parseControlDescription(validSliderControlDescriptionValue)).toEqual(
      right(validSliderControlDescriptionValue),
    )
  })
  it('parses a popup list description correctly', () => {
    expect(parseControlDescription(validPopUpListControlDescriptionValue)).toEqual(
      right(validPopUpListControlDescriptionValue),
    )
  })
  it('parses a options list description correctly', () => {
    expect(parseControlDescription(validOptionsControlDescriptionValue)).toEqual(
      right(validOptionsControlDescriptionValue),
    )
  })
  it('parses a color description correctly', () => {
    expect(parseControlDescription(validColorControlDescriptionValue)).toEqual(
      right(validColorControlDescriptionValue),
    )
  })
  it('parses a component instance description correctly', () => {
    expect(parseControlDescription(validComponentInstanceControlDescriptionValue)).toEqual(
      right(validComponentInstanceControlDescriptionValue),
    )
  })
  it('parses an ignore description correctly', () => {
    expect(parseControlDescription(validIgnoreControlDescriptionValue)).toEqual(
      right(validIgnoreControlDescriptionValue),
    )
  })
  it('fails on a value that is not an object', () => {
    expect(parseControlDescription('hat')).toEqual(
      left(descriptionParseError('Value is not an object.')),
    )
  })
  it('fails on a value that is an invalid case of one of the descriptions', () => {
    const value = {
      ...validOptionsControlDescriptionValue,
      title: true,
    }
    expect(parseControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
})

describe('parsePropertyControls', () => {
  it('returns the property controls fully parsed when they are all valid', () => {
    const propertyControlsValue = {
      width: validNumberControlDescriptionValue,
      height: validSliderControlDescriptionValue,
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberControlDescriptionValue),
      height: right(validSliderControlDescriptionValue),
    })
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
  it('returns the property controls fully parsed when some are invalid', () => {
    const propertyControlsValue = {
      width: validNumberControlDescriptionValue,
      height: {
        ...validSliderControlDescriptionValue,
        defaultValue: 'hat',
      },
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberControlDescriptionValue),
      height: left(
        objectFieldParseError('defaultValue', descriptionParseError('Value is not a number.')),
      ),
    })
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
  it('gives an error if the entire value is invalid', () => {
    const propertyControlsValue = 5
    const expectedResult: ParseResult<ParsedPropertyControls> = left(
      descriptionParseError('Property controls are not an object.'),
    )
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
})
