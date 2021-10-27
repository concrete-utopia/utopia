import {
  NumberControlDescription,
  EnumControlDescription,
  BooleanControlDescription,
  StringControlDescription,
  PopUpListControlDescription,
  OptionsControlDescription,
  ColorControlDescription,
  IgnoreControlDescription,
  ImageControlDescription,
  StyleObjectControlDescription,
  FolderControlDescription,
  RawJSControlDescription,
} from 'utopia-api'
import {
  parseNumberControlDescription,
  parseEnumControlDescription,
  parseBooleanControlDescription,
  parseStringControlDescription,
  parsePopUpListControlDescription,
  parseOptionsControlDescription,
  parseColorControlDescription,
  ParsedPropertyControls,
  parsePropertyControls,
  parseIgnoreControlDescription,
  parseImageControlDescription,
  parseStyleObjectControlDescription,
  parseFolderControlDescription,
  parseControlDescription,
  parseRawJSControlDescription,
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
  it('fails on an invalid label', () => {
    const value = {
      ...validObject,
      label: true,
    }
    expect(parseFn(value)).toEqual(
      left(objectFieldParseError('label', descriptionParseError('Not a string.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validObject,
      control: 'ham sandwich',
    }
    expect(parseFn(value)).toEqual(
      left(objectFieldParseError('control', descriptionParseError('Not a member of an enum.'))),
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
  label: 'Boolean Control',
  control: 'boolean',
  defaultValue: true,
  disabledTitle: 'Not set.',
  enabledTitle: 'Value is set',
}

describe('parseBooleanControlDescription', () => {
  runBaseTestSuite(
    validBooleanControlDescriptionValue,
    ['control'],
    ['hat'],
    true,
    parseBooleanControlDescription,
  )
})

const validColorControlDescriptionValue: ColorControlDescription = {
  label: 'Slider Control',
  control: 'color',
  defaultValue: '#FFFFFF',
}

describe('parseColorControlDescription', () => {
  runBaseTestSuite(
    validColorControlDescriptionValue,
    ['control'],
    ['hat', 9],
    true,
    parseColorControlDescription,
  )
})

const validRawJSControlDescriptionValue: RawJSControlDescription = {
  label: 'Raw JS Control',
  control: 'rawjs',
}

describe('parseRawJSControlDescription', () => {
  runBaseTestSuite(
    validRawJSControlDescriptionValue,
    ['control'],
    [],
    false,
    parseRawJSControlDescription,
  )
})

const validEnumControlDescriptionValue: EnumControlDescription = {
  label: 'Enum Control',
  control: 'enum',
  defaultValue: 5,
  options: ['hat', 5, true, undefined, null],
  optionTitles: ['first title', 'second title'],
  displaySegmentedControl: true,
}

describe('parseEnumControlDescription', () => {
  runBaseTestSuite(
    validEnumControlDescriptionValue,
    ['control', 'options'],
    [['hat']],
    true,
    parseEnumControlDescription,
  )
})

const validIgnoreControlDescriptionValue: IgnoreControlDescription = {
  label: 'Ignore Description',
  control: 'ignore',
}

describe('parseIgnoreControlDescription', () => {
  runBaseTestSuite(
    validIgnoreControlDescriptionValue,
    ['control'],
    [],
    false,
    parseIgnoreControlDescription,
  )
})

const validImageControlDescriptionValue: ImageControlDescription = {
  label: 'Image Control',
  control: 'image',
  defaultValue: 'www.somewebsite.com/iamanimage.jpg',
}

describe('parseImageControlDescription', () => {
  runBaseTestSuite(
    validImageControlDescriptionValue,
    ['control'],
    [0],
    true,
    parseImageControlDescription,
  )
})

const validNumberControlDescriptionValue: NumberControlDescription = {
  label: 'Number Title',
  control: 'number',
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
    ['control'],
    ['hat'],
    true,
    parseNumberControlDescription,
  )
})

const validOptionsControlDescriptionValue: OptionsControlDescription = {
  label: 'Pop Up List Control',
  control: 'options',
  defaultValue: 5,
  options: [
    { value: 5, label: 'Five' },
    { value: 8, label: 'Eight' },
  ],
}

describe('parseOptionsControlDescription', () => {
  runBaseTestSuite(
    validOptionsControlDescriptionValue,
    ['control', 'options'],
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
          arrayIndexParseError(0, descriptionParseError('Not an object.')),
        ),
      ),
    )
  })
})

const validPopUpListControlDescriptionValue: PopUpListControlDescription = {
  label: 'Pop Up List Control',
  control: 'popuplist',
  defaultValue: 5,
  options: [
    { value: 5, label: 'Five' },
    { value: 8, label: 'Eight' },
  ],
}

describe('parsePopUpListControlDescription', () => {
  runBaseTestSuite(
    validPopUpListControlDescriptionValue,
    ['control', 'options'],
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
          arrayIndexParseError(0, descriptionParseError('Not an object.')),
        ),
      ),
    )
  })
})

const validStringControlDescriptionValue: StringControlDescription = {
  label: 'String Control',
  control: 'string',
  defaultValue: 'Some text',
  placeholder: 'Enter text',
  obscured: true,
}

describe('parseStringControlDescription', () => {
  runBaseTestSuite(
    validStringControlDescriptionValue,
    ['control'],
    [9],
    true,
    parseStringControlDescription,
  )
})

const validStyleObjectControlDescriptionValue: StyleObjectControlDescription = {
  label: 'Style Object Control',
  control: 'styleobject',
  defaultValue: { width: 100 },
  placeholder: { height: 100 },
}

describe('parseStyleObjectControlDescription', () => {
  runBaseTestSuite(
    validStyleObjectControlDescriptionValue,
    ['control'],
    ['hat', 9],
    true,
    parseStyleObjectControlDescription,
  )
})

const validFolderControlDescriptionValue: FolderControlDescription = {
  control: 'folder',
  controls: {
    style: validStyleObjectControlDescriptionValue,
    someSlider: validNumberControlDescriptionValue,
  },
}

describe('parseControlDescription', () => {
  it('parses a number description correctly', () => {
    expect(
      parseControlDescription(
        validNumberControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validNumberControlDescriptionValue))
  })
  it('parses an enum description correctly', () => {
    expect(
      parseControlDescription(
        validEnumControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validEnumControlDescriptionValue))
  })
  it('parses a boolean description correctly', () => {
    expect(
      parseControlDescription(
        validBooleanControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validBooleanControlDescriptionValue))
  })
  it('parses a string description correctly', () => {
    expect(
      parseControlDescription(
        validStringControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validStringControlDescriptionValue))
  })
  it('parses a popup list description correctly', () => {
    expect(
      parseControlDescription(
        validPopUpListControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validPopUpListControlDescriptionValue))
  })
  it('parses a options list description correctly', () => {
    expect(
      parseControlDescription(
        validOptionsControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validOptionsControlDescriptionValue))
  })
  it('parses a color description correctly', () => {
    expect(
      parseControlDescription(
        validColorControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validColorControlDescriptionValue))
  })
  it('parses a raw js control description correctly', () => {
    expect(
      parseControlDescription(
        validRawJSControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validRawJSControlDescriptionValue))
  })
  it('parses an ignore description correctly', () => {
    expect(
      parseControlDescription(
        validIgnoreControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validIgnoreControlDescriptionValue))
  })
  it('parses a folder instance description correctly', () => {
    expect(
      parseControlDescription(
        validFolderControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validFolderControlDescriptionValue))
  })
  it('fails on a value that is not an object', () => {
    expect(parseControlDescription('hat', 'testPropName', 'includeSpecialProps')).toEqual(
      left(descriptionParseError('Not an object.')),
    )
  })
  it('fails on a value that is an invalid case of one of the descriptions', () => {
    const value = {
      ...validOptionsControlDescriptionValue,
      label: true,
    }
    expect(parseControlDescription(value, 'testPropName', 'includeSpecialProps')).toEqual(
      left(objectFieldParseError('label', descriptionParseError('Not a string.'))),
    )
  })
})

describe('parsePropertyControls', () => {
  it('returns the property controls fully parsed when they are all valid', () => {
    const propertyControlsValue = {
      width: validNumberControlDescriptionValue,
      height: validNumberControlDescriptionValue,
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberControlDescriptionValue),
      height: right(validNumberControlDescriptionValue),
    })
    expect(parsePropertyControls(propertyControlsValue, 'includeSpecialProps')).toEqual(
      expectedResult,
    )
  })
  it('returns the property controls fully parsed when some are invalid', () => {
    const propertyControlsValue = {
      width: validNumberControlDescriptionValue,
      height: {
        ...validNumberControlDescriptionValue,
        defaultValue: 'hat',
      },
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberControlDescriptionValue),
      height: left(objectFieldParseError('defaultValue', descriptionParseError('Not a number.'))),
    })
    expect(parsePropertyControls(propertyControlsValue, 'includeSpecialProps')).toEqual(
      expectedResult,
    )
  })
  it('gives an error if the entire value is invalid', () => {
    const propertyControlsValue = 5
    const expectedResult: ParseResult<ParsedPropertyControls> = left(
      descriptionParseError('Not an object.'),
    )
    expect(parsePropertyControls(propertyControlsValue, 'includeSpecialProps')).toEqual(
      expectedResult,
    )
  })
})
